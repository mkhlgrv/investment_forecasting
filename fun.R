do.call.pipe <- function(args,what){
  do.call(what,args)
}
create.lagv <- function(df, nlag){
  if(!is.integer(nlag)|nlag == 0){
    stop("lag must be integer and greater than 0")
  }
  nvar <- ncol(df)
  for(j in 1:nvar){
    y <- df[,j]
    if(all(y==1|y==0)|!is.numeric(y)){
    }
    yname <- names(df)[j]
    for(i in 1:nlag){
      df <- merge.xts(df, new_lag=lag.xts(y, k = i))
      names(df)[length(names(df))] <- paste0(yname, "_lag",i)
    }
  }
  df
}

get.stationary.panel <- function(df, df_with_for, test.length){
  dates <- time(df)
  result <- df %>%
    as.xts %>%
    as.list %>%
    imap(function(x,i){
      df_fori <- df_with_for[,i]
      # проводим adf test для простой разности
      d0 <- adf.test(x) %>% .$p.value
      if(d0<=0.05&i != "FC"){
        type <-  "d0"
        x_stat <- x
        x_stat_for <- df_fori
      } else{
        d1 <- adf.test(diff.xts(x) %>% na.omit) %>% .$p.value
        if(d1<=0.05|i == "FC"){
          type <-  "d1"
          x_stat <- diff.xts(x)
          x_stat_for <- diff.xts(df_fori)
        } else
        {
          log_d1 <- 1
          try({
            log_d1 <- adf.test(diff.xts(log(x)) %>% na.omit) %>% .$p.value
          })
          if(log_d1<=0.05&any(x>0)&any(df_fori>0)){
            type <- "log_d1"
            x_stat <- diff.xts(log(x))
            x_stat_for <- diff.xts(log(df_fori))
          } else
          {
            d2 <- adf.test(diff.xts(x, differences = 2) %>% na.omit) %>% .$p.value
            if(d2<=0.1){
              type <- "d2"
              x_stat <- diff.xts(diff.xts(x))
              x_stat_for <- diff.xts(diff.xts(df_fori))
            } else{
              # сообщений нет
              message(i)
              message(min(c(d0, d1,d2)))# log_d1, d2)))
              type <- "d0"
              x_stat <- x
              x_stat_for <- df_fori
            }
          }
        }
        
      }
      x_stat_for <- x_stat_for[(nrow(x_stat_for) - test.length + 1):nrow(x_stat_for),]
      x_stat <- as.xts(x_stat)
      x_stat_for <-  as.xts(x_stat_for)
      names(x_stat_for) <- names(x_stat) <- i
      list(typedf = data.frame(tsname = i, type = type, stringsAsFactors = FALSE),
           statdf = x_stat,
           statdf_for = x_stat_for)
    })
  statdf <- do.call(merge.xts, result %>% map(function(x){
    x$statdf
  })) %>% na.omit
  statdf_for <- do.call(merge.xts, result %>% map(function(x){
    x$statdf_for
  })) %>% na.omit
  typedf <- result %>% map_dfr(function(x){
    x$typedf
  })
  list(train = statdf, test = statdf_for, type = typedf)
}


get.stl.forecast <- function(df, train, horizon){
  remain <- df[train,]
  forec <- df[1:horizon,]
  for(i in colnames(df)){
    dec <- stl(df[train, i], # model error -trend -seasonality with loess(LOcal regrEESion)
               s.window = 4,
               robust = T)
    # remain[,i] <- dec$time.series[,3] # remainder
    forec[,i] <- dec %>% forecast(h = horizon) %>% .$mean
  }
  forec
}


# Get panel regression result ----
get.panel <- function(df 
                      ,window, # initial window
                      horizon, # time slises horizon = 1
                      model, niter = NULL, df_true = df_long){
  if(!any(c("zoo", "xts") %in% class(df))){
    stop("df must be 'zoo' or 'xts'")
  }
  df %<>% na.omit()
  dates <- time(df)
  expand.grid(model = model, window = window) %>% 
    split(seq(nrow(.))) %>%
    map(function(x){
        
        TS <- createTimeSlices(dates, 
                               initialWindow = x$window,
                               horizon = horizon,
                               fixedWindow = TRUE)
        message(paste0(Sys.time(),":\n",
                       "start training ", x$model," model for window ",x$window, " and horizon ", horizon))
        map2(TS$train, TS$test, function(train_slice, test_slice){
          df.train <- df[train_slice, ]
          if(x$model != "VAR"){
            X.train <- model.matrix(FC~0+., data = df.train)
            y.train <- df.train$FC %>% as.numeric()
            df.test <- get.stl.forecast(df, train_slice, horizon)
            X.test <- model.matrix(FC~0+.,data = df.test)
          }
          
        ## Random forest ----  
          if(x$model == "rf"){
            tc <- trainControl(method="repeatedcv", number=5, repeats=5, search="grid")
            
            tunegrid <- expand.grid(.mtry=seq(1,20))
            
            rf.out <- train(x = X.train,
                            y = y.train,
                            method = "rf", 
                            metric = "RMSE",
                            trControl = tc,
                            tuneGrid = tunegrid)
            
            bestmtry <- rf.out$bestTune[1,1]
            message(bestmtry)

            m_rf <- randomForest(x = X.train, y = y.train, mtry = bestmtry)
            y.pred <- y.pred.ts <- predict(m_rf, newdata = X.test) %>%
                as.numeric
            
            
            
            y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
            y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
            y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
            y.pred <- exp(y.pred)
            tibble(model = x$model,
                   window = x$window,
                   horizon = c(1:horizon),
                   mtry = bestmtry,
                   date = dates[test_slice], # date of forecast
                   y.true = df_true$FC[test_slice] %>% as.numeric,
                   y.pred = y.pred,
                   y.true.ts = df$FC[test_slice] %>% as.numeric,
                   y.pred.ts = y.pred.ts)
          }
          
        ## Regularisation ------
          else if(grepl("lasso|elnet|ridge", x$model)){
            if(grepl("lasso", x$model)){
              bestal = 1
            } else if(grepl("ridge", x$model)){
              bestal = 0
            } else if(grepl("elnet", x$model)){
              bestal = 0.5
            }
            if(grepl("pc", x$model)){
                pclist <-  prcomp(X.train, scale. = TRUE)
                X.train <- pclist$x
                X.test <- predict(pclist, newdata = X.test)
              }
            if(grepl("adaptive", x$model)){
                train_ada <- as.data.frame(X.train) %>%
                  mutate(y = y.train)
                test_ada <-  as.data.frame(X.test) %>%
                  mutate(y = 0)
                m_ada <- glmnet(X.train, y.train, alpha = 0, lambda = 0)
                w3 <- 1/abs(as.numeric(coef(m_ada))
                            [1:(ncol(X.train))] )^0.5 ## Using gamma = 1
                w3[which(w3 == Inf)] <- 999999999 ## Replacing values estimated as Infinite for 999999999
              } else {
                w3 <- rep(1, ncol(X.train))
              }
              set.seed(2019)
              glm.out <- train(x=X.train,
                               y=y.train,
                               method = "glmnet",
                               metric = "RMSE",
                               penalty.factor = w3,
                               tuneGrid = expand.grid(.alpha = bestal,.lambda = seq(0.1,0.001,length = niter)))
              
              # assign("glm.out", glm.out, envir = globalenv())
              # stop()
              
              bestlam <- glm.out$bestTune$lambda
              m_glm <- glmnet(X.train, y.train, alpha = bestal, lambda = bestlam, penalty.factor = w3)
              nonzero <- predict(m_glm, type = "nonzero") %>% nrow
              if(is.null(nonzero)){
                nonzero <- 0
              } 
              if(!grepl("post", x$model)){
                y.pred <- predict(m_glm,newx = X.test)
                
                if(length(dim(y.pred)!=1)){
                  y.pred <- y.pred[,1]
                }
              } else if(grepl("post", x$model)){
                if(nonzero!=0){
                  nzvars <- predict(m_glm, type = "nonzero") %>% .[,1]
                  train_post <- as.data.frame(X.train) %>%
                    select(nzvars) %>%
                    mutate(y = y.train)
                  test_post <-  as.data.frame(X.test) %>%
                    select(nzvars) %>%
                    mutate(y = 0)
                  m_post <- lm(y~., data = train_post)
                  y.pred <- predict(m_post, newdata = test_post)
                } else{
                  message(paste0(x$model,
                                 ": none of nonzero coeffs for lambda = ",
                                 bestlam , 
                                 " and alpha = ", bestal))
                  y.pred <- predict(m_glm,newx = X.test)
                }
                
              }
              
              if(nonzero!=0){
                nzvars <- predict(m_glm, type = "nonzero") %>% .[,1] %>% paste0(collapse = " ")
              } else {
                nzvars = ""
              }
              
              y.pred.ts <- y.pred
              y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
              y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
              y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
              y.pred <- exp(y.pred)
              tibble(model = x$model,
                     window = x$window,
                     horizon = c(1:horizon),
                     date = dates[test_slice], # date of forecast
                     y.true = df_true$FC[test_slice] %>% as.numeric,
                     y.pred = y.pred,
                     bestlam = bestlam,
                     bestal = bestal,
                     nonzero = nonzero,
                     nzvars = nzvars,
                     y.true.ts = df$FC[test_slice] %>% as.numeric,
                     y.pred.ts = y.pred.ts
                     )
          }
            
        ## Spike-and-Slab ---- 
          # можно добавить nzavrs как и для lasso
          else if(grepl("ss", x$model)){
              set.seed(2019)
              
              m_ss <- spikeslab(x = X.train, y = y.train, n.iter2 = niter)
              
              
              y.pred.ts <- y.pred <- predict(m_ss, newdata = X.test)$yhat.gnet
              
              
              y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
              y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
              y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
              y.pred <- exp(y.pred)
              tibble(model = x$model,
                     window = x$window,
                     horizon = c(1:horizon),
                     date = dates[test_slice], # date of forecast
                     y.true = df_true$FC[test_slice] %>% as.numeric,
                     y.pred = y.pred,
                     y.true.ts = df$FC[test_slice] %>% as.numeric,
                     y.pred.ts = y.pred.ts)
          } 
          ## Boosting ----
          else if(grepl("boost", x$model)){
              
              tc <- trainControl(method = "cv", number = 5)
              tune_grid <- expand.grid(nrounds = niter,
                                       max_depth = c(5),
                                       eta = c(0.05),
                                       gamma = 1,
                                       colsample_bytree = 0.75,
                                       min_child_weight = 0,
                                       subsample = 0.6)
              set.seed(2019)
              m_boost <- train(x = X.train,
                               y = y.train,
                               method = "xgbTree", 
                               metric = "RMSE",
                               tuneGrid = tune_grid)
              
              y.pred.ts <- y.pred <- predict(m_boost, X.test)
              
              
              
              y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
              y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
              y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
              y.pred <- exp(y.pred)
              tibble(model = x$model,
                     window = x$window,
                     horizon = c(1:horizon),
                     date = dates[test_slice], # date of forecast
                     y.true = df_true$FC[test_slice] %>% as.numeric,
                     y.pred = y.pred,
                     y.true.ts = df$FC[test_slice] %>% as.numeric,
                     y.pred.ts = y.pred.ts)
          }
          ## Accelerator ----
          else if(grepl("acc", x$model)){
            data.train <- data.frame(I = y.train,
                                     GDP = df.train$GDPEA_Q_DIRI %>% as.numeric)
            
            data.test <- data.frame(I = 0,
                                    GDP = df.test$GDPEA_Q_DIRI %>% as.numeric)
            
            m_acc <- lm(I~GDP, data.train)
            
            y.pred <- y.pred.ts <- predict(m_acc, newdata = data.test)
            
            y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
            y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
            y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
            y.pred <- exp(y.pred)
            tibble(model = x$model,
                   window = x$window,
                   horizon = c(1:horizon),
                   date = dates[test_slice], # date of forecast
                   y.true = df_true$FC[test_slice] %>% as.numeric,
                   y.pred = y.pred,
                   y.true.ts = df$FC[test_slice] %>% as.numeric,
                   y.pred.ts = y.pred.ts)
            
            
          } else if(grepl("VAR",x$model)){
              m_VAR <- constructModel(df.train %>% as.matrix(),
                                         p=4,
                                         "Basic",
                                         gran=c(50,10),
                                         RVAR=FALSE,
                                         h=1,
                                         cv="Rolling",
                                         MN=FALSE,
                                         verbose=FALSE,
                                         IC=TRUE)
              
              out=cv.BigVAR(m_VAR)
              y.pred <- c()
              for(i in 1:horizon){
                y.pred <- c(y.pred, predict(out, i)[1,1])
              }
              
              y.pred.ts <- y.pred
              y.pred[1:4] <- log(df_true$FC[test_slice[1:4] - 4]) + y.pred[1:4]
              y.pred[5:8] <- y.pred[1:4] + y.pred[5:8]
              y.pred[9:12] <- y.pred[5:8] + y.pred[9:12]
              y.pred <- exp(y.pred)
              tibble(model = x$model,
                     window = x$window,
                     horizon = c(1:horizon),
                     date = dates[test_slice], # date of forecast
                     y.true = df_true$FC[test_slice] %>% as.numeric,
                     y.pred = y.pred,
                     y.true.ts = df$FC[test_slice] %>% as.numeric,
                     y.pred.ts = y.pred.ts)
              
            
          }

              }) %>% do.call.pipe(rbind)
       
          
    #message(paste0(Sys.time(),":\n", x$model," for window ",x$window, " and horizon ", horizon, " trained"))    
        
  }) %>%
    do.call.pipe(rbind)
}




# get.rw <- function(df, window, horizon, nlead, model){
#   map(nlead, function(nleadi){
#   # pre-train -----
#   # Преобразовываем y (смещаем на нужное nleadi число шагов)
#   df$FC <- lag.xts(df$FC, k = -nleadi)
#   # удаляем крайние значения 
#   df %<>% na.omit
#   dates <- time(df)
#   expand.grid(window = window, horizon = horizon) %>% 
#     split(seq(nrow(.))) %>% map(function(x){
#       TS <- createTimeSlices(dates, 
#                              initialWindow = x$window,
#                              horizon = x$horizon,
#                              fixedWindow = TRUE)
#       
#       # Random walk ----
#       
#       map2(TS$train, TS$test, function(tr, te){
#         
#         # значение y перед началом теста
#         y.pretest <- df$FC[last(tr)] %>% as.numeric()
#         
#         y.pred <- y.pretest
#         
#         tibble(model = model,
#                nlead = nleadi,
#                window = x$window,
#                horizon = x$horizon,
#                date = dates[te],
#                y.true = df$FC[te] %>% as.numeric,
#                y.pred = y.pred)
#       }) %>% do.call.pipe(rbind)
#       
#       
#     }) %>% do.call.pipe(rbind)
#   })%>% do.call.pipe(rbind)
# }
# 
# get.sacc <- function(df, window, horizon, nlead, model){
#   map(nlead, function(nleadi){
#     # pre-train -----
#     # удаляем крайние значения 
#     df <-  df[, c("FC","GDPEA_Q_DIRI")]
#     df %<>% na.omit
#     # даты
#     dates <- time(df)
#     expand.grid(window = window, horizon = horizon) %>% 
#       split(seq(nrow(.))) %>% map(function(x){
#         TS <- createTimeSlices(dates, 
#                                initialWindow = x$window,
#                                horizon = nleadi,
#                                fixedWindow = TRUE)
#         
#         # simple accelerator
#         # сначала прогнозируем gdp c помощью sarima на nlead шагов
#         # после этого по формуле I~forecast(gdp) (без лагов)
#         # строим прогнозные значения I
#         
#         map2(TS$train, TS$test, function(tr, te){
#           
#           deseas <- remove.seas(df=df,
#                                 train = tr,
#                                 test = te,
#                                 horizon = horizon,
#                                 onlyy = model == "rf_utf")
#           
#           
#           
#           
#           dfi <- df
#           dfi[c(tr, te)] <- rbind(deseas$ds, deseas$ds_for)
#           y.pred.s <- deseas$s_for %>% .$FC %>% as.numeric() %>% last
#           
#           X.train <- dfi[c(tr, first(te)),] %>% as.data.frame()
#           
#           X.pred <- tibble(FC = 0, GDPEA_Q_DIRI = NA)
#           X.pred$GDPEA_Q_DIRI <-
#             ets(X.train$GDPEA_Q_DIRI) %>% forecast(h = nleadi) %>%
#             .$mean %>%
#             as.numeric() %>%
#             last
#           
#           m_sacc <- lm(FC~GDPEA_Q_DIRI, data = X.train)
#           
#           
#           y.pred <- predict(m_sacc, data = X.pred) %>% last()
#           tibble(model = model,
#                  nlead = nleadi,
#                  window = x$window,
#                  horizon = x$horizon,
#                  date = dates[first(te)+nleadi-1],
#                  y.true = df$FC[first(te)+nleadi-1] %>% as.numeric,
#                  y.pred = y.pred+y.pred.s,
#                  y.pred.s = y.pred.s)
#         }) %>% do.call.pipe(rbind)
#         
#         
#       }) %>% do.call.pipe(rbind)
#   })%>% do.call.pipe(rbind)
#   
# }
# 
# 
# # get.ar <- function(df, window, horizon, nlead, model){
# #   
# #   
# #    
# #     # train -----
# #     # получаем результаты для каждой комбинации window, horizon
# #     else {
# #           if(grepl("sacc", model)){
# #             # Simple accelerator
# #             # I~Y without seasonality removing
# #             # сначал прогнозируем gdp после прогнозируем, не убирая сезонности и тренда, изменения I
# #             map2(TS$train, TS$test, function(tr, te){
# #               x_test <- 
# #               m_sacc <- lm(FC~0+GDPEA_Q_DIRI, data = df[tr,]) 
# #               y.pred <- predict(m_sacc, newdata = df[te,])
# #               
# #               tibble(model = model,
# #                      nlead = nleadi,
# #                      window = x$window,
# #                      
# #                      horizon = x$horizon,
# #                      date = dates[te],
# #                      y.true = df$FC[te] %>% as.numeric,
# #                      y.pred = y.pred)
# #             }) %>% do.call.pipe(rbind)
# #           } else {
# #             if(grepl("acc", model)){
# #               # Accelerator
# #               map2(TS$train, TS$test, function(tr, te){
# #                 m_sacc <- lm(FC~0+GDPEA_Q_DIRI, data = df[tr,]) 
# #                 y.pred <- predict(m_sacc, newdata = df[te,])
# #                 
# #                 tibble(model = model,
# #                        nlead = nleadi,
# #                        window = x$window,
# #                        
# #                        horizon = x$horizon,
# #                        date = dates[te],
# #                        y.true = df$FC[te] %>% as.numeric,
# #                        y.pred = y.pred)
# #               }) %>% do.call.pipe(rbind)
# #             }
# #           }
# #         }
# #       })})
# #   
# #   
# #   
# #   # # Преобразовываем y (смещаем на нужное nleadi число шагов)
# #   # df$FC <- lag.xts(df$FC, k = -nleadi)
# #   # # удаляем крайние значения 
# #   # df %<>% na.omit
# #   # #даты
# #   # dates <- time(df)
# #   # # приводим к матрично-векторному виду
# #   # y <- df$FC %>% as.numeric()
# #   # X <- as.matrix(model.matrix(FC~0+., data = df))
# #   # 
# #   #     
# #   # if(model == "rw"){
# #   #   nlead %>% map(function(nleadi){
# #   #     # удаление сезонности  
# #   #     deseas <- remove.seas(df=df,
# #   #                           train = TS$train[[1]],
# #   #                           test = TS$test[[1]],
# #   #                           horizon = horizon,
# #   #                           onlyy = model == "rf_utf")
# #   #     tibble(model = model,
# #   #            nlead = nleadi,
# #   #            date  = dates,
# #   #            y.true = y %>% as.numeric,
# #   #            y.pred = 0)
# #   #   }) %>% do.call.pipe(rbind)
# #   # } else if(model == "arp"){
# #   #   bestarma <- auto.arima(y,d = 0, max.p = 12, max.q = 12, seasonal = FALSE)
# #   #   bestp <- bestarma$arma[1]
# #   #   bestq <- bestarma$arma[2]
# #   #   map(nlead, function(nleadi){
# #   #     # получаем результаты для каждой комбинации window, horizon
# #   #     expand.grid(window = window, horizon = horizon) %>%
# #   #       split(seq(nrow(.))) %>%
# #   #       map(function(x){
# #   #         TS <- createTimeSlices(y[-c((length(y)-nleadi+1):length(y))],
# #   #                                initialWindow = x$window,
# #   #                                horizon = x$horizon,
# #   #                                fixedWindow = TRUE)
# #   #         map2(TS$train, TS$test, function(tr, te){
# #   #           # разбиваем выборку 
# #   #           y.true <- y.pred <- rep(NA, length(te))
# #   #           for(i in 1:(length(te))){
# #   #             #print(sum(y[(te[i+1]):(te[i+1]+nleadi-1)]))
# #   #             # обучение модели
# #   #             y.train <- y[c(tr[-c(1:i)], (last(tr)+1):(last(tr)+i))]
# #   #             m_arma <- arima0(y.train,
# #   #                              order = c(bestp, 0 , bestq))
# #   #             # сумма изменений
# #   #             y.pred[i] <- predict(m_arma,
# #   #                               n.ahead = nleadi,
# #   #                               se.fit = FALSE) %>% sum
# #   #             y.true[i] <- sum(y[(te[i]+1):(te[i]+nleadi)])
# #   #               
# #   #           }
# #   #           tibble(model = model,
# #   #                  nlead = nleadi,
# #   #                  bestp = bestp,
# #   #                  bestq = bestq,
# #   #                  window = x$window,
# #   #                  horizon = x$horizon,
# #   #                  date = dates[te],
# #   #                  y.true = y.true,
# #   #                  y.pred= y.pred)
# #   #         }) %>% do.call.pipe(rbind)
# #   #         
# #   #       })%>%
# #   #       do.call.pipe(rbind)
# #   #   })%>%
# #   #     do.call.pipe(rbind)
# #   #   
# #   # } else stop("unknown model") 
# #   # 
# # }
# 


transform_to_score <- function(df){
  df %>%
    split(.$model) %>%
    map(function(x){
      x %>%  print()
      
    })
    
}

get.score <- function(df){
  df %>%
    group_by(horizon, window, model) %>%
    summarise(rmspe = RMSPE(y.pred.ts, y.true.ts),
              rmse = RMSE(y.pred.ts, y.true.ts),
              rrse = RRSE(y.pred.ts, y.true.ts),
              mae= MAE(y.pred.ts, y.true.ts),
              r2 = R2_Score(y.pred.ts, y.true.ts))
}


check.normal <- function(df = outall %>%
                           filter(model!="Accelerator") ){
  df %>%
    mutate(resid = y.true - y.pred,
           group = paste(model, window, horizon)) %>%
    split(.$group) %>%
    map_dfr(function(x){
      sw = shapiro.test(x$resid) %>% .$p.value
      tibble(model = first(x$model),
             window = first(x$window),
             horizon = first(x$horizon), pv = sw)
    })
}

