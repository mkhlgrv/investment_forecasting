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


get.panel.r <- function(df, window, horizon, nlead, model, niter = NULL){
  if(!any(c("zoo", "xts") %in% class(df))){
    stop("df must be 'zoo'")
  }
  # Поменяем порядок аргументов
  
  map(nlead, function(nleadi){
    
    # Преобразовываем данные
    # (преобразуем здесь, а не внутри import.R, потому что, возможно, будем исследовать не только unemp)
    # мы предсказываем сумму изменений беработицы
    unemp <- df$UNEMPL_M_SH
    df$UNEMPL_M_SH <- lag.xts(df$UNEMPL_M_SH, k = -1)
    if(nleadi>=2){
      for(i in 2:nleadi){
        df$UNEMPL_M_SH <- df$UNEMPL_M_SH + lag.xts(unemp, k = -i)
      }
    }
    # удаляем крайние значения 
    df %<>% na.omit
    dates <- time(df)
    # приводим к матрично-векторному виду
    y <- df$UNEMPL_M_SH %>% as.numeric()
    X <- as.matrix(model.matrix(UNEMPL_M_SH~0+., data = df))
    
    # получаем результаты для каждой комбинации window, horizon
    expand.grid(window = window, horizon = horizon) %>% 
      split(seq(nrow(.))) %>% map(function(x){
        TS <- createTimeSlices(y, 
                               initialWindow = x$window,
                               horizon = x$horizon,
                               fixedWindow = TRUE)
        if(grepl("rf", model)){
          # Random forest ----
          
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            rf.out <- train(x = X.train,
                            y = y.train,
                            method = "rf", 
                            metric = "RMSE")
            bestmtry <- rf.out$bestTune[1,1]
            m_rf <- randomForest(x = X.train, y = y.train, mtry = bestmtry)
            y.pred <- predict(m_rf, newdata = X.test) %>%
              as.numeric
            tibble(model = model,
                   nlead = nleadi,
                   window = x$window,
                   mtry = bestmtry,
                   horizon = x$horizon,
                   date = dates[te],
                   y.true = y.test,
                   y.pred= y.pred)
          }) %>% do.call.pipe(rbind)
          
          
        } else if(grepl(c("lasso|ridge|elnet"),
                        model)){
          
          if(grepl("lasso", model)){
            bestal = 1
          } else if(grepl("ridge", model)){
            bestal = 0
          } else if(grepl("elnet", model)){
            bestal = 0.5
          }
          
          
          
          # assign("cv.out", cv.out, envir = globalenv())
          # stop()
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            
            if(grepl("pc", model)){
              pclist <-  prcomp(X.train)
              X.train <- pclist$x
              X.test <- predict(pclist, newdata = X.test)
            }
            
            if(grepl("adaptive", model)){
              train_ada <- as.data.frame(X.train) %>%
                mutate(U = y.train)
              test_ada <-  as.data.frame(X.test) %>%
                mutate(U = y.test)
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
                            tuneGrid = expand.grid(.alpha = bestal,.lambda = seq(0.1,0.00001,length = niter)))
            bestlam <- glm.out$bestTune$lambda
            m_glm <- glmnet(X.train, y.train, alpha = bestal, lambda = bestlam, penalty.factor = w3)
            nonzero <- predict(m_glm, type = "nonzero") %>% nrow
            if(is.null(nonzero)){
              nonzero <- 0
            } 
            if(!grepl("post", model)){
              y.pred <- predict(m_glm,newx = X.test)
              if(length(dim(y.pred)!=1)){
                y.pred <- y.pred[,1]
              }
            } else if(grepl("post", model)){
              if(nonzero!=0){
                nzvars <- predict(m_glm, type = "nonzero") %>% .[,1]
                train_post <- as.data.frame(X.train) %>%
                  select(nzvars) %>%
                  mutate(U = y.train)
                test_post <-  as.data.frame(X.test) %>%
                  select(nzvars) %>%
                  mutate(U = y.test)
                m_post <- lm(U~., data = train_post)
                y.pred <- predict(m_post, newdata = test_post)
              } else{
                message(paste0(model,
                               ": none of nonzero coeffs for lambda = ",
                               bestlam , 
                               " and alpha = ", bestal))
                y.pred <- predict(m_glm,newx = X.test)
              }
              
            } else stop("unknown model")
            
            if(nonzero!=0){
              nzvars <- predict(m_glm, type = "nonzero") %>% .[,1] %>% paste0(collapse = " ")
            } else {
              nzvars = ""
            }
            
            tibble(model = model,
                   nlead = nleadi,
                   bestlam = bestlam,
                   bestal = bestal,
                   window = x$window,
                   horizon = x$horizon,
                   date = dates[te],
                   nonzero = nonzero,
                   nzvars = nzvars,
                   y.true = y.test,
                   y.pred= y.pred)
          }) %>% do.call.pipe(rbind)
          
          
          
          
          
        } 
        else if(grepl("ss", model)){
          
            map2(TS$train, TS$test, function(tr, te){
              set.seed(2019)
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            m_ss <- spikeslab(x = X.train, y = y.train, n.iter2 = niter)
            y.pred <- predict(m_ss, newdata = X.test)$yhat.gnet
            tibble(model = model,
                   nlead = nleadi,
                   window = x$window,
                   horizon = x$horizon,
                   date = dates[te],
                   y.true = y.test,
                   y.pred= y.pred)
          
          }) %>% do.call.pipe(rbind)
        } else if(grepl("boost", model)){
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            tc <- trainControl(method = "cv", number = 5)
            tune_grid <- expand.grid(nrounds = niter,
                                     max_depth = c(5),
                                     eta = c(0.05),
                                     gamma = 1,
                                     colsample_bytree = 0.75,
                                     min_child_weight = 0,
                                     subsample = 0.6)
            set.seed(2019)
            bst.out <- train(x = X.train,
                             y = y.train,
                             method = "xgbTree", 
                             metric = "RMSE",
                             tuneGrid = tune_grid)
            y.pred <- predict(bst.out, X.test)
            tibble(model = model,
                   nlead = nleadi,
                   window = x$window,
                   horizon = x$horizon,
                   date = dates[te],
                   y.true = y.test,
                   y.pred= y.pred)
          }) %>% do.call.pipe(rbind) 
        }
          else {stop(paste0(model,": unknown model"))}
        
        
      })%>% do.call.pipe(rbind)
    
    
  })%>% do.call.pipe(rbind)
}

get.ar <- function(df, window, horizon, nlead, model){
  # Данные
  y <- df_tf$UNEMPL_M_SH %>% na.omit
  # время
  dates <- time(y)
  if(model == "rw"){
    nlead %>% map(function(nleadi){
      y.true <- rep(NA, nleadi)
      for(i in 1:(length(y)-nleadi)){
        y.true[i] <- sum(y[(i+1):(i+nleadi)])
      }
      tibble(model = model,
             nlead = nleadi,
             date  = dates,
             y.true = y %>% as.numeric,
             y.pred = 0)
    }) %>% do.call.pipe(rbind)
  } else if(model == "arp"){
    bestarma <- auto.arima(y,d = 0, max.p = 12, max.q = 12, seasonal = FALSE)
    bestp <- bestarma$arma[1]
    bestq <- bestarma$arma[2]
    map(nlead, function(nleadi){
      # получаем результаты для каждой комбинации window, horizon
      expand.grid(window = window, horizon = horizon) %>%
        split(seq(nrow(.))) %>%
        map(function(x){
          TS <- createTimeSlices(y[-c((length(y)-nleadi+1):length(y))],
                                 initialWindow = x$window,
                                 horizon = x$horizon,
                                 fixedWindow = TRUE)
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку 
            y.true <- y.pred <- rep(NA, length(te))
            for(i in 1:(length(te))){
              #print(sum(y[(te[i+1]):(te[i+1]+nleadi-1)]))
              # обучение модели
              y.train <- y[c(tr[-c(1:i)], (last(tr)+1):(last(tr)+i))]
              m_arma <- arima0(y.train,
                               order = c(bestp, 0 , bestq))
              # сумма изменений
              y.pred[i] <- predict(m_arma,
                                n.ahead = nleadi,
                                se.fit = FALSE) %>% sum
              y.true[i] <- sum(y[(te[i]+1):(te[i]+nleadi)])
                
            }
            tibble(model = model,
                   nlead = nleadi,
                   bestp = bestp,
                   bestq = bestq,
                   window = x$window,
                   horizon = x$horizon,
                   date = dates[te],
                   y.true = y.true,
                   y.pred= y.pred)
          }) %>% do.call.pipe(rbind)
          
        })%>%
        do.call.pipe(rbind)
    })%>%
      do.call.pipe(rbind)
    
  } else stop("unknown model") 
  
}



get.score <- function(df, type){
  if(type == "regular"){
    df %<>% 
      group_by(horizon, window, nlead, model)
  } else if(type == "ar"){
    df %<>% 
      group_by(model, nlead, bestq, bestp, window, horizon)
  } else if(type == "rw"){
    df %<>% 
      na.omit %>%
      group_by(model, nlead)
  } else if(type %in% c("rf", "ss")){
    df %<>%
      group_by(horizon, window, nlead, model)
  } 
  df %>%
    summarise(rmspe = RMSPE(y.pred, y.true),
              rmse = RMSE(y.pred, y.true),
              rrse = RRSE(y.pred, y.true),
              mae= MAE(y.pred, y.true),
              r2 = R2_Score(y.pred, y.true))
}
