# Autoregression -----
# Random Walk and AR(p)

rm(list = ls())
load("tfdata.RData")
# Модель предполагает, что безработица сохранится на таком же уровне
# посчитаем score mae rmse

get.ar <- function(df, window, horizon, nlead, model){
  # Данные
  y <- df_tf$UNEMPL_M_SH %>% na.omit
  # время
  dates <- time(y)
  if(model == "rw"){
    nlead %>% map(function(nleadi){
      tibble(model = model,
             nlead = nleadi,
             pred.date =lag.xts(dates, k = nleadi), 
             date  = dates,
             y.true = y %>% as.numeric,
             y.pred = lag.xts(y, k = nleadi) %>% as.numeric)
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
          TS <- createTimeSlices(y,
                                 initialWindow = x$window,
                                 horizon = x$horizon,
                                 fixedWindow = TRUE,
                                 skip = nleadi-1)
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку  
            y.train <- y[tr]
            y.true <- y[te] %>% as.numeric
            m_arma <- arima0(y,
                             order = c(bestp, 0 , bestq))
            y.pred <- predict(m_arma, n.ahead = horizon + nleadi-1, se.fit = FALSE) %>%
              .[(nleadi): (horizon + nleadi-1)]
            tibble(model = model,
                   nlead = nleadi,
                   bestp = bestp,
                   bestq = bestq,
                   window = x$window,
                   horizon = x$horizon,
                   pred.date = dates[last(tr)],# дата предсказания
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

arlist <- map(c("rw","arp"),
              function(modeli){get.ar(df = df_tf,
                                      window = 120,
                                      horizon = 12,
                                      nlead = c(1:12),
                                      model = modeli)})
save(arlist, file = "arlist.RData")

# Panel data ----
rm(list=ls())
load("tfdata_panel.RData")

## Regression with regularisation ----
## Ridge, LASSO, Post-LASSO, and Elastic Network----

do.call.pipe <- function(args,what){
  do.call(what,args)
}
get.regular.r <- function(df, window, horizon, nlead, model){
  if(!any(c("zoo", "xts") %in% class(df))){
    stop("df must be 'zoo'")
  }
  # Поменяем порядок аргументов
  
  map(nlead, function(nleadi){
    
    # Преобразовываем данные
    # (преобразуем здесь, а не внутри import.R, потому что, возможно, будем исследовать не только unemp)
    # мы предсказываем следующее значение безработицы
    df$UNEMPL_M_SH <- lag.xts(df$UNEMPL_M_SH, k = -nleadi)
    # удаляем крайние значения 
    df %<>% na.omit
    dates <- time(df)
    # приводим к матрично-векторному виду
    y <- df$UNEMPL_M_SH %>% as.numeric()
    X <- as.matrix(model.matrix(UNEMPL_M_SH~., data = df))
    
    # получаем результаты для каждой комбинации window, horizon
    expand.grid(window = window, horizon = horizon) %>% 
      split(seq(nrow(.))) %>% map(function(x){
        tc <- trainControl(method = "timeslice", initialWindow = x$window,
                           horizon = x$horizon,
                           fixedWindow = TRUE,
                           skip = FALSE)
        if(grepl("lasso", model)){
          alpha = 1
        } else if(grepl("ridge", model)){
          alpha = 0
        } else if(grepl("elnet", model)){
          alpha = seq(0,1,by = 0.1)
        }
        set.seed(2019)
        cv.out <- train(x=X,
                        y=y,
                        method = "glmnet",
                        metric = "RMSE",
                        trControl = tc,
                        tuneGrid = expand.grid(.alpha = alpha,.lambda = seq(0.1,0.00001,length = 5000)))
        bestlam <- cv.out$bestTune$lambda
        bestal <- cv.out$bestTune$alpha
        # assign("cv.out", cv.out, envir = globalenv())
        # stop()
        TS <- createTimeSlices(y, 
                               initialWindow = x$window,
                               horizon = x$horizon,
                               fixedWindow = TRUE,
                               skip = FALSE)
        map2(list(bestlam, bestlam/2, bestlam/3, bestlam/4, bestlam/5),
             as.list(seq(1:5)),
             function(lambda, ltype){
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            m_glm <- glmnet(X.train, y.train, alpha = bestal, lambda = lambda)
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
                    mutate(u = y.test)
                  m_post <- lm(U~., data = train_post)
                  y.pred <- predict(m_post, newdata = test_post)
                } else{
                message(paste0(model,
                               ": none of nonzero coeffs for lambda = ",
                               bestlam , ltype,
                               " and alpha = ", bestal))
                y.pred <- predict(m_glm,newx = X.test)
              }
            } else stop("unknown model")
            tibble(model = model,
                   nlead = nleadi,
                   bestlam = bestlam,
                   bestal = bestal,
                   lambda = ltype,
                   window = x$window,
                   horizon = x$horizon,
                   pred.date = dates[last(tr)],# дата предсказания
                   date = dates[te],
                   nonzero = nonzero,
                   y.true = y.test,
                   y.pred= y.pred)
          }) %>% do.call.pipe(rbind)
        })%>% do.call.pipe(rbind)
        
        
      })%>% do.call.pipe(rbind)
  })%>% do.call.pipe(rbind)
  
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

lasso.out <- get.regular.r(df_tf, 120, 12,nlead = c(1:6), model = "lasso")
postlasso.out <-get.regular.r(df_tf, 120, 12,nlead = c(1:6), model = "post_lasso")
# Попрбобуем трансформировать данные еще сильнее. Добавим лагированные переменные
# все переменные залагованы на 6
df_tf_lag <- create.lagv(df_tf, nlag = 6L)
lasso.out.lag <- get.panel.r(df = df_tf_lag, window = 120, horizon = 12, nlead = c(1:6), model = "lasso_lag")
postlassolag.out <-get.panel.r(df_tf_lag, 120, 12,nlead = c(1:6), model = "post_lasso_lag")

ridge.out <- get.panel.r(df_tf_lag, 120, 12,nlead = c(1:6), model = "ridge")

glmnet_list <- list(lasso.out, postlasso.out,lasso.out.lag, postlassolag.out, ridge.out %>% select(-bestal))
save(glmnet_list, file = "glmnet_list.RData")
glmneterror <- do.call(rbind, glmnet_list) %>%
  select(-c(window, horizon)) %>%
  #filter(lambda == 5, nlead ==2) %>%
  group_by(date, lambda, nlead, model) %>%
  mutate(y.pred = mean(y.pred),
         nonzero = mean(nonzero)) %>%
  mutate(error = y.pred - y.true) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_density(aes(x = error, colour = model), size = 0.4)+
  #geom_line(aes(x = date, y = y.true, colour = "true"), size =0.9) +
  #geom_line(aes(x = date, y = abs(y.true - y.pred), colour = model), size = 0.9)+
  labs(title = "Forecast error distribution",y ="unemploymnet")+
  facet_grid(rows =vars(nlead), cols = vars(lambda), labeller = label_both)

pdf("glmnet_error.pdf")
print(glmneterror)
dev.off()
load("rawdata_panel.RData")
unemp.true <- df$UNEMPL_M_SH %>%
  as.data.frame() %>%
  rownames_to_column %>%
  setNames(c("date","y.true")) %>%
  mutate(date = as.yearmon(date))
# график в уровнях ----
p <- do.call(rbind, lasso_list) %>%
  filter(nlead %in% 2:4, lambda %in% 2:4) %>%
  group_by(model, nlead, lambda, date) %>%
  mutate(y.pred = mean(y.pred),
         nonzero = mean(nonzero)) %>%
  ungroup() %>%
  select(-c(y.true, pred.date)) %>%
  unique %>%
  inner_join(unemp.true, by = "date") %>%
  group_by(model, nlead, lambda) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% 
  ggplot() +
  geom_smooth(aes(x = date, y = y.true, colour = "true"), size =0.5, span = 0.7, se = F) +
  geom_smooth(aes(x = date, y = y.pred, colour = model), size = 0.5, span = 0.7, se = F)+
  ylab("unemploymnet")+
  facet_grid(rows =vars(nlead),cols = vars(lambda))

pdf("LASSO_forecast.pdf")
print(p)
dev.off()



lasso.out.lag %>%
  
  group_by(date, lambda) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true")) +
  geom_line(aes(x = date, y = y.pred, colour = "pred"))+
  facet_wrap(vars(lambda))

# Теперь используем Post-LASSO




get.score <- function(df){
    df %>% 
    group_by(horizon, window, lambda, nlead, model) %>%
    summarise(rmspe = RMSPE(y.pred, y.true),
              rmse = RMSE(y.pred, y.true),
              mae= MAE(y.pred, y.true),
              r2 = R2_Score(y.pred, y.true),
              nonzero = mean(nonzero)) %>%
    ungroup
}


score_df <- rbind(get.score(lasso.out),
get.score(lasso.out.lag) %>% mutate(model = "lasso_lag"))
score_df
score_info <- score_df %>%
  group_by(model, nlead) %>%
  summarise(mean = mean(rmse),
            sd = sd(rmse))
score_df %>%
  mutate(nlead=as.factor(nlead)) %>%
  ggplot()+
  geom_point(aes(y = rmse, x = nlead, colour = model), position = position_dodge(width = 0.9))+
  geom_errorbar(data = score_info, aes(x = nlead, y = mean, ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.9))
  
