# AR -----
rm(list = ls())
load("rawdata_ar.RData")
# Случайное блужданиe

# Модель предполагает, что безработица сохранится на таком же уровне
# посчитаем score mae rmse


get.rw <- function(y, dates){# безработица
  data.frame(date = dates,
                       window = 0,
                       horizon = 1,
             # костыль, надо нормально скачать xts
                       y_true = y %>% as.vector(),
             y_pred = 0,
                       model = "randomwalk") %>%
    #mutate(y_pred = lag(y_true)) %>% 
    na.omit
}

get.ar1 <- function(y, dates, window, horizon){
  expand.grid(window = window, horizon = horizon) %>% 
    split(seq(nrow(.))) %>% map_dfr(function(x){
      TS <- createTimeSlices(y, initialWindow = x$window,
                             horizon = x$horizon,
                             fixedWindow = TRUE,
                             skip = FALSE)
      map2_dfr(TS$train, TS$test, function(tr, te){
        y_train <-  y[tr]
        y_test <- y[te] %>% last
        date_test <- dates[te] %>% last
        model <- auto.arima(y_train,max.order = 1, max.q = 0, d = 0, allowmean = FALSE)
        y_pred <- forecast(model, h = x$horizon) %>% as.data.frame %>% .[,1] %>% last
        data.frame(date =date_test,
                   window = x$window,
                   horizon = x$horizon,
                   y_true = y_test,
                   y_pred= y_pred)
      })
      
    }) %>%
    mutate(model = "ar1")
}

get.ar <- function(y, dates, window, horizon){
  expand.grid(window = window, horizon = horizon) %>% 
    split(seq(nrow(.))) %>% map_dfr(function(x){
      TS <- createTimeSlices(y, initialWindow = x$window,
                       horizon = x$horizon,
                       fixedWindow = TRUE,
                      skip = FALSE)
      map2_dfr(TS$train, TS$test, function(tr, te){
        y_train <-  y[tr]
        y_test <- y[te] %>% last
        date_test <- dates[te] %>% last
        model <- auto.arima(y_train, max.q = 0, d = 0, allowmean = FALSE)
        y_pred <- forecast(model, h = x$horizon) %>% as.data.frame %>% .[,1] %>% last
        data.frame(date =date_test,
                   window = x$window,
                   horizon = x$horizon,
                   y_true = y_test,
                   y_pred= y_pred)
      })
      
    }) %>%
    mutate(model = "arp")
}
result_df = rbind(get.rw(y = y,
                             dates = y_dates), 
                      get.ar1(y = y,
                             dates = y_dates,
                             window = c(120),
                             horizon = c(1, 3, 6, 12)),        
             get.ar(y = y,
                    dates = y_dates,
                    window = c(120),
                    horizon = c(1, 3, 6, 12))) %>%
  mutate(date = zoo::as.yearmon(date))

# считаем значения метрик
start_date <- result_df %>%  # раньше этой даты оценивать некорректно
  group_by(window, horizon, model) %>%
  summarise(start_date = min(date)) %>%
  pull(start_date) %>%
  max
score_df <- result_df %>%
  filter(date>= start_date) %>%
  group_by(window, horizon, model) %>%
  summarise(mae = MAE(y_pred, y_true),
            rmse = RMSE(y_pred, y_true))

score_df  %>% as.data.frame() %>% print(digits = 10)

# строим график изменений
result_df %>%
  #filter(date>= start_date) %>%
  filter(horizon==3) %>%
  ggplot() +
  geom_line(aes(x = date, y = y_true,colour = "true"))+
  geom_line(aes(x = date, y =y_pred, colour = model)) +
  facet_grid(rows = vars(horizon), cols = vars(window))
# строим график в уровнях
result_df %>%
  #filter(date>= start_date) %>%
  #filter(horizon==3) %>%
  group_by(window, horizon, model) %>%
  mutate(y_true = cumsum(y_true)) %>%
  mutate(y_pred = sapply(seq_along(horizon), function(h, horizon, y_true){
    indx = h - horizon[h]
    if(indx > 0)
      y_true[indx]
    else
      NA
  }, horizon = horizon, y_true = y_true) + y_pred) %>%
  ggplot() +
  geom_line(aes(x = date, y = y_true,colour = "true"))+
  geom_line(aes(x = date, y =y_pred, colour = model)) +
  facet_grid(rows = vars(horizon), cols = vars(window))

# НАДО Поменять даты на ноябрь 2001 - январь 2017!!!
# Panel data ----
rm(list=ls())
load("tfdata_panel.RData")
## LASSO ----

get.panel.r <- function(df, window, horizon){
  if(!"zoo" %in% class(df)){
    stop("df must be 'zoo'")
  }
  # важно, что предсказываем мы следующее значение безработицы 
  df$UNEMPL_M_SH <- lag.xts(df$UNEMPL_M_SH, k = -1)
  # удаляем крайние значения 
  df %<>% na.omit
  dates <- time(df)
  
  # преобразовываем данные
  # (преобразуем здесь, а не внутри import.R, потому что, возможно, будем исследовать не только unemp)
  y <- df$UNEMPL_M_SH %>% as.numeric()
  X <- as.matrix(model.matrix(UNEMPL_M_SH~., data = df))
  
  expand.grid(window = window, horizon = horizon) %>% 
    split(seq(nrow(.))) %>% map(function(x){
      tc <- trainControl(method = "timeslice", initialWindow = x$window,
                         horizon = x$horizon,
                         fixedWindow = TRUE,
                         skip = FALSE)
      cv.out <- train(x=X,
            y=y,
            method = "glmnet",
            metric = "RMSE",
            trControl = tc,
            tuneGrid = expand.grid(.alpha = 1,.lambda = seq(0.11,0.0001,length = 100)))
      bestlam <- cv.out$bestTune$lambda
      # assign("cv.out", cv.out, envir = globalenv())
      # stop()
      TS <- createTimeSlices(y, 
                             initialWindow = x$window,
                         horizon = x$horizon,
                         fixedWindow = TRUE,
                         skip = FALSE)
      map(list(bestlam, bestlam/2, bestlam/3, bestlam/4, bestlam/5), function(lambda){
        map2(TS$train, TS$test, function(tr, te){
          # разбиваем выборку
          X.train <- X[tr, ]
          X.test <- X[te, ]
          y.train <- y[tr]
          y.test <- y[te]
          m_lasso <- glmnet(X.train, y.train, alpha = 1, lambda = lambda)
          nnonzero <- predict(m_lasso, type = "nonzero") %>% nrow
          y.pred <- predict(m_lasso,newx = X.test)
          tibble(pred.date = dates[last(tr)],# дата предсказания
                 date = dates[te],
                 nnonzero = nnonzero,
                 model = "lasso",
                 lambda = lambda,
                 window = x$window,
                 horizon = x$horizon,
                 y.true = y.test,
                 y.pred= y.pred[,1])
        })
      })

      
    })
}

lasso.out <- get.panel.r(df_tf, 120, c(12))
lasso.out <- do.call(rbind,do.call(rbind,do.call(rbind,lasso.out)))
lasso.out %>%
  group_by(date, lambda) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true")) +
  geom_line(aes(x = date, y = y.pred, colour = "pred"))+
  facet_wrap(vars(lambda))

# Попрбобуем трансформировать данные еще сильнее. Добавим лагированные переменные
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
      df <- merge.zoo(df, new_lag=lag.xts(y, k = i))
      names(df)[length(names(df))] <- paste0(yname, "_lag",i)
    }
  }
  df
}

# все переменные залагованы на 6
df_tf_lag <- create.lagv(df_tf, nlag = 6L)


lasso.out.lag <- get.panel.r(df = df_tf_lag, window = 120, horizon = 12)
lasso.out.lag <- do.call(rbind,do.call(rbind,do.call(rbind,lasso.out.lag)))
lasso.out.lag %>%
  
  group_by(date, lambda) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true")) +
  geom_line(aes(x = date, y = y.pred, colour = "pred"))+
  facet_wrap(vars(lambda))


get.rmspe <- function(df){
    do.call(rbind, split(df,seq(nrow(df))) %>%
    map(function(x){
      x$horizon <- seq(as.Date(x$pred.date),
                              as.Date(x$date),
                              by = "month") %>%
                          length-1
      x
    }))%>%
    group_by(horizon, window, lambda) %>%
    mutate(rmspe = RMSPE(y.pred, y.true)) %>%
    ungroup
}
score_df <- rbind(get.rmspe(lasso.out),
get.rmspe(lasso.out.lag) %>% mutate(model = "lasso_lag"))
score_df %>%
  mutate(horizon=as.factor(horizon)) %>%
  ggplot()+
  geom_boxplot(aes(y = rmspe, x = horizon, colour = model), position = position_dodge(width = 0.9))
  
