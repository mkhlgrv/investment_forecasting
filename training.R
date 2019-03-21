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
  # (преобразуем здесь, а не внутри import.R, потому что, возможно, будем исследвоать не только unemp)
  df$UNEMPL_M_SH <- lag.xts(df$UNEMPL_M_SH, k = -1)
  df %<>% na.omit
  dates <- time(df)
  expand.grid(window = window, horizon = horizon) %>% 
    split(seq(nrow(.))) %>% map_dfr(function(x){
      TS <- createTimeSlices(dates, initialWindow = x$window,
                             horizon = x$horizon,
                             fixedWindow = TRUE,
                             skip = FALSE)
      map2_dfr(TS$train, TS$test, function(tr, te){
        df_train <- df %>%
          window(start = dates[first(tr)],
                 end = dates[last(tr)])
        df_test <- df %>%
          window(start = dates[first(te)],
                 end = dates[last(te)])
        
        date_test <- dates[last(te)]
        X <- model.matrix(UNEMPL_M_SH~., data = df_train)
        y <- df_train$UNEMPL_M_SH
        lambdas <- seq(0.07, 1/10000000000, length = 300) # надо разобраться
        m_lasso <- glmnet(X, y, alpha = 1, lambda = lambdas)
        print(m_lasso)
        print(plot(m_lasso, xvar = "lambda", label = TRUE))
        stop()
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
get.panel.r(df_tf, 120, 6)
model2 <- cv.glmnet(x = data2.x, y= data2.y, standardize = TRUE)

bestlam <- model2$lambda.min
lasso.coef  <- predict(model2, type = 'coefficients', s = 0.001)
lasso.coef
cv.g