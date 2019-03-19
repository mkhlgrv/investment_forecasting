library(sophisthse)
library(tseries)
library(MLmetrics)
library(caret)
library(xts)
series <- series_info %>%
  filter(freq == 12)
# загружаем данные
df <- sophisthse(series.name = series$table, output = "data.frame")

missmap(df)
df2 <- df %>%
  filter(!is.na(UNEMPL_M_SH),
               `T` >= zoo::as.yearmon("2002-01-01"),
         `T` <= zoo::as.yearmon("2016-12-01"))

nonmis <- sapply(df2, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` == 0) %>%
  pull(tname)
  
df2 %<>% select(nonmis)
# пропущенные значения
missmap(df2)
getwd()
df <- df2
#save(df, file = "df.RData")
rm(list = ls())
load("df.RData")

# нарисуем данные
ggplot(df) + geom_line(aes(x = `T`, y = UNEMPL_M_SH))
# Во-первых, очистим данные от сезонности и тренда

# addictive 
y_dec <- sophisthse(series.name = "UNEMPL_M_SH", output = "zoo") %>%
  window(start = zoo::as.yearmon("2002-01-01"),
         end = zoo::as.yearmon("2018-12-01")) %>% 
  decompose()
y <- y_dec$x - y_dec$seasonal
autoplot(y)
# adf test выбираем addictive seasoanlity
y <- y - stats::lag(y, -1, na.pad = TRUE)
autoplot(y)
# кажется этого достаточно
acf(y, lag.max = length(y))
pacf(y, lag.max = length(y))
# корреляции
cormat <- cor(df[,-1])
cormatbig <- matrix(0, 130, 130)
cormatbig[which(abs(cormat)>0.99)] <- 1


y_dates <- zoo::as.yearmon(time(y))
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
