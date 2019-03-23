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

get.panel.r <- function(df, window, horizon, nlead){
  if(!any(c("zoo", "xts") %in% class(df))){
    stop("df must be 'zoo'")
  }
  # Поменяем порядок аргументов
  do.call.pipe <- function(args,what){
    do.call(what,args)
  }
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
        map2(list(bestlam, bestlam/2, bestlam/3, bestlam/4, bestlam/5),
             as.list(seq(1:5)),
             function(lambda, ltype){
          map2(TS$train, TS$test, function(tr, te){
            # разбиваем выборку
            X.train <- X[tr, ]
            X.test <- X[te, ]
            y.train <- y[tr]
            y.test <- y[te]
            m_lasso <- glmnet(X.train, y.train, alpha = 1, lambda = lambda)
            nonzero <- predict(m_lasso, type = "nonzero") %>% nrow
            if(is.null(nonzero)){
              nonzero <- 0
            }
            y.pred <- predict(m_lasso,newx = X.test)
            tibble(model = "lasso",
                   nlead = nleadi,
                   bestlam = bestlam,
                   lambda = ltype,
                   window = x$window,
                   horizon = x$horizon,
                   pred.date = dates[last(tr)],# дата предсказания
                   date = dates[te],
                   nonzero = nonzero,
                   y.true = y.test,
                   y.pred= y.pred[,1])
          }) %>% do.call.pipe(rbind)
        })%>% do.call.pipe(rbind)
        
        
      })%>% do.call.pipe(rbind)
  })%>% do.call.pipe(rbind)
  
}

lasso.out <- get.panel.r(df_tf, 120, 12,nlead = c(1:6))
rbind(lasso.out,
      lasso.out.lag %>%
        mutate(model = "lasso_lag")) %>%
  select(-c(window, horizon)) %>%
  filter(lambda == 5, nlead ==2) %>%
  group_by(date, lambda, nlead, model) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true"), size =0.9) +
  geom_line(aes(x = date, y = y.pred, colour = model), size = 0.9)+
  ylab("unemploymnet")+
  facet_wrap(vars(nlead))

rbind(lasso.out,
      lasso.out.lag %>%
        mutate(model = "lasso_lag")) %>%
  group_by(model, nlead, lambda, date) %>%
  summarise(y.true = mean(y.true),
            y.pred = mean(y.true),
            nonzero = mean(nonzero)) %>%
  ungroup %>%
  group_by(model, nlead, lambda) %>%
  mutate(y.true = cumsum(y.true)) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% View
  ggplot(aes(x = date, y = y.true))+
  geom_line()
  m %>%
  ggplot() +
  geom_line(aes(x = date, y = y_true,colour = "true"))+
  geom_line(aes(x = date, y =y_pred, colour = model)) +
  facet_grid(rows = vars(horizon), cols = vars(window))


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
      df <- merge.xts(df, new_lag=lag.xts(y, k = i))
      names(df)[length(names(df))] <- paste0(yname, "_lag",i)
    }
  }
  df
}

# все переменные залагованы на 6
df_tf_lag <- create.lagv(df_tf, nlag = 6L)


lasso.out.lag <- get.panel.r(df = df_tf_lag, window = 120, horizon = 12, nlead = c(1:6))

lasso.out.lag %>%
  
  group_by(date, lambda) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true")) +
  geom_line(aes(x = date, y = y.pred, colour = "pred"))+
  facet_wrap(vars(lambda))


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
  
