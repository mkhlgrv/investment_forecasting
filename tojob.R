
source('lib.R')
create_lag <- function(df, lag){
  out <- df
  for(i in 1:lag){
    x <- lag.xts(df, k = i)
    colnames(x) <- paste0(colnames(x), "_lag", i)
    out %<>% merge.zoo(x)
  }
  return(out %>% as.xts)
}



# 1. начальная дата
# 2. конечная дата
# 3. модель
# 4. набор переменных (только из тех, что указаны в calibrating)
# 5. на сколько периодов вперед прогнозировать (max)

train.model <- function(startdt, enddt, model,
                        # series parameter 
                        # only for regularisation and machine learning models
                        series='', lag = 4, h = 4L){
  # import df
  load('data/stationary_data.RData')
  
  if(series!=''){
    df %<>% df[, c('investment',
                   series)]
  }
  # create lag
  if(lag != 0){
    df %<>% create_lag(lag)
  }
  
  df %<>% na.omit 
  # проверка на start и end
  
  if(startdt >= enddt){
    message('start must be greater then end')
    break()
  }
  startdt <- max(startdt %>% as.yearqtr,
                 first(time(df) %>% .[-(1:(1+h))]) %>% as.yearqtr)%>% as.Date
  
  enddt <- min(enddt %>% as.yearqtr,
               last(time(df) %>% .[-((nrow(df)-h+1):nrow(df))]) %>% as.yearqtr)%>% as.Date
  
  if(df[paste0(startdt, "/", enddt)] %>% nrow < 50){
    message('train set length must be greater then 50')
  }
  
  if(!is.integer(h)){
    message('h must be integer')
    break()
  }
  
  df$y <- lag.xts(df$investment, k = -h)
  df %<>% na.omit
  
  train_n <- which(time(df) %>% as.Date()==(startdt %>% as.yearqtr %>% as.Date)):
    which((time(df) %>% as.Date())== 
            (enddt %>% as.yearqtr%>% as.Date))
  
  test_n <- (which((time(df) %>% as.Date())==(enddt%>% as.yearqtr%>% as.Date))+1):nrow(df)
  
  
  X.matrix <- model.matrix(y~0+., data = df)
  
  X.train <- X.matrix[train_n,]
  
  X.test <- X.matrix[test_n,]
  
  y.train <- df$y[train_n] %>% as.numeric
  
  y.test <- df$y[test_n] %>% as.numeric
  
  tc <- trainControl(method = "timeslice", initialWindow = 40,horizon = 10,fixedWindow = TRUE)
  
  if(model == 'lasso'){
    train.out <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",trControl = tc,
                       tuneGrid = expand.grid(.alpha = c(1),.lambda = seq(0.05,0.0001,length = 300)))
    
    
    model_fit <- glmnet(X.train,
                        y.train,
                        alpha = 1,
                        lambda = train.out$bestTune[1,2])
    
    pred <- predict(model_fit,
                    newx = rbind(X.train, X.test)) %>% as.numeric
    
  } 
  else if(model == 'ridge'){
    train.out <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",trControl = tc,
                       tuneGrid = expand.grid(.alpha = 0,.lambda = seq(0.05,0.0001,length = 300)))
    model_fit <- glmnet(X.train,
                        y.train,
                        alpha = 0,
                        lambda = train.out$bestTune[1,2])
    
    
    
    pred <- predict(model_fit,newx = rbind(X.train, X.test)) %>% as.numeric
  } 
  else if (model == 'adalasso'){
    
    train.ridge <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",trControl = tc,
                         tuneGrid = expand.grid(.alpha = 0,.lambda = seq(0.05,0.0001,length = 300)))
    
    m_ridge <- glmnet(X.train,
                      y.train,
                      alpha = 0,
                      lambda = train.ridge$bestTune[1,2])
    
    train.out <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",trControl = tc,
                       tuneGrid = expand.grid(.alpha = 1,.lambda = seq(0.05,0.0001,length = 300)))
    
    w3 <- 1/abs(as.numeric(coef(m_ridge))
                [1:(ncol(X.train))] )^0.5 ## Using gamma = 1
    w3[which(w3 == Inf)] <- 999999999 ## Replacing values estimated as Infinite for 999999999
    
    model_fit <- glmnet(X.train,
                        y.train,
                        alpha = 1,
                        lambda = train.out$bestTune[1,2],
                        penalty.factor = w3)
    
    pred <- predict(model_fit,newx = rbind(X.train, X.test))
    
  } else if (model == 'postlasso'){
    
    train.out <- train(x=X.train,
                       y=y.train,
                       method = "glmnet",
                       metric = "RMSE",trControl = tc,
                       tuneGrid = expand.grid(.alpha = c(1),.lambda = seq(0.05,0.0001,length = 300)))
    
    
    model_lasso <- glmnet(X.train,
                          y.train,
                          alpha = 1,
                          lambda = train.out$bestTune[1,2])
    
    nzvars <- predict(model_lasso, type = "nonzero") %>% .[[1]]
    
    train_post <- as.data.frame(X.train) %>%
      select(nzvars) %>%
      mutate(y = y.train)
    test_post <-  as.data.frame(X.test) %>%
      select(nzvars) %>%
      mutate(y = 0)
    model_fit <- lm(y~., data = train_post)
    
    pred <- predict(model_fit,newdata = rbind(train_post,test_post ))
    
  } else if (model == 'rf'){
    
    tunegrid <- expand.grid(.mtry=seq(5,10))
    
    tc_rf <- trainControl(method='repeatedcv', 
                          number=10, 
                          repeats=3, 
                          search='grid')
    
    train.out <- train(x = X.train,
                       y = y.train,
                       method = "rf", 
                       metric = "RMSE",
                       trControl = tc_rf,
                       tuneGrid = tunegrid)
    
    bestmtry <- train.out$bestTune[1,1]
    
    model_fit <- randomForest(x = X.train, y = y.train, mtry = bestmtry)
    
    pred <-  predict(model_fit, newdata = rbind(X.train, X.test)) %>%
      as.numeric
    
  } else if (model =='ss'){
    
    train.out <- model_fit <- spikeslab(x = X.train, y = y.train, n.iter2 = 10000)
    
    pred <- predict(model_fit, newdata = rbind(X.train, X.test))$yhat.gnet
    
  } else if (model == 'arima'){
    if(h == 1){
      train.out <- model_fit <- Arima(y.train,order = c(3, 1, 3), fixed = c(NA, NA, NA, NA, NA, NA))
    } else if (h == 2){
      train.out <- model_fit <- Arima(y.train,order = c(4, 1, 4), fixed = c(0, NA, NA, NA, 0, NA, NA, NA,))
    } else if (h == 3){
      train.out <- model_fit <- Arima(y.train,order = c(5, 1, 5), fixed = c(0, 0, NA, NA, NA, 0, 0, NA, NA, NA))
    } else if (h == 4){
      train.out <- model_fit <- Arima(y.train,order = c(6, 1, 6), fixed = c(0, 0, 0,NA, NA, NA, 0, 0, 0, NA, NA, NA))
    }
    
    pred <- Arima(c(y.train, y.test), model=model_fit) %>% fitted() %>% as.numeric()
    
  }
  list(
    model = model,
    series = series,
    lag = lag,
    startdt = startdt,
    enddt = enddt,
    date = time(df)[c(train_n,test_n)] %>% as.Date,
    h = h,
    pred = pred,
    train.out = train.out,
    model_fit = model_fit
  )
  
}

out <- expand.grid(startdt = seq(as.Date('1992-01-01'), as.Date('1999-01-01'), by = 'quarter'),
            enddt = as.Date('2011-12-01'), model = c('lasso',
                                                                    'ridge',
                                                                    'adalasso',
                                                                    'postlasso',
                                                                    'rf',
                                                                    'ss',
                                                                    'arima'),
            lag = c(1:4),
            h=c(1L:4L)
            ) %>%
  split(seq(1:nrow(.))) %>%
  map(function(x){
    train.model(startdt=x$startdt,
                enddt=x$enddt,
                model = x$model,
                lag=x$lag,
                h=x$h
                )
  })

save(out, file = 'data/out.RData')
