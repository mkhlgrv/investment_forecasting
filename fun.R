do.call.pipe <- function(args,what){
  do.call(what,args)
}
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
  if(!model %in% c('arima', 'rw')){
    if(series!=''){
      df %<>% df[, c('investment',
                     series)]
    }
    # create lag
    if(lag != 0){
      df %<>% create_lag(lag)
    }
    
    if(!is.integer(h)){
      message('h must be integer')
      return(NULL)
    }
    
    df$y <- lag.xts(df$investment, k = -h)
    
    if(h == 0){
      df$investment <- NULL
    }
    
    df %<>% na.omit 
    
    # проверка на start и end
    
    if(startdt >= enddt){
      message('start must be greater then end')
      return(NULL)
    }
    
    startdt <- max(startdt %>% as.yearqtr,
                   first(time(df)) %>% as.yearqtr) %>%
      as.Date
    
    
    enddt <- min(enddt %>% as.yearqtr,
                 last(time(df)) %>% as.yearqtr) %>%
      as.Date
    
    
    if(df[paste0(startdt, "/", enddt)] %>% nrow < 48){
      message('train set length must be greater then 48 quarters')
      return(NULL)
    }
    
    
    
    train_n <- which(time(df) %>% as.Date()==(startdt %>% as.yearqtr %>% as.Date)):
      which((time(df) %>% as.Date())== 
              (enddt %>% as.yearqtr%>% as.Date))
    
    test_n <- (which((time(df) %>% as.Date())==(enddt%>% as.yearqtr%>% as.Date))+1):nrow(df)
    
    
    X.matrix <- model.matrix(y~0+., data = df)
    
    X.train <- X.matrix[train_n,]
    
    X.test <- X.matrix[test_n,]
    
    y.train <- df$y[train_n] %>% as.numeric
    
    y.test <- df$y[test_n] %>% as.numeric
    
    tc <- trainControl(method = "timeslice", initialWindow = 40,horizon = 8,fixedWindow = TRUE)
    
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
      
      pred <- predict(model_fit,newx = rbind(X.train, X.test)) %>%
        as.numeric
      
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
      
    }
  } else {
    
      
      if(!is.integer(h)){
        message('h must be integer')
        return(NULL)
      }
      
      df %<>% .[,'investment']
      
      df$y <- df$investment
      
      df %<>% na.omit 
      
      # проверка на start и end
      
      if(startdt >= enddt){
        message('start must be greater then end')
        return(NULL)
      }
      
      startdt <- max(startdt %>% as.yearqtr,
                     first(time(df)) %>% as.yearqtr) %>%
        as.Date
      
      
      enddt <- min(enddt %>% as.yearqtr,
                   last(time(df)) %>% as.yearqtr) %>%
        as.Date
      
      
      if(df[paste0(startdt, "/", enddt)] %>% nrow < 48){
        message('train set length must be greater then 48 quarters')
        return(NULL)
      }
      
      
      
      train_n <- which(time(df) %>% as.Date()==(startdt %>% as.yearqtr %>% as.Date)):
        which((time(df) %>% as.Date())== 
                (enddt %>% as.yearqtr%>% as.Date))
      
      test_n <- (which((time(df) %>% as.Date())==(enddt%>% as.yearqtr%>% as.Date))+1):nrow(df)
    
      y.train <- df$y[train_n] %>% as.numeric
      y.test <- df$y[test_n] %>% as.numeric
      y.full <- c(y.train, y.test)
      
      if (model == 'arima'){
      train.out <- model_fit <- auto.arima(y.train,seasonal = FALSE, stationary = TRUE) 
      
      maxord <- arimaorder(model_fit) %>% sum
      
      if(h == 0){
        pred <- Arima(y.full, model=model_fit) %>% fitted() %>% as.numeric()
      } else {
        if(maxord == 1){
          pred <- c()
        } else{
          pred <- rep(NA, maxord-1)
        }
        for(i in maxord:length(y.full)){
          fit <- Arima(y.full[1:i],
                        model=model_fit)
          pred <- c(pred, forecast(fit, h) %>% as.data.frame %>% .[h,1])
        }
      }
      
    } else if (model == 'rw'){
      pred <- y.full
      train.out <- model_fit <- NULL
    }
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

transform_to_score <- function(df){
  df %>%
    split(.$model) %>%
    map(function(x){
      x %>%  print()
      
    })
    
}

get.score <- function(df){
  df %<>%
    group_by(model, startdt, enddt, lag, h)
  rbind(df %>% filter(date <= enddt) %>%
    summarise(rmspe = RMSPE(pred, true),
              rmse = RMSE(pred, true),
              rrse = RRSE(pred, true),
              mae= MAE(pred, true),
              r2 = R2_Score(pred, true),
              type = 'train'),
  df %>% filter(date > enddt) %>%
    summarise(rmspe = RMSPE(pred, true),
              rmse = RMSE(pred, true),
              rrse = RRSE(pred, true),
              mae= MAE(pred, true),
              r2 = R2_Score(pred, true),
              type = 'test')
  ) %>% ungroup
}


normal.score <- function(df)

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

