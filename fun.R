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
                        tuneGrid = expand.grid(.alpha = alpha,.lambda = seq(0.1,0.00001,length = 1000)))
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