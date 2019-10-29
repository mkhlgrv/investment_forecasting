do.call.pipe <- function(args,what){
  do.call(what,args)
}
create_lag <- function(df, lag){
  out <- df
  if(lag > 0){
    for(i in 1:lag){
      x <- lag.xts(df, k = i)
      colnames(x) <- paste0(colnames(x), "_lag", i)
      out %<>% merge.zoo(x)
    }
  }
  
  return(out %>% as.xts)
}
 
# 1. начальная дата
# 2. конечная дата
# 3. модель
# 4. набор переменных (только из тех, что указаны в calibrating)
# 5. на сколько периодов вперед прогнозировать (max)

train.model <- function(startdt= as.Date('2000-01-01'),
                        enddt = as.Date('2012-01-01'),
                        model,
                        # series parameter 
                        # only for regularisation and machine learning models
                        series='',
                        lag = 2L,
                        h = 4L,
                        # parameters for durable evaluations with function arguments from expand.grid table
                        i = NULL, 
                        N = NULL
                        ){
  message(paste0(i, '/', N))
  # import df
  if(series == ''){
    load('~/investment_forecasting/data/stationary_data.RData')
  } else if(series == 'e') {
    load('~/investment_forecasting/data/stationary_data_ext.RData')
  }
  
  if(!model %in% c('arima', 'rw')){
    if(!series %in% c('', 'e')){
      df %<>% df[, c('investment',
                     series)]
    }
    # create lag
    if(lag != 0 |! model %in% c('arima', 'rw')){
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
    
    df %<>% .[rowSums(is.na(.[,colnames(.)!='y']))==0,]
    
    
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
    
    if(which((time(df) %>% as.Date())==(enddt%>% as.yearqtr%>% as.Date))+1>length(df$y %>% na.omit)){
      #message('enddt must not be greater then last date when investment data is avaliable minus h quarters')
      return(NULL)
    }
    
    train_n <- which(time(df) %>% as.Date()==(startdt %>% as.yearqtr %>% as.Date)):
      which((time(df) %>% as.Date())== 
              (enddt %>% as.yearqtr%>% as.Date))
    
    test_n <- (which((time(df) %>% as.Date())==(enddt%>% as.yearqtr%>% as.Date))+1):nrow(df)
    
    
    df$y %<>% na.fill(0)

    
    X.matrix <- model.matrix(y~0+., data = df)
    
    X.train <- X.matrix[train_n,]
    
    X.test <- X.matrix[test_n,]
    
    y.train <- df$y[train_n] %>% as.numeric
    
    y.test <- df$y[test_n] %>% as.numeric
    
    
    
    
    tc <- trainControl(method = 'cv', number = 10)
      
      
      # trainControl(method = "timeslice",
      #                  initialWindow = 40,
      #                  horizon = 8,
      #                  fixedWindow = TRUE)
    
    if(model == 'lasso'){
      train.out <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",
                         trControl = tc,
                         tuneGrid =
                           expand.grid(.alpha = c(1),
                                       .lambda = seq(0.05,0.0001,length = 100)))
      
      
      model_fit <- glmnet(X.train,
                          y.train,
                          alpha = 1,
                          lambda = train.out$bestTune[1,2])
      
      pred <- predict(model_fit,
                      newx = rbind(X.train, X.test)) %>% as.numeric
      
    } 
    
    else if(model == 'elnet'){
      train.out <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",
                         trControl = tc,
                         tuneGrid =
                           expand.grid(.alpha = c(0.5),
                                       .lambda = seq(0.05,0.0001,length = 100)))
      
      
      model_fit <- glmnet(X.train,
                          y.train,
                          alpha = 0.5,
                          lambda = train.out$bestTune[1,2])
      
      pred <- predict(model_fit,
                      newx = rbind(X.train, X.test)) %>% as.numeric
      
    } 
    else if(model == 'ridge'){
      train.out <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",trControl = tc,
                         tuneGrid = expand.grid(.alpha = 0,.lambda = seq(0.05,0.0001,length = 100)))
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
                           tuneGrid = expand.grid(.alpha = 0,.lambda = seq(0.05,0.0001,length = 100)))
      
      m_ridge <- glmnet(X.train,
                        y.train,
                        alpha = 0,
                        lambda = train.ridge$bestTune[1,2])
      
      train.out <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",trControl = tc,
                         tuneGrid = expand.grid(.alpha = 1,.lambda = seq(0.05,0.0001,length = 100)))
      
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
                         tuneGrid = expand.grid(.alpha = c(1),.lambda = seq(0.05,0.0001,length = 100)))
      
      
      model_lasso <- glmnet(X.train,
                            y.train,
                            alpha = 1,
                            lambda = train.out$bestTune[1,2])
      
      
      
      nzvars <- predict(model_lasso, type = "nonzero") %>% .[[1]]
      
      
      
      train_post <- as.data.frame(X.train) %>%
        select(nzvars) %>%
        mutate(y = y.train)
      
      
      
      if(class(X.test)=='numeric'){
        X.test %<>% as.tibble %>% t %>% set_colnames(colnames(X.train))
      }
      
      test_post <-  as.data.frame(X.test) %>%
        select(nzvars) %>%
        mutate(y = 0)
      
      
      model_fit <- lm(y~., data = train_post)
      
      pred <- predict(model_fit,newdata = rbind(train_post,test_post ))
      
    } else if (model == 'rf'){
      
      
      
      train.out <- NULL
      model_fit <- randomForest(x = X.train, y = y.train, ntree = 100)
      
      pred <-  predict(model_fit, newdata = rbind(X.train, X.test)) %>%
        as.numeric
      
    } else if (model =='ss'){
      
      train.out <- NULL
      
      model_fit <- spikeslab(x = X.train,
                             y = y.train,
                             n.iter2 = 1000,
                             bigp.smalln = ncol(X.train)>=nrow(X.train),
                             intercept = TRUE)
      
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
      train.out <- NULL
      model_fit <- auto.arima(y.train,seasonal = FALSE, stationary = TRUE) 
      
      
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
  df %>% filter(enddt <= '2016-10-01',
                date > enddt,
                date <= as.Date(enddt + 366*2)) %>%
    summarise(rmspe = RMSPE(pred, true),
              rmse = RMSE(pred, true),
              rrse = RRSE(pred, true),
              mae= MAE(pred, true),
              r2 = R2_Score(pred, true),
              type = 'test')
  ) %>% ungroup
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


sliderValues <- function (inputId,
                          label,
                          values,
                          from,
                          to = NULL,
                          grid = TRUE,
                          width = NULL,
                          postfix = NULL,
                          prefix = NULL,
                          dragRange = TRUE,
                          disable = FALSE,
                          animate = FALSE) {
  validate_fromto <-
    function(fromto = NULL,
             values = NULL,
             default = 0) {
      if (!is.null(fromto)) {
        if (is.character(values) & is.numeric(fromto)) {
          fromto <- fromto - 1
        } else {
          fromto <- which(values == fromto) - 1
        }
      } else {
        fromto <- default
      }
      return(fromto)
    }
  
  sliderProps <- shiny:::dropNulls(
    list(
      class = "js-range-slider",
      id = inputId,
      `data-type` = if (!is.null(to))
        "double"
      else
        "single",
      `data-from` = validate_fromto(fromto = from, values = values),
      `data-to` = validate_fromto(
        fromto = to,
        values = values,
        default = length(values)
      ),
      `data-grid` = grid,
      `data-prefix` = if (is.null(prefix)) {
        "null"
      } else {
        shQuote(prefix, "sh")
      },
      `data-postfix` = if (is.null(postfix)) {
        "null"
      } else {
        shQuote(postfix, "sh")
      },
      `data-drag-interval` = dragRange,
      `data-disable` = disable,
      `data-values` = if (is.numeric(values)) {
        paste(values, collapse = ", ")
      } else {
        paste(shQuote(values, type = "sh"), collapse = ", ")
      }
    )
  )
  sliderProps <- lapply(
    X = sliderProps,
    FUN = function(x) {
      if (identical(x, TRUE))
        "true"
      else if (identical(x, FALSE))
        "false"
      else
        x
    }
  )
  sliderTag <- tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width))
      paste0("width: ", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label))
      shiny:::controlLabel(inputId, label),
    do.call(
      tags$input,
      list(
        type = if (is.numeric(values) &
                   is.null(to)) {
          "number"
        } else {
          "text"
        },
        #class = "js-range-slider",
        id = inputId,
        name = inputId,
        value = ""
      )
    ),
    tags$style(
      whisker::whisker.render(
        template =
          "input[id='{{id}}'] {
        -moz-appearance:textfield;
}
input[id='{{id}}']::-webkit-outer-spin-button,
input[id='{{id}}']::-webkit-inner-spin-button {
-webkit-appearance: none;
margin: 0;
}", data = list(id = inputId))
    ),
    tags$script(
      HTML(
        whisker::whisker.render(
          template = '$("#{{id}}").ionRangeSlider({
          type: "{{data-type}}",
          from: {{data-from}},
          to: {{data-to}},
          grid: {{data-grid}},
          keyboard: true,
          keyboard_step: 1,
          postfix: {{data-postfix}},
          prefix: {{data-prefix}},
          drag_interval: {{data-drag-interval}},
          values: [{{data-values}}],
          disable: {{data-disable}}
          });',
          data = sliderProps
        )
      ))
  )
  if (identical(animate, TRUE)) 
    animate <- animationOptions()
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- icon("play", lib = "glyphicon")
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- icon("pause", lib = "glyphicon")
    sliderTag <- htmltools::tagAppendChild(
      sliderTag,
      tags$div(class = "slider-animate-container", 
               tags$a(href = "#", class = "slider-animate-button", 
                      `data-target-id` = inputId, `data-interval` = animate$interval, 
                      `data-loop` = animate$loop, span(class = "play", 
                                                       animate$playButton), 
                      span(class = "pause", 
                           animate$pauseButton)))
    )
  }
  dep <- htmltools::htmlDependency(
    "ionrangeslider",
    "2.1.12",
    c(href = "shared/ionrangeslider"),
    script = "js/ion.rangeSlider.min.js",
    stylesheet = c(
      "css/ion.rangeSlider.css",
      "css/ion.rangeSlider.skinShiny.css"
    )
  )
  htmltools::attachDependencies(sliderTag, dep)
}


correct.names <- Vectorize(vectorize.args = "model",
                           FUN = function(model) {
                             switch(as.character(model),
                                    'lasso' = 'LASSO',
                                    'postlasso' = 'Post-LASSO',
                                    'adalasso' = 'Adaptive LASSO',
                                    'ridge' = 'Ridge',
                                    'elnet' = 'Elastic Net',
                                    'rf' = 'Random Forest',
                                    'ss' = 'Spike-and-Slab',
                                    'arima' = 'AR',
                                    'rw' = 'Random Walk')})

# округление до 4 знаков и умножение на 100
meanroundpercent <- function(x){
  (x %>% mean %>% round(4))*100
}

get.dm <- function(df){
    df %>%
  filter(enddt <= '2016-10-01',
         date > enddt,
         date <= as.Date(enddt + 366*2)) %>%
            mutate(error = pred-true) %>%
    dcast(model + enddt + h + lag + date ~ startdt, value.var = 'error') %>%
    group_by(model, h, lag) %>%
    summarise(pvalue = ifelse(all(`1997-01-01`==`2000-01-01`), 1, 
                              (forecast::dm.test(
                                                 `2000-01-01`,
                                                 `1997-01-01`) %>% .$p.value)),
              stat = ifelse(all(`1997-01-01`==`2000-01-01`), 1, 
                                 (forecast::dm.test(
                                   `2000-01-01`,
                                   `1997-01-01`) %>% .$statistic))
              
              ) %>%
    ungroup()
    
  }


