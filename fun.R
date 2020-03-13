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

train.model <- function(startdt= as.Date('1996-01-01'),
                        enddt = as.Date('2016-10-01'),
                        model,
                        # series parameter 
                        # only for regularisation and machine learning models
                        series='e',
                        lag = 0L,
                        h = 1L,
                        target = c('investment','RTRD_Q_DIRI','GDPEA_Q_DIRI','UNEMPL_Q_SH', 'CPI_Q_CHI'),
                        # parameters for durable evaluations with function arguments from expand.grid table
                        i = NULL, 
                        N = NULL,
                        ntree = NULL,
                        eta = NULL
                        ){
  message(paste0(i, '/', N))
  # import df
  # seed
  set.seed(2019)
  
  
  if(series == ''){
    
    load('~/investment_forecasting/data/stationary_data.RData')
    
  } else if(series == 'e') {
    
    load('~/investment_forecasting/data/stationary_data_ext.RData')
    
  } else if(series == 'oil'){
    load('~/investment_forecasting/data/stationary_data_oil.RData')
  }
  
  print(target)
  target <- match.arg(target)
  
  if(!model %in% c('arima', 'rw')){
    if(!series %in% c('', 'e', 'oil')){
      df %<>% df[, c(target,
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
    
    df$y <- lag.xts(df[,target], k = -h)
    
    if(h == 0){
      if(target == 'investment'){
        df$investment <-df$invest2gdp <- df$GDPEA_Q_DIRI <-  NULL
      } else{
        df %<>% .[, colnames(.) != target]
      }
    } else{
      df$gdplag <- df$investmentlag <- df$invest2gdplag <- NULL
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
    
    
    
    
    # tc <- trainControl(method = 'cv', number = 10)
      
    tc <- trainControl(method = "timeslice",
                       initialWindow = 40,
                       horizon = 1,
                       skip=0,
                       fixedWindow = TRUE)
    
    lambda <- seq(-8,-1,length = 200) %>% exp()
    
    if(model == 'lasso'){
      train.out <- train(x=X.train,
                         y=y.train,
                         method = "glmnet",
                         metric = "RMSE",
                         trControl = tc,
                         tuneGrid =
                           expand.grid(.alpha = c(1),
                                       .lambda = lambda))
      
      
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
                                       # new
                                       .lambda = lambda))
      
      
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
                         tuneGrid = expand.grid(.alpha = 0,
                                                .lambda = lambda))
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
                           tuneGrid = expand.grid(.alpha = 0,.lambda = lambda))
      
      m_ridge <- glmnet(X.train,
                        y.train,
                        alpha = 0,
                        lambda = train.ridge$bestTune[1,2])
      
      w3 <- 1/abs(as.numeric(coef(m_ridge))
                  [1:(ncol(X.train))] )^0.5 ## Using gamma = 1
      w3[which(w3 == Inf)] <- 999999999 ## Replacing values estimated as Infinite for 999999999
      
      train.out <- train(x=X.train,
                         y=y.train,
                         penalty.factor = w3,
                         method = "glmnet",
                         metric = "RMSE",
                         trControl = tc,
                         tuneGrid = expand.grid(.alpha = 1,.lambda = lambda))
      

      
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
                         tuneGrid = expand.grid(.alpha = c(1),.lambda = lambda))
      
      
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
      model_fit <- randomForest(x = X.train,
                                y = y.train,
                                ntree = ntree,
                                nodesize = 5)
      
      pred <-  predict(model_fit, newdata = rbind(X.train, X.test)) %>%
        as.numeric
      
    } else if (model =='ss'){
      
      train.out <- NULL
      
      model_fit <- spikeslab(x = X.train,
                             y = y.train,
                             n.iter2 = 500,
                             bigp.smalln = ncol(X.train)>=nrow(X.train),
                             intercept = TRUE)
      
      pred <- predict(model_fit, newdata = rbind(X.train, X.test))$yhat.gnet
      
    } else if (model == 'boost'){
      
      print(eta)
      tune_grid <- expand.grid(nrounds = 100,
                               max_depth = c(5),
                               eta = eta,
                               gamma = 0,
                               colsample_bytree = 0.3,
                               min_child_weight = 1,
                               subsample = 1)
      
      
      train.out <- NULL
      model_fit <- train(x = X.train,
                         y = y.train,
                         method = "xgbTree",
                         metric = "RMSE",
                         tuneGrid = tune_grid)
      
      pred <- predict(model_fit, newdata = rbind(X.train, X.test)) %>%
        as.numeric
      
    }
  } else 
    {
    
      
      if(!is.integer(h)){
        message('h must be integer')
        return(NULL)
      }
      
      df %<>% .[,target]
      
      df$y <- df[,target]
      
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
      if(h == 0){
        pred <- lag.xts(y.full)
      } else{
        pred <- y.full
      }
      train.out <- model_fit <- NULL
    }
  }
  

  list(
    model = model,
    target = target,
    eta = eta,
    ntree = ntree,
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
    group_by(model, target, ntree, eta,
             startdt, enddt, lag, h)
  rbind(df %>% filter(date <= enddt) %>%
          summarise(rmspe = RMSPE(pred, true),
                    rmse = RMSE(pred, true),
                    rrse = RRSE(pred, true),
                    mae= MAE(pred, true),
                    r2 = R2_Score(pred, true),
                    type = 'train'),
        df %>% filter(enddt <= as.Date(as.yearqtr(as.Date('2018-10-01'))-h/4),
                      date > as.Date(as.yearqtr( enddt)+h/4),
                      date <= as.Date(as.yearqtr( enddt)+(h+1)/4)) %>%
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
                                    'rf' = 'Случайный лес (N = 100)',
                                    'rf_100' = 'Случайный лес (N = 100)',
                                    'rf_500' = 'Случайный лес (N = 500)',
                                    'rf_1000' = 'Случайный лес (N = 1000)',
                                    'rf_2000' = 'Случайный лес (N = 2000)',
                                    
                                    'ss' = 'Spike and Slab',
                                    'arima' = 'AR',
                                    'rw' = 'Случайное блуждание',
                                    
                                    'boost' = 'Бустинг (eta = 0,3)',
                                    'boost_100' = 'Бустинг (eta = 0,3)',
                                    
                                    'boost_100_1' = 'Бустинг (eta = 0,1)',
                                    
                                    'boost_100_2' = 'Бустинг (eta = 0,2)',
                                    'boost_100_4' = 'Бустинг (eta = 0,4)',
                                    )})


correct.names.pred <- Vectorize(vectorize.args = "model",
                                FUN = function(model) {
                                  switch(as.character(model),
                                         'investment' = 'Валовое накопление основного капитала',
                                         'mkr_1d' = 'Ставка межбанковского рынка, 1 день (в среднем за квартал)',
                                         'mkr_7d' = 'Ставка межбанковского рынка, 7 дней (в среднем за квартал)',
                                         'gov_6m' = 'Доходность 6-месячных государственных облигаций (в среднем за квартал)',
                                         'reer' = 'Реальный эффективный обменный курс (на конец квартала)',
                                         'neer' = 'Номинальный эффективный обменный курс (на конец квартала)',
                                         'oil' ='Цена нефти Brent (на конец квартала)',
                                         'rts' = 'Индекс RTS/Московской биржи (на конец квартала)',
                                         'CPI_Q_CHI' = 'Индекс потребительских цен',
                                         'GDPEA_Q_DIRI' = 'ВВП в постоянных ценах',
                                         'EMPLDEC_Q' = 'Заявленная потребность в работниках (в среднем за квартал)',
                                         'UNEMPL_Q_SH' ='Норма безработицы (в среднем за квартал)',
                                         'CONSTR_Q_NAT' = 'Ввод в действие жилых домов',
                                         'WAG_Q' ='Индекс реальной зарплаты',
                                         'CONI_Q_CHI'='Индекс цен на строительно-монтажные работы',
                                         'CTI_Q_CHI'='Индекс тарифов на грузовые перевозки ',
                                         'AGR_Q_DIRI'='Индекс реального объема сельскохозяйственного производства',
                                         'RTRD_Q_DIRI'='Индекс реального оборота розничной торговли',
                                         'HHI_Q_DIRI'='Индекс реальных денежных доходов населения ',
                                         'M0_Q'='М0 (на конец квартала)',
                                         'M2_Q'='М2 (на конец квартала)',
                                         'CBREV_Q'='Доходы консолидированного бюджета',
                                         'CBEX_Q'='Расходы консолидированного бюджета ',
                                         'FBREV_Q'='Доходы федерального бюджета',
                                         'FBEX_Q'='Расходы федерального бюджета',
                                         'RDEXRO_Q'='Официальный курс доллара (на конец квартала)',
                                         'RDEXRM_Q'='Курс доллара на ММВБ (на конец квартала) ',
                                         'LIAB_T_Q'='Кредиторская задолженность (в среднем за квартал)',
                                         'LIAB_UNP_Q'='Просроченная кредиторская задолженность (в среднем за квартал)',
                                         'LIAB_S_Q'='Задолженность поставщикам (в среднем за квартал)',
                                         'LIAB_B_Q'='Задолженность в бюджет (в среднем за квартал)',
                                         'DBT_T_Q'='Дебиторская задолженность (в среднем за квартал)',
                                         'DBT_UNP_Q'='Просроченная дебиторская задолженность (в среднем за квартал)',
                                         'EX_T_Q'='Экспорт', 
                                         'IM_T_Q'='Импорт',
                                         'PPI_EA_Q'='Индекс цен производителей промышленных товаров',
                                         'invest2gdp'='Доля валового накопления основного капитала в ВВП (номинал)',
                                         ###########
                                         'gdplag' = 'ВВП в постоянных ценах, лаг',
                                         'investmentlag' = 'Валовое накопление основного капитала, лаг',
                                         'invest2gdplag' = 'Доля валового накопления основного капитала в ВВП (номинал), лаг'
                                         )})




# округление до 4 знаков и умножение на 100
meanroundpercent <- function(x){
  (x %>% mean %>% round(4))*100
}

get.dm <- function(df){
  df %>%
    filter(enddt <= as.Date('2016-10-01'),
           date > as.Date(as.yearqtr( enddt)+h/4),
           date <= as.Date(as.yearqtr( enddt)+(h+1)/4)) %>%
    
    dcast(model + h + lag +true+date~ startdt, value.var = 'pred') %>%
    group_by(model, h, lag) %>%
    summarise(pvalue = ifelse(all(`1996-01-01`==`2000-01-01`), 1,
                              (DM.test(`2000-01-01`,
                                       `1996-01-01`,
                                       y = true,
                                       c=TRUE,
                                       H1 =ifelse(
                                         mean((`2000-01-01`-true)^2) < 
                                           mean((`1996-01-01`-true)^2),
                                         'more',
                                         ifelse(mean((`2000-01-01`-true)^2) > 
                                                  mean((`1996-01-01`-true)^2),
                                                'less', 'same')
                                       )) %>% .$p.value)),
              stat = ifelse(all(`1996-01-01`==`2000-01-01`), 0,
                            (DM.test(`2000-01-01`,
                                     `1996-01-01`,
                                     y = true,
                                     c=TRUE,
                                     H1 =ifelse(
                                       mean((`2000-01-01`-true)^2) < 
                                         mean((`1996-01-01`-true)^2),
                                       'more',
                                       ifelse(mean((`2000-01-01`-true)^2) > 
                                                mean((`1996-01-01`-true)^2),
                                              'less', 'same')
                                     )) %>% .$statistic))
              
    ) %>%
    ungroup()
  
}

get.dm.lag <- function(df){
  df %>%
    filter(enddt <= as.Date(as.yearqtr(as.Date('2018-10-01'))-h/4),
           date > as.Date(as.yearqtr( enddt)+h/4),
           date <= as.Date(as.yearqtr( enddt)+(h+1)/4)) %>%
    mutate(error = pred-true) %>%
    group_by(model, lag, startdt, h) %>%
    mutate(e_m = mean(error^2)) %>%
    ungroup %>%
    mutate(group = paste0(model, startdt, h)) %>%
    split(.$group) %>%
    map_dfr(function(x){
      true <- x$true[which(x$e_m == min(x$e_m))]
      bench <- x$pred[which(x$e_m == min(x$e_m))]
      x %<>% dcast(model+startdt+ h+ date~lag, value.var='pred')
      pvalue <- c()
      for(i in 5:9){
        actualx <- x %>% as.data.frame %>% .[,i]
        pvalue <- c(pvalue, ifelse(all(actualx== bench), 1,
                                   DM.test(bench,actualx,
                                           true,c = TRUE,
                                           
                                           H1 = 'more'
                                   ) %>% .$p.value
                                   
        ))
        
        
      }
      data.frame(model=x$model %>% first,
                 startdt = x$startdt %>% first,
                 h = x$h %>% first,
                 lag = 0:4,
                 pvalue = pvalue)
    })
}