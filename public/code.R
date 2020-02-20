
library(wbstats)
library(dplyr)
library(data.table)
library(rio)
library(stringr)
library(magrittr)
library(purrr)
library(ggplot2)
library(latex2exp)
library(stargazer)
library(glmnet)
library(corrplot)
library(dplyr) 
library(lubridate) 
library(rio) 
library(stringr) 
library(tibble) 
library(magrittr) 
library(purrr) 
library(data.table) 
library(tidyr) 
library(forecast) 
library(gridExtra) 
library(grid)
library(Amelia) 
library(fastDummies) 
library(gnm) 
library(parallel) 
library(xgboost) 
library(rbenchmark)
#library(plotly)

library(e1071) 
library(rlang)
library(caret) 
library(DiagrammeR) 
library(sophisthse)
library(DiagrammeRsvg) 
library(rsvg) 
library(neuralnet) 
library(ggridges)
library(tseries)
library(xts) 
library(DtD) 
library(readr) 
library(TTR) 
library(zoo) 
library(seqHMM) 
library(hexbin)
library(sophisthse)
library(tseries)
library(caret)
library(sophisthse)
library(tseries)
library(MLmetrics)
library(caret)
library(xts)
library(randomForest)
library(spikeslab)
library(BigVAR)
library(xtable)
library(readxl)
library(httr)
library(ggpubr)
library(shiny)
library(scales)
library(DT)
library(lemon)
library(shinyWidgets)
library(dplyr)
library(multDM)
library(tables)
library(xtable)
library(animation)
library(gganimate)
library(multDM)

# import rawdata
load('raw.Rdata')

# # доходность ртс, процентные ставки на межбанковском рынке, спреды по облигациям,
# темпы роста номинального эффективного курса, реального эффективного курса, прирост цен на нефть,
# 4 разность логарифма ВВП и ИПЦ, отношение номинальных инвестиций к номинальному ввп + лаги
# extended series ----

eseries <- c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO',
             'reer', 'neer', 'oil', 'rts',
             
             'GDPEA_C_Q', 'INVFC_Q', # служебные для invest2gdp
             
             
             'CPI_Q_CHI',# ипц
             'GDPEA_Q_DIRI', # ввп
             
             
             'EMPLDEC_Q', # заявленная потребность в работника
             'UNEMPL_Q_SH',# безработица
             'CONSTR_Q_NAT', # индекс строительно-монтажных работ, 
             'WAG_Q', # зарплата
             'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
             'CTI_Q_CHI', # индекс тарифов на грузовые перевозки
             'AGR_Q_DIRI', # индекс сельхоз производства
             #'CNSTR_Q_DIRI',# индекс работ в строительстве
             'RTRD_Q_DIRI', # оборот розничной торговли
             'HHI_Q_DIRI',# индекс реальных денежных доходов населения
             'M0_Q', # M0
             'M2_Q',# М2
             'CBREV_Q',# доходы конс. бюджета 
             'CBEX_Q',# расходы конс. бюджета
             'FBREV_Q',# доходы фед. бюджета
             'FBEX_Q',# расходы фед. бюджета
             'RDEXRO_Q',# официальный курс доллара
             'RDEXRM_Q',# курс доллара на ммвб
             'LIAB_T_Q',# кредиторская задолженность в среднем за период
             'LIAB_UNP_Q',# просроченная кредиторская задолженность в среднем за период
             'LIAB_S_Q',# кредиторская задолженность поставщикам в среднем за период
             'LIAB_B_Q',# кредиторская задолженность в бюджет в среднем за период
             'DBT_T_Q',#дебиторская задолженность в среднем за период
             'DBT_UNP_Q',#просроченная дебиторская задолженность в среднем за период
             # 'DBT_P_Q',# дебиторская задолженность покупателей в среднем за период
             'EX_T_Q',# экспорт
             'IM_T_Q',# импорт
             'PPI_EA_Q' # (после 2004-01)
)


df <- rawdata[,eseries]
# отношение номинальных инвестиций к номинальному ВВП
df$invest2gdp <- df$INVFC_Q/df$GDPEA_C_Q

# удалить лишнее
df$GDPEA_C_Q <- df$INVFC_Q <- NULL

# вставить gko в 6m gov bond yield
df$gov_6m[which(is.na(df$gov_6m))] <- 
  df$GKO[which(is.na(df$gov_6m))]

df$GKO <- NULL

# нестационарные ряды без сезонности (разность) ----
for(i in c('reer','neer','oil','rts')){
  df[,i] %<>% diff.xts(log=TRUE)
}

# нестационарные ряды с сезонностью (4-ая разность) ----
for(i in c('investment', 'CPI_Q_CHI',
           'invest2gdp',
           # 'deflator', только с 1996
           'GDPEA_Q_DIRI',
           
           'EMPLDEC_Q',
           'UNEMPL_Q_SH',
           
           'CONSTR_Q_NAT', 
           ###### 'TRP_Q_PASS_DIRI', 
           'WAG_Q', 
           'CONI_Q_CHI', 
           'CTI_Q_CHI', 
           'AGR_Q_DIRI', 
           'RTRD_Q_DIRI', 
           'HHI_Q_DIRI',
           'M0_Q', 
           'M2_Q',
           ####   'IR_Q',
           #### 'ICR_Q',
           'CBREV_Q',
           'CBEX_Q',
           'FBREV_Q',
           'FBEX_Q',
           'RDEXRO_Q',# официальный курс доллара
           'RDEXRM_Q',# курс доллара на ммвб
           'LIAB_T_Q',# кредиторская задолженность в среднем за период
           'LIAB_UNP_Q',# просроченная кредиторская задолженность в среднем за период
           'LIAB_S_Q',# кредиторская задолженность поставщикам в среднем за период
           'LIAB_B_Q',# кредиторская задолженность в бюджет в среднем за период
           'DBT_T_Q',#дебиторская задолженность в среднем за период
           'DBT_UNP_Q',#просроченная дебиторская задолженность в среднем за период
           ########## 'DBT_P_Q',# дебиторская задолженность покупателей в среднем за период
           'EX_T_Q',# экспорт
           'IM_T_Q',# импорт
           'PPI_EA_Q' # (после 2004-01)
           
           
           
)){
  df[,i] %<>% diff.xts(lag = 4, log=TRUE)
}



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
                        N = NULL
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
                                ntree = 100,
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
      
      tune_grid <- expand.grid(nrounds = 100,
                               max_depth = c(5),
                               eta = c(0.3),
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


out <- expand.grid(startdt = c(as.Date('1996-01-01'), as.Date('2000-01-01')),
                         enddt = seq(as.Date('2012-10-01'), as.Date('2030-10-01'), by = 'quarter'),
                         lag = c(0L),
                         h=c(1L:8L), 
                         model = c('arima',
                                   'boost',
                                   'adalasso',
                                   'postlasso',
                                   'lasso',
                                   'elnet',
                                   'rf',
                                   'rw',
                                   'ss',
                                   'ridge'
                                   )
) %T>% 
  (function(x) {assign('N', nrow(x), envir = globalenv())}) %>%
  split(seq(1:nrow(.))) %>%
  
  imap(function(x, i){
    train.model(startdt=x$startdt,
                enddt=x$enddt,
                model = x$model,
                lag=x$lag,
                h=x$h,
                i = i,
                N = N
    )
  })

%>%
  map_dfr(function(x){
    data.frame(model=x$model,
               lag = x$lag,
               startdt= x$startdt,
               enddt= x$enddt,
               h = x$h,
               date = x$date,
               pred=x$pred) 
    
  })
save(out, file = 'out.RData')
