source('~/investment_forecasting/lib.R')
source('~/investment_forecasting/fun.R')

out_arima <- expand.grid(startdt = c(as.Date('1996-01-01'), as.Date('2000-01-01')),
                         enddt = seq(as.Date('2012-10-01'), as.Date('2018-10-01'), by = 'quarter'),
                         lag = c(0L),
                      h=c(0L:8L), 
                      model = c('arima')
) %T>% 
  (function(x) {assign('N', nrow(x), envir = globalenv())}) %>%
  split(seq(1:nrow(.))) %>%
  
  imap(function(x, i){
    train.model(startdt=x$startdt,
                enddt=x$enddt,
                model = x$model,
                lag=x$lag,
                h=x$h,
                target = 'CPI_Q_CHI',
                i = i,
                N = N
    )
  })

save(out_arima,
     file = 'out/full/out_arima.RData')

short_arima <- out_arima %>%
  map_dfr(function(x){
    data.frame(model=x$model,
               lag = x$lag,
               startdt= x$startdt,
               enddt= x$enddt,
               h = x$h,
               date = x$date,
               pred=x$pred) 
    
  })
save(short_arima, file = 'out/short_arima.RData')

