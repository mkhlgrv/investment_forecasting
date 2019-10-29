source('~/investment_forecasting/lib.R')
source('~/investment_forecasting/fun.R')

out_rf <- expand.grid(startdt = c(as.Date('1997-01-01'), as.Date('2001-01-01')),
                      enddt = seq(as.Date('2012-10-01'), as.Date('2018-10-01'), by = 'quarter'),
                      lag = c(0L:4L),
                      h=c(0L:8L),
                      model = c('rf')
)%T>% 
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

save(out_rf,
     file = 'out_rf.RData')


short_rf <- out_rf %>%
  map_dfr(function(x){
    data.frame(model=x$model,
               lag = x$lag,
               startdt= x$startdt,
               enddt= x$enddt,
               h = x$h,
               date = x$date,
               pred=x$pred) 
    
  })
save(short_rf, file = 'short_rf.RData')

