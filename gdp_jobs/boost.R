source('~/investment_forecasting/lib.R')
source('~/investment_forecasting/fun.R')

out_boost <- expand.grid(startdt = c(as.Date('1996-01-01'), as.Date('2000-01-01')),
                      enddt = seq(as.Date('2012-10-01'), as.Date('2018-10-01'), by = 'quarter'),
                      lag = c(0L),
                      h=c(1L:8L),
                      model = c('boost'),
                      target = c('RTRD_Q_DIRI','GDPEA_Q_DIRI','UNEMPL_Q_SH', 'CPI_Q_CHI'),
                      eta = c(0.1, 0.2, 0.3)
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
                N = N,
                eta = x$eta,
                target =x$target %>% as.character
    )
  })

save(out_boost,
     file = 'out/full_gdp/out_boost.RData')


short_boost <- out_boost %>%
  map_dfr(function(x){
    data.frame(model=x$model,
               target = x$target,
               eta = x$eta,
               lag = x$lag,
               startdt= x$startdt,
               enddt= x$enddt,
               h = x$h,
               date = x$date,
               pred=x$pred) 
    
  })
save(short_boost, file = 'out/short_gdp/short_boost.RData')
