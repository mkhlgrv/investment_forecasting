source('~/investment_forecasting/lib.R')
source('~/investment_forecasting/fun.R')

out_rw <- expand.grid(startdt = c(as.Date('1996-01-01'), as.Date('2000-01-01')),
                         enddt = seq(as.Date('2012-10-01'), as.Date('2018-10-01'), by = 'quarter'),
                         lag = c(0L),
                         h=c(1L:8L),
                         model = c('rw'),
                         target = c('RTRD_Q_DIRI','GDPEA_Q_DIRI','UNEMPL_Q_SH', 'CPI_Q_CHI')
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
                target =x$target %>% as.character
    )
  })

save(out_rw,
     file = 'out/full_gdp/out_rw.RData')


short_rw <- out_rw %>%
  map_dfr(function(x){
    data.frame(model=x$model,
               target = x$target,
               lag = x$lag,
               startdt= x$startdt,
               enddt= x$enddt,
               h = x$h,
               date = x$date,
               pred=x$pred) 
    
  })
save(short_rw, file = 'out/short_gdp/short_rw.RData')
