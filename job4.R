source('lib.R')
source('fun.R')

out4 <- expand.grid(startdt = seq(as.Date('1997-01-01'), as.Date('2000-01-01'), by = 'quarter'),
                    enddt = as.Date('2011-10-01'),
                    lag = c(0L:4L),
                    h=c(0L:4L), 
                    model = c('arima', 'rw')
) %>%
  filter(model!='arima' | lag == 0) %>%
  split(seq(1:nrow(.))) %>%
  map(function(x){
    train.model(startdt=x$startdt,
                enddt=x$enddt,
                model = x$model,
                lag=x$lag,
                h=x$h
    )
  })

save(out4, file = 'data/out4.RData')
