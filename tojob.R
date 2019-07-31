
source('lib.R')
source('fun.R')

out <- expand.grid(startdt = seq(as.Date('1997-04-01'), as.Date('1998-01-01'), by = 'quarter'),
            enddt = as.Date('2011-12-01'),
            lag = c(0:4),
            h=c(1L:4L), model = c('lasso',
                                                                    'ridge',
                                                                    'adalasso',
                                                                    'postlasso',
                                                                    'rf',
                                                                    'ss',
                                                                    'arima')
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

save(out, file = 'data/out.RData')
