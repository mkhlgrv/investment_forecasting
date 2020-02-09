rm(list=ls())

source('fun.R', encoding = 'utf-8')
source('lib.R')

getwd()
load('out/short_rf_100.RData')
load('out/short_rf_500.RData')
load('out/short_rf_1000.RData')
load('out/short_rf_2000.RData')

load('out/short_boost_100.RData')
# load('out/short_boost_500.RData')

load('out/short_boost_100_2.RData')
# load('out/short_boost_500_2.RData')

load('out/short_boost_100_1.RData')

load('out/short_boost_100_4.RData')


#load('out/short_rf.RData')
#load('out/short_boost.RData')

load('out/short_arima.RData')
load('out/short_rw.RData')
load('out/short_ss.RData')
load('out/short_adalasso.RData')
load('out/short_lasso.RData')
load('out/short_postlasso.RData')
load('out/short_ridge.RData')
load('out/short_rf.RData')
load('out/short_elnet.RData')
load('out/short_zero.RData')




out_short <-do.call(rbind,
                    list(
                      short_ss,
                      #short_boost,
                      #short_rf,
                      short_elnet,
                      short_arima,
                      short_adalasso,
                      short_rw,
                      short_ridge,
                      short_postlasso,
                      short_lasso
                    )) %>%
  filter(h!=0) %>%
  rbind(short_zero %>%
          filter(!model %in%c('rf', 'boost')) %>%
          mutate(startdt = as.character(startdt)) %>%
          mutate(startdt = ifelse(startdt == '1996-04-01', '1996-01-01', startdt)) %>%
          mutate(startdt = as.Date(startdt))) %>%
  rbind(do.call(rbind,
                list(
                  short_boost_100,
                  short_boost_100_2,
                  short_boost_100_4,
                  short_boost_100_1,
                  short_rf_100,
                  short_rf_500,
                  short_rf_1000,
                  short_rf_2000
                )
        ))



out_short$model <- correct.names(model = out_short$model)


save(out_short, file='data/out_short.RData')
rm(list=ls())
setwd('~/investment_forecasting/')
load('data/out_short.RData')
load('data/raw.RData')
source('fun.R')
source('lib.R')
ytrue <- rawdata$investment %>%
  rbind(xts(x = c(NA, NA),
            order.by = c('2019-12-01', '2020-01-01') %>% as.Date %>% as.yearqtr) %>%
          set_colnames('investment'))
out_true <- data.frame(date = ytrue %>%
                         time %>%
                         as.Date,
                       true = ytrue %>%
                         as.numeric %>%
                         diff.xts(lag = 4, log=TRUE),
                       true_lag = ytrue %>%
                         as.numeric %>%
                         lag.xts(k = 4)) %>%
  right_join(out_short %>%
               mutate(group = paste(model, lag, startdt, enddt, h)) %>%
               split(.$group) %>%
               map_dfr(function(x){
                 
                 x$group <- NULL
                 m <- x$model %>% first
                 l <- x$lag %>% first
                 sd <- x$startdt %>% first
                 ed <- x$enddt %>% first
                 h <- x$h %>% first
                 dt <- x$date %>% max %>% as.yearqtr %>% as.numeric %>% add(1/4)
                 
                 # bind by rows
                 rbind(x, data.frame(model = m,
                                     lag = l,
                                     startdt = sd,
                                     enddt = ed,
                                     h = h,
                                     date = seq(dt,
                                                dt + (h+1)/4,
                                                by = 1/4
                                     ) %>% as.yearqtr %>% as.Date,
                                     pred = 0))
               }) %>%
               group_by(model, lag, startdt, enddt, h) %>%
               mutate(pred = lag(pred, first(h))),
             by = 'date')


# таблица scoredf_raw (без деления на бенчмарки)
scoredf_raw <- get.score(out_true %>%
                           na.omit) %>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt', 'enddt')) %>%
  filter(variable == 'rmse') %>%
  mutate(
    h = as.factor(h)) %>%
  dcast(model+lag+h+type+startdt+enddt~variable,
        fun.aggregate = mean) %>%
  mutate(h = h %>% as.character %>% as.numeric)

# таблица scoredf (rmse даны относительно rw)

scoredf <- get.score(out_true %>%
                       na.omit) %>%
  filter(type == 'test') %>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt','enddt')) %>%
  filter(variable == 'rmse') %>%
  dcast(model+lag+h+type+startdt~variable, fun.aggregate=mean) %>%
  mutate(hls = paste0(h,lag, startdt)) %>%
  #filter(h !=0) %>%
  split(.$hls) %>%
  map_dfr(function(x){
    # if(x$h %>% first == 0){
    # x$rmse = x$rmse/ x$rmse[which(x$model == 'AR')]
    # }
    # else
    # {
    x$rmse = x$rmse/x$rmse[which(x$model == 'Случайное блуждание')]
    # }
    x %>% select(-hls)
  })


# optlag <- scoredf_raw %>%
# filter(type == 'test') %>%
# group_by(model, startdt, enddt, h) %>%
# filter(rmse == min(rmse)) %>%
# filter(lag == min(lag)) %>%
# ungroup %>%
# select(model, startdt, enddt, h, lag) %>%
# unique
#
# optstart <- scoredf_raw %>%
# filter(type == 'test') %>%
# group_by(model, enddt, h, lag) %>%
# filter(rmse == min(rmse)) %>%
# filter(startdt == max(startdt)) %>%
# ungroup %>%
# select(model, startdt, enddt, h, lag) %>%
# unique


# out_hair <-
  # out_true %>%
  # filter(enddt < as.Date(as.yearqtr(date)-h/4)) %>%
  # group_by(date, h, model, startdt) %>%
  # filter(enddt == max(enddt)) %>%
  # ungroup %>%
  # mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  # mutate(pred = ifelse(h == 0, true, pred)) %>%
  #   filter(startdt == '2000-01-01',
  #          forecastdate <='2019-01-01',
  #          h>0,
  #          model != 'Random Walk') %>% 
  #   
  #   # вариант 1 просто рисуем прогнозы
  #   
  #   ggplot()+
  #   stat_summary(aes(x = date, y = true),
  #                fun.y=mean, geom='line', alpha = 0.5, size = 4, color = 'grey')+
  #   geom_line(aes(date, pred,  color = forecastdate,
  #                 group = interaction(startdt,
  #                                     forecastdate),
  #                 linetype = factor(startdt)))+
  #   facet_wrap(vars(model))+
  #   scale_y_continuous(limits = c(-0.2, 0.3))
    
    # вариант 2 сумма квадратов ошибок на каждую дату прогноза
    # с ростом h растет и абсолютная ошибка,
    # поэтому делим ошибку одной модели на среднюю ошибку для каждого h
    
    # na.omit %>%
    # filter(h<=2) %>%
    # mutate(error = (pred - true)^2) %>%
    # group_by(h) %>%
    # mutate(error = (error-mean(error))/sd(error)) %>%
    # ungroup %>%
    # group_by(forecastdate, model, startdt) %>%
    # summarise(sse = mean(error)) %>%
    # ggplot()+
    # geom_line(aes(forecastdate, sse,
    #               color = factor(startdt)))+
    # facet_wrap(vars(model))
    #   
      



out_cumulative <- out_true %>%
  filter(lag==0) %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  #inner_join(optlag, by = c('model', 'lag', 'h', 'startdt', 'enddt')) %>%
  filter(quarter(forecastdate) == 3,
         year(date) >= 2012) %>%
  mutate(pred_cumulative = exp(log(true_lag)+pred),
         true_cumulative = exp(log(true_lag)+true))

save(out_true, out_short, ytrue,scoredf, scoredf_raw,#out_hair,
     out_cumulative,
     
     file = 'shinydata.RData')

# # ui--—

rm(list=ls())

source('lib.R')
source('fun.R')
load('data/raw.RData')
load('shinydata.RData')



choises_q <- format(seq.Date(from = as.Date("2012-01-01"),
                             to = as.Date("2019-01-01"),
                             by = "quarter")  %>%
                      as.yearqtr,
                    format = "%Y Q%q")

runApp()







scoredf %>%
  
  filter(type=='test',
         startdt == min(startdt)) %>%
  dplyr::group_by(model, startdt,enddt, h, lag) %>%
  mutate(rm_min = min(mean(rmse, na.rm=TRUE))) %>%
  filter(mean(rmse) == mean(rm_min)) %>%
  ggplot(aes(enddt,
             rmse,
             group=interaction(model, lag, h ),
             color=as.factor(h)))+
  geom_line()+
  facet_wrap(vars(model))

scoredf %>%
  inner_join(optlag,
             by = c("model", "lag", "h", "startdt", "enddt")) %>%
  filter(type=='test',
         h >=5,
         model
         
         != 'Random Walk') %>% dcast(model~h, value.var='rmse', fun.aggregate = min) %>%
  xtable::xtable() %>%
  print




scoredf %>%
  filter(model != 'Random Walk',
         type=='test') %>%
  inner_join(optlag,
             by = c('startdt', 'enddt', 'h', 'model', 'lag')) %>%
  mutate(startdt = as.factor(startdt)) %>%
  ggplot()+
  stat_summary(aes(h, rmse, color= model),geom='line',size=1, fun.y=mean)#+
geom_boxplot(aes(model, rmse, fill=startdt))


# 2012 - 2014 полностью
# 13-15 full
# 14-16 full
# 15 - 17 full (enddt = 2014-4q)
# 16, 17, 18 (enddt = 2015-3q)



out_cumulative %>%
  inner_join(optlag, by = c('model', 'lag', 'h', 'startdt', 'enddt')) %>%
  filter(startdt == max(startdt),
         date >= as.Date('2011-07-01'),
         !model %in% c('Random Walk', 'LASSO'),
         model == 'Random Forest'
  ) %>%
  mutate(fdme=paste0(forecastdate, model, enddt) %>% as.factor,
         datediff = (date - enddt) %>% as.numeric,
         pred = ifelse(h == 0, true, pred)) %>%
  filter(datediff >= 0) %>%
  group_by(lag, h, model, startdt, enddt, forecastdate) %>%
  ggplot()+
  geom_line(aes(x = date, y = pred, group = fdme, color = datediff), alpha = 0.5)+
  geom_line(aes(x = date, y = true), color='black', size = 1, linetype = 'dashed')+
  stat_summary(aes(x = date, y = pred),
               fill = 'darkblue',
               size = 1,
               fun.data = "mean_se",geom="ribbon", alpha = 0.5)+
  facet_wrap(vars(model))+
  #scale_x_date(limits = as.Date(c('2011-07-01', '2019-01-01')))+
  scale_colour_gradient(low = "cornflowerblue", high = "white")
# server ——
# main plot
# metrics (rmse, r2, mae, mape)
# knowcasting


for(modeli in out_true$model %>% unique){
  print(modeli)
  plot <- out_true %>%
    filter(model == modeli,
           startdt == max(startdt),
           lag != 0,
           h != 0,
           date > min(enddt)) %>%
    mutate(enddt.factor = as.factor(enddt)) %>%
    ggplot()+
    geom_line(aes(x = date, y = pred, group = enddt.factor), color = 'darkgreen', alpha = 0.2)+
    geom_line(aes(x = date, y = true))+
    # geom_vline(aes(xintercept = min(enddt)),
    # color = "red",linetype="dashed")+
    labs(title = modeli) +
    facet_grid(vars(lag), vars(h))
  png(paste0('plot/facets/',modeli,".png"), width = 1000, height = 700)
  print(plot)
  dev.off()
}


# проблема: модель lasso показывает плохие результаты (прогноз по среднему)
# на горизонте прогнозирования 3-4 квартала




# усредненные по горизонту прогнозирования значения
out_true %>%
  filter(startdt == max(startdt)) %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4) %>% as.factor()
  ) %>%
  # filter(model == 'ss') %>%
  ggplot(aes(x = date, y = pred, color = model))+
  stat_summary(fun.y = 'mean', geom="line")+
  
  geom_line(aes(x = date, y = true), color = 'black')+
  facet_wrap(vars(lag))




# график, на котром показано, как меняется rmse при изменении границ тренировочной выборки
# rmse в 1997-01-01 (первая дата) для соответствующего лага и соответствующего горизонта прогнозирования = 1

# меняем startdt

scoredf_raw %>%
  right_join(optlag, by = c('model', 'startdt', 'enddt', 'h', 'lag')) %>%
  filter(type == 'test', enddt == min(enddt), h !=0) %>%
  filter(model != 'Random Walk') %>%
  filter(model != 'ARIMA') %>%
  group_by(model, lag, h) %>%
  arrange(startdt) %>%
  mutate(rmse = rmse/first(rmse)) %>%
  ggplot() +
  geom_line(aes(x = startdt, y = rmse, linetype = model, color=model), size = 0.703)+
  facet_wrap( vars(h), ncol = 2) +
  labs(x = 'Левая граница тренировочной выборки', y = 'RMSFE относительно RW',
       title ='Ошибки прогноза относительно границы тренировочной выборки',
       subtitle = 'Горизонт прогнозирования от 1 до 4 кварталов') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))




scoredf %>%
  right_join(optlag, by = c('model', 'startdt', 'enddt', 'h', 'lag')) %>%
  filter(type == 'test', startdt == max(startdt)) %>%
  filter(model != 'Random Walk') %>%
  group_by(model, lag, h) %>%
  arrange(startdt) %>%
  #mutate(rmse = rmse/first(rmse)) %>%
  ggplot() +
  geom_line(aes(x = enddt, y = rmse, linetype = model, color=model), size = 0.703)+
  facet_wrap( vars(h), ncol = 2) +
  
  labs(x = 'Правая граница тренировочной выборки', y = 'RMSFE относительно RW')+
  # title ='Ошибки прогноза относительно границы тренировочной выборки') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))+
  scale_x_date(limits = as.Date(c('2011-10-01', '2017-01-01')))


# вычислим значимость коэффициентов в модели spike and slab ——
getwd()
rm(list=ls())
source('fun.R')
source('lib.R')
# ss data
load('out/full/out_ss.RData')

load('jobs/short_ss.RData')


out_ss %<>% plyr::compact()
ssprob <- out_ss %>%
  imap_dfr(function(x, i){
    series <- x$series
    
    h <- x$h
    lag <- x$lag
    
    prednames <- x$model_fit$x %>%
      colnames
    
    
    melted <- x$model_fit$model %>%
      melt
    if(nrow(melted) == 0){
      prob=NA
    } else{
      
      melted %<>% mutate(value = factor(as.character(value), levels = c(1:length(prednames)))) %>%
        dcast(`L1`~value, fun.aggregate = length,fill = 0, drop = FALSE)
      n <- nrow(melted)
      
      prob <- (melted %>%
                 .[,-1] %>%
                 colSums())/n %>% as.numeric
      
      
    }
    print(i)
    
    
    
    data.frame(series = x$series,
               h = x$h,
               lag = x$lag,
               startdt = x$startdt,
               enddt = x$enddt,
               predictor = prednames,
               prob = prob)
  })


ss_coefs <- out_ss %>%
  imap_dfr(function(x, i){
    series <- x$series
    
    h <- x$h
    lag <- x$lag
    
    
    
    coefs <- x$model_fit$gnet
    prednames <- names(coefs)
    
    data.frame(series = x$series,
               h = x$h,
               lag = x$lag,
               startdt = x$startdt,
               enddt = x$enddt,
               predictor = prednames,
               value = coefs %>% as.numeric)
  })



p <- ssprob %>%
  filter(lag ==0,
         startdt=='2001-01-01'#,
         
         
         # predictor %in% c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO',
         #
         # 'oil', 'rts',
         # 'CPI_Q_CHI',
         # 'GDPEA_Q_DIRI',
         # #'EMPLDEC_Q',
         # 'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
         # 'CNSTR_Q_DIRI'# индекс работ в строительств
         #)
         
  ) %>%
  inner_join(ss_coefs,
             by = c("series", "h", "lag", "startdt", "enddt", "predictor")) %>%
  mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
  group_by(h, lag, predictor) %>%
  mutate(prob_mean = mean(prob)) %>%
  ungroup %>%
  group_by(h, lag, startdt, enddt) %>%
  arrange(desc(prob_mean)) %>%
  filter(row_number()<=5) %>%
  ungroup() %>%
  mutate(h = as.factor(h),
         value= ifelse(prob<0.5, NA, value)) %>%
  ggplot()+
  #stat_summary(aes(enddt, prob,
  # color = predictor),fun.y=mean, geom = 'line', linetype='dashed') +
  geom_line(aes(enddt, prob, color = predictor))+
  facet_wrap(vars( h))
plotly::ggplotly(p)

save(ssprob, file='data/ssprob.Rda')

load('data/ssprob.Rda')