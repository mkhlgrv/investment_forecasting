rm(list=ls())
# trainout <- c(out1, out2, out3, out5, out6, out7,out4)

out_short <-
  trainout %>% 
  map_dfr(function(x){
      data.frame(model=x$model,
                 lag = x$lag,
                 startdt= x$startdt,
                 enddt= x$enddt,
                 h = x$h,
                 date = x$date,
                 pred=x$pred) 
    
  }) %>%
  filter(enddt <= as.Date('2015-01-01'))



# Vectorized SWITCH:
correct.names <- Vectorize(vectorize.args = "model",
                 FUN = function(model) {
                   switch(as.character(model),
                          'lasso' = 'LASSO',
                          'postlasso' = 'Post-LASSO',
                          'adalasso' = 'Adaptive LASSO',
                          'ridge' = 'Ridge',
                          'rf' = 'Random Forest',
                          'ss' = 'Spike-and-Slab',
                          'arima' = 'ARIMA',
                          'rw' = 'Random Walk')})

out_short$model <- correct.names(model = out_short$model)


load('jobs/short_arima.RData')
load('jobs/short_rw.RData')
load('jobs/short_ss.RData')
load('jobs/short_adalasso.RData')
load('jobs/short_lasso.RData')
load('jobs/short_postlasso.RData')
load('jobs/short_ridge.RData')



out_short <-do.call(rbind, list(short_ss, short_arima, short_adalasso, short_rw, short_ridge, short_postlasso, short_lasso))
save(out_short, file='data/out_short.RData')
rm(list=ls())
setwd('~/investment_forecasting/')
load('data/out_short.RData')
load('data/raw.RData')
source('fun.R')
source('lib.R')
ytrue <- rawdata$investment


out_true <- data.frame(date = ytrue %>%
                         time %>%
                         as.Date,
                       true = ytrue %>%
                         as.numeric %>%
                         diff.xts(lag = 4, log=TRUE),
                       true_lag = ytrue %>%
                         as.numeric %>%
                         lag.xts(lag = 4)) %>%
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
               mutate(pred = lag(pred, min(h))),
             by = 'date')



# округление до 4 знаков и умножение на 100
meanroundpercent <- function(x){
  (x %>% mean %>% round(4))*100
}



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

# техническая таблица
benchmarkscore <- scoredf_raw %>%
  filter(model == 'Random Walk')

# таблица scoredf (rmse даны относительно rw)
scoredf <- get.score(out_true %>%
                       na.omit)%>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt', 'enddt')) %>%
  filter(variable == 'rmse') %>%
  dcast(model+lag+h+type+startdt+enddt~variable) %>%
  mutate(hse = paste0(h,lag,  startdt, enddt, type)) %>%
  filter(h !=0) %>%
  split(.$hse) %>%
  map_dfr(function(x){
    h <- x$h %>% first %>% as.numeric
    startdt <- x$startdt %>% first
    enddt <- x$enddt %>% first
    type <- x$type %>% first
    
    x$rmse <- x$rmse/
      (benchmarkscore$rmse[which(benchmarkscore$h== h &
                                  benchmarkscore$startdt==startdt &
                                   benchmarkscore$enddt == enddt)] %>% first)
    x %>% select(-hse)
  }) 


optlag <- scoredf_raw %>% 
  filter(type == 'train') %>%
  group_by(model, startdt, enddt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  ungroup %>%
  select(model, startdt, enddt, h, lag) %>%
  unique


out_hair <- out_true %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  inner_join(optlag, by = c('model', 'lag', 'h', 'startdt', 'enddt')) %>%
  mutate(
         datediff = (forecastdate - enddt) %>%
           as.numeric,
         pred = ifelse(h == 0, true, pred)) %>%
   filter(datediff > 0 )


save(out_true, out_short, ytrue,scoredf, scoredf_raw,out_hair, file = 'data/shinydata.RData')
# # ui----

source('lib.R')
source('fun.R')
load('data/shinydata.RData')


choises_q <- format(seq.Date(from = as.Date("2012-01-01"),
                                   to = as.Date("2019-01-01"),
                                 by = "quarter") %>%
                          as.yearqtr,
                        format = "%Y Q%q")

runApp()



# 2012 - 2014 полностью
# 13-15 full
# 14-16 full
# 15 - 17 full (enddt = 2014-4q)
# 16, 17, 18 (enddt =  2015-3q)



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
# server ----
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
    # geom_vline(aes(xintercept  = min(enddt)),
    #            color = "red",linetype="dashed")+
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
  labs(x = 'Правая граница тренировочной выборки', y = 'RMSFE относительно RW',
       title ='Ошибки прогноза относительно границы тренировочной выборки',
       subtitle = 'Горизонт прогнозирования от 1 до 4 кварталов') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))


# вычислим значимость коэффициентов в модели spike and slab ----
rm(list=ls())
source('fun.R')
# ss data
load('jobs/out_ss.RData')

load('jobs/short_ss.RData')

ssprob <- out_ss %>%
  imap_dfr(function(x, i){
  series <- x$series
 
  h <- x$h
  lag <- x$lag
  x$startdt
  x$enddt
  
  prednames <- x$model_fit$x %>%
    colnames
  
  melted <- x$model_fit$model %>%
    melt %>%
    mutate(value = factor(as.character(value), levels = c(1:length(prednames)))) %>%
    dcast(`L1`~value, fun.aggregate = length,fill = 0, drop = FALSE)
  n <- nrow(melted)
  
  prob <- (melted %>%
    .[,-1] %>%
    colSums())/n %>% as.numeric
  
  print(i)

  data.frame(series = x$series,
             h = x$h,
             lag = x$lag,
             startdt = x$startdt,
             enddt = x$enddt,
             predictor = prednames,
             prob = prob)
})
# save(ssprob, file='data/ssprob.Rda')

load('data/ssprob.Rda')

ssprob %>%
  inner_join(optlag %>% filter(model == 'ss') %>% select(-model),
             by = c('lag', 'h', 'startdt', 'enddt')) %>%
  filter(h >=4, startdt == max(startdt)) %>%
  
  mutate(
         startdt = as.factor(startdt)) %>%
  group_by(h, predictor, startdt) %>%
  mutate(prob_mean = mean(prob)) %>%
  ungroup %>%
  group_by(predictor) %>%
  filter(max(prob_mean)>0.6) %>% #|grepl('oil', first(predictor))) %>%
  #mutate(group = paste0(h, "_", startdt)) #%>%
  ggplot()+
  geom_line(aes(enddt, prob, color = h, group = h))+
  #scale_alpha_discrete(range = c(0, 0.3))
  #stat_summary(aes(enddt, prob), alpha = 0.5, geom = 'ribbon', fun.data = 'mean_cl_boot')+
  facet_wrap(vars(predictor))



scoredf_raw %>%
  filter(type == 'test') %>%
  mutate(mseh = paste0(model, startdt, enddt, h)) %>%
  filter(h!=0) %>%
  split(.$mseh) %>%
  map_dfr(function(x){
    m <- x$model %>% first
    s <- x$startdt %>% first
    e <- x$enddt %>% first
    h <- x$h %>% first
    x %>% filter(lag == unique(optlag$lag[which(optlag$model == m &
                                    optlag$startdt == s &
                                    optlag$enddt == e &
                                    optlag$h == h)])) %>%
      select(-mseh)
  }) %>%
  filter(startdt == max(startdt)) %>%
  # dcast(startdt + enddt ~ model, value.var = 'rmse') %>%
  # mutate(date = startdt) %>%
  ggplot(aes(enddt, rmse, color = model))+
    geom_line()+
  facet_wrap(vars(h))


  