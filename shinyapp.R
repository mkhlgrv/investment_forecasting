rm(list=ls())

source('fun.R')
source('lib.R')

getwd()
load('jobs/short_arima.RData')
load('jobs/short_rw.RData')
load('jobs/short_ss.RData')
load('jobs/short_adalasso.RData')
load('jobs/short_lasso.RData')
load('jobs/short_postlasso.RData')
load('jobs/short_ridge.RData')
load('jobs/short_rf.RData')




# 
# out_short <- expand.grid(startdt = c(as.Date('1997-01-01'), as.Date('2000-01-01')),
#             enddt =  c(as.Date('2013-01-01'), as.Date('2018-07-01')),
#             lag = c(2L),
#             h=c(1L,2L,8L),
#             model = c('postlasso', 'ss', 'rf', 'rw','lasso', 'adalasso', 'ridge')
# ) %>%
#   split(seq(1:nrow(.))) %>%
#   map(function(x){
#     train.model(startdt=x$startdt,
#                 enddt=x$enddt,
#                 model = x$model,
#                 lag=x$lag,
#                 h=x$h
#     )
#   })  %>% plyr::compact() %>%
#   map_dfr(function(x){
#     data.frame(model=x$model,
#                lag = x$lag,
#                startdt= x$startdt,
#                enddt= x$enddt,
#                h = x$h,
#                date = x$date,
#                pred=x$pred)
# 
#   })


out_short <-do.call(rbind,
                    list(short_ss
                         ,
                         short_arima,
                         short_adalasso,
                          short_rw
                         ,
                         short_ridge,
                         short_postlasso,
                         short_lasso,
                         short_rf
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

optstart <- scoredf_raw %>% 
  filter(type == 'train') %>%
  group_by(model, enddt, h, lag) %>%
  filter(rmse == min(rmse)) %>%
  filter(startdt == max(startdt)) %>%
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



out_cumulative <- out_true %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  inner_join(optlag, by = c('model', 'lag', 'h', 'startdt', 'enddt')) %>% 
  filter(quarter(forecastdate) == 4,
         year(date) >= 2012) %>% 
  mutate(pred_cumulative = exp(log(true_lag)+pred),
         true_cumulative = exp(log(true_lag)+true))

save(out_true, out_short, ytrue,scoredf,optlag, optstart, scoredf_raw,out_hair, rawdata,out_cumulative,
     
     file = 'shinydata.RData')
# # ui----

rm(list=ls())
source('lib.R')
source('fun.R')
load('rawdata.RData')
load('shinydata.RData')

choises_q <- format(seq.Date(from = as.Date("2012-01-01"),
                             to = as.Date("2019-01-01"),
                             by = "quarter") %>%
                      as.yearqtr,
                    format = "%Y Q%q")

runApp()



med_forecast <- import('data/med_forecast.csv', encoding = 'UTF-8', header = TRUE) %>%
  melt %>%
  set_names(c('fctname', 'year', 'value')) %>%
  mutate(year = as.character(year) %>% as.numeric) %>%
  mutate(fctyear = substr(fctname, 1, 4) %>% as.numeric) %>%
  filter(fctyear < year)
  


my_forecast <-
  out_cumulative %>%
  dplyr::group_by(forecastdate, model, h) %>%
  mutate(end_max = max(enddt)) %>% 
  filter(enddt == end_max) %>% 
  ungroup() %>% 
  filter(h!=0) %>%
  mutate(year  = year(date),
         h_year = if_else(h<=4, 1, 2)) %>%
  dplyr::group_by(model,h_year, year, startdt, forecastdate) %>%
  summarise(pred = sum(pred_cumulative),
            true_lag = sum(true_lag),
            true = sum(true_cumulative)) %>%
  mutate(pred = 100*(pred/ true_lag - 1),
         true = 100*(true/ true_lag - 1)) %>%
  ungroup %>% select(-forecastdate)



raw_y <- rawdata$investment %>%
  as.data.frame() %>%
  rownames_to_column('year') %>%
  mutate(year = year(as.yearqtr(year))) %>%
  group_by(year) %>%
  summarise(investment = sum(investment)) %>%
  mutate(investment = 100*(investment/lag(investment)-1))


forec_vs <- my_forecast %>%
  select(-c(true_lag, true)) %>%
  filter(h_year ==1, startdt == max(startdt)) %>%
  filter(!is.na(pred)) %>%
  filter(!model %in% c('Random Walk', 'AR'))
plot1 <- forec_vs %>%   ggplot()+
  geom_bar(aes(year, pred, fill = model),
           stat="identity",
           # fill = 'white',
           position = 'dodge',

           #position = position_dodge2(width = 0.9, preserve = "single"),
           color='black')+
  scale_fill_discrete(name = "Модель")+
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(10,10,10,10))

plot2 <- ggplot()+

  geom_bar(aes(year, value, group=fctname,
               fill = 'Прогноз МЭР',
               alpha ='Прогноз МЭР'), med_forecast %>%
             group_by(year) %>%
                     filter(fctyear== max(fctyear)) %>%
               filter(year <=2019 )
           ,
           stat="identity",
           position = 'dodge'
           )+
  scale_alpha_manual(values = 0.4)+
  scale_fill_manual(values = 'blue')+
  guides(fill = guide_legend(" "),
         alpha = guide_legend(" "))+
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(10,10,10,10))


p <- forec_vs %>%   ggplot()+
  geom_bar(aes(year, pred, fill = model),
           stat="identity",
           # fill = 'white',
           position = 'dodge',
           
           #position = position_dodge2(width = 0.9, preserve = "single"),
           color='black')+
  
  geom_bar(aes(year, value, group=fctname,
               ),
           fill = 'blue',
           alpha =0.4,
           med_forecast %>%
             group_by(year) %>%
             filter(fctyear== max(fctyear)) %>%
             filter(year <=2019 )
             ,
           stat="identity",
           position = 'dodge'
  )+geom_point(aes(year, investment),
               data = raw_y %>% filter(year >=2012),
               color = 'black', size = 2)+
  geom_line(aes(year, investment),
            data = raw_y %>% filter(year >=2012),
            color = 'black')+
  
  scale_fill_discrete(guide="none")+
  
  labs(#title = 'Инвестиции в России: прогнозы МЭР и прогнозы автора',
       subtitle = 'Горизонт прогнозирования - один год',
       x = 'Дата',
       y = 'Изменение валового накопления основного капитала,\n в % к прошлому году')+
  theme_bw()
  
  
  
grid.arrange(p,
             arrangeGrob(g_legend(plot1),
                         g_legend(plot2),
                         nrow=2),
             ncol=2,widths=c(7,3))

  



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
           model != 'Random Walk') %>% dcast(model~h, value.var='rmse', fun.aggregate = min) %>%
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
  labs(x = 'Правая граница тренировочной выборки', y = 'RMSFE относительно RW')+
       # title ='Ошибки прогноза относительно границы тренировочной выборки') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))+
  scale_x_date(limits = as.Date(c('2011-10-01', '2017-01-01')))


# вычислим значимость коэффициентов в модели spike and slab ----
rm(list=ls())
source('fun.R')
# ss data
load('jobs/out_ss.RData')

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



ssprob %>%
  filter(h>4) %>%
  inner_join(ss_coefs,
                      by = c("series", "h", "lag", "startdt", "enddt", "predictor")) %>%
  inner_join(optlag %>% filter(model == 'Spike-and-Slab'), by = c("h", "lag", "startdt", "enddt")) %>%
  mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
  group_by(h, lag, predictor) %>%
  mutate(prob_mean = mean(prob)) %>%
  ungroup() %>%
  group_by(predictor) %>%
  filter(mean(prob_mean) > 0.375) %>%
  mutate(h = as.factor(h)) %>%
  ggplot(aes(enddt, value,
             color = predictor))+
  stat_summary(fun.y=mean, geom = 'line') +
  facet_wrap(vars( h))
save(ssprob, file='data/ssprob.Rda')

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
  filter(max(prob_mean)>0.3) %>% #|grepl('oil', first(predictor))) %>%
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
################################

# что то нажо сделать стем, что после конца прогнозирования на оджну точку только одно наблдюение
# группы должны создаваться не по fdme а просто mde

load('ssdatatest.RDS')
ssdatatest %>%
  select(-c(true, fdme)) %>%
  filter(date >= '2015-01-01') %>%
  na.omit %>%
  mutate(newfd = paste0(model, forecastdate)) %>%
  ggplot(aes(x = date, y = pred, color = newfd))+
  geom_point()+
  geom_line()
  