rm(list=ls())

source('fun.R', encoding = 'utf-8')
source('lib.R')
getwd()
load('out/short_gdp/short_rf.RData')
load('out/short_gdp/short_rw.RData')
load('out/short_gdp/short_boost.RData')


out_short <-do.call(bind_rows,
                    list(
                      short_rw,
                      short_boost,
                      short_rf
                    ))
# out_short$model <- correct.names(model = out_short$model)

load('data/raw.RData')

ytrue <- rawdata[, c("RTRD_Q_DIRI", "GDPEA_Q_DIRI", "UNEMPL_Q_SH", "CPI_Q_CHI")]
  # rbind(xts(x = c(NA, NA),
  #           order.by = c('2019-12-01', '2020-01-01') %>% as.Date %>% as.yearqtr) %>%
  #         set_colnames('investment'))
out_true <- out_short %>% 
  split(.$target) %>%
  map_dfr(
    function(x){
      y <- ytrue[,x$target %>% first]
      out_true <- data.frame(date = y %>%
                               time %>%
                               as.Date,
                             true = y %>%
                               as.numeric %>%
                               diff.xts(lag = 4, log=TRUE),
                             true_lag = y %>%
                               as.numeric %>%
                               lag.xts(k = 4)) %>%
        right_join(x %>%
                     mutate(group = paste(model, lag, startdt, enddt, h)) %>%
                     split(.$group) %>%
                     map_dfr(function(x){
                       
                       x$group <- NULL
                       m <- x$model %>% first
                       tg <- x$target %>% first
                       nt <- x$ntree %>% first
                       et <- x$eta %>% first
                       l <- x$lag %>% first
                       sd <- x$startdt %>% first
                       ed <- x$enddt %>% first
                       h <- x$h %>% first
                       dt <- x$date %>% max %>% as.yearqtr %>% as.numeric %>% add(1/4)
                       
                      
                       # bind by rows
                       rbind(x, data.frame(model = m,
                                           target = tg,
                                           ntree = nt,
                                           eta = et,
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
                     group_by(model, ntree, eta, lag, startdt, enddt, h) %>%
                     mutate(pred = lag(pred, first(h))),
                   by = 'date')
    }
  )



# таблица scoredf_raw (без деления на бенчмарки)
scoredf_raw <- get.score(out_true) %>%
  melt(id.vars = c('target','model','ntree',
                   'eta',
                   'lag', 'h', 'type', 'startdt', 'enddt')) %>%
  filter(variable == 'rmse') %>%
  mutate(
    h = as.factor(h)) %>%
  dcast(model+target+ntree+eta+
          lag+h+type+startdt+enddt~variable,
        fun.aggregate = mean) %>%
  mutate(h = h %>% as.character %>% as.numeric)


scoredf <- get.score(out_true) %>%
  filter(type == 'test') %>%
  melt(id.vars = c('target','model','ntree',
                   'eta',
                   'lag', 'h', 'type', 'startdt', 'enddt')) %>%
  filter(variable == 'rmse') %>%
  dcast(model+target+ntree+eta+lag+h+type+startdt+enddt~variable,
        fun.aggregate=mean) %>%
  mutate(hls = paste0(h,lag, startdt, target)) %>%
  #filter(h !=0) %>%
  split(.$hls) %>%
  map_dfr(function(x){
    # if(x$h %>% first == 0){
    # x$rmse = x$rmse/ x$rmse[which(x$model == 'AR')]
    # }
    # else
    # {
    x0 <- x
    bench <- x$rmse[which(x$model == 'rw')]
    x %<>% group_by(model, ntree, eta, startdt, target, lag, h) %>%
      summarise(rmse = sqrt(sum(rmse^2))/sqrt(sum(bench^2)))
    # x$rmse = x$rmse/x$rmse[which(x$model == 'rw')]
    x
  
  })

scoredf %>%
  #filter(type == 'test') %>%
  # filter(startdt == '1996-01-01') %>%
  filter(lag==0, h > 0) %>% #, model == 'rf') %>%
  #group_by(model,eta,
  #         ntree, target, lag, h, startdt) %>%
  #summarise(rmse = mean(rmse)) %>%
  ungroup %>%
  mutate(startdt = as.character(startdt))%>%
  dcast(target+model+ 
        startdt+ntree+eta~ h) %>%
  xtable() %>%
  print(include.rownames = FALSE)


# static hair plot ----
out_hair <- out_true %>%
  rbind(out_true %>%
          filter(h == 1)%>%
          mutate(h = 0)) %>%
  filter(enddt < as.Date(as.yearqtr(date)-h/4)) %>%
  group_by(date, h, model, target, ntree, startdt) %>%
  filter(enddt == max(enddt)) %>%
  ungroup %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  mutate(pred = ifelse(h == 0, true, pred)) %>%
  mutate(giftime =as.numeric(forecastdate)+0.2*((date -forecastdate) %>% as.numeric())) %>%
  filter(forecastdate <='2019-01-01') %>%
  mutate(true = ifelse(date <= forecastdate, true, NA))


fordata <- out_hair %>%
  filter(startdt ==
           '2000-01-01',
         date <='2019-01-01') %>%
  filter(model !='rw') %>%
  mutate(model = paste0(model, ntree, eta))
  # eta


# hair <- 

  ggplot(fordata%>%
                 mutate(true_na = ifelse(date <= forecastdate, true, NA)))+
  geom_path(data = fordata  %>%
              mutate(true_na = ifelse(date <= forecastdate, true, NA)) %>%
              filter(!is.na(true_na)),


            aes(date, true_na,
                alpha = 'Наблюдаемые\nзначения',
                color = 'Наблюдаемые\nзначения',
                size = 'Наблюдаемые\nзначения',
                linetype = 'Наблюдаемые\nзначения'
                ))+
  geom_line(aes(date, pred,
                group = interaction(startdt,
                                    forecastdate),
                alpha = 'Прогноз',
                color = 'Прогноз',
                size = 'Прогноз',
                linetype = 'Прогноз'
                )
  )+
  facet_wrap(vars(interaction(model, target)), scales = 'free_y')+
  #scale_y_continuous(limits = c(-0.2, 0.15))+
  labs(x = 'Дата',
       y = 'Квартальное изменение валового
       накопления\nосновного капитала (разность логарифмов)')+
  theme_bw()+
  scale_colour_manual(name="",
                      values=c('grey', 'black'),
                      guide = guide_legend(override.aes=list(linetype=c(1,2),
                                                             alpha = c(0.5, 1),
                                                             size = c(2, 0.6)))) +
  scale_size_manual(name="Size",values=c(2,0.6), guide="none") +
  scale_alpha_manual(name="Alpha",values=c(0.5,1), guide="none") +
  scale_linetype_manual(name="Type",values=c(1,2), guide="none") +
  theme(legend.position="bottom")

# print(hair)
