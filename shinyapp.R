rm(list=ls())
trainout <- c(out1, out2, out3, out4)




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
    
  })

# save(out_short, file='data/out_short.RData')
rm(list=ls())
load('data/out_short.RData')
load('data/raw.RData')
ytrue <- rawdata$investment


# # ui----

ui <- pageWithSidebar(
  headerPanel('Прогнозирование инвестиций'),
  sidebarPanel(
    radioButtons('plottype',
                 'Выберите тип графика',
                 с('logdiff', 'cumulative')),
    selectizeInput('startdt', 'Start date',
                   out_short$startdt %>% unique),
    
    numericInput('lag',
                 'Lag',
                 value = out_short$lag %>% max,
                 min = out_short$lag %>% min, 
                 max = out_short$lag %>% max),
    numericInput('h',
                 'Horizon',
                 value =  out_short$h %>% median,
                 min = out_short$h %>% min, 
                 max = out_short$h %>% max),
    selectizeInput('model', 'Model',
                   out_short$model %>% unique),
    actionButton("update", "Update View")
    ),
  mainPanel(plotOutput('forecast'))
  )

server <- function(input, output){

  df <- eventReactive(input$update,{
    if(input$plottype == 'logdiff'){
      out_short %>%
        filter(model == input$model,
               lag == input$lag,
               startdt == input$startdt,
               h == input$h) %>%
        mutate(pred = pred %>% lag(n = input$h))
    } else if(input$plottype == 'cumulative'){
      out_short
    }

  })

  
  output$forecast <- renderPlot({
    
    ggplot()+
      geom_line(data = df(),
                aes(x = date,
                    y = pred),
                    col= 'red') +
      geom_line(data = NULL,
                aes(x = ytrue %>%
                      time %>%
                      as.Date,
                    y = ytrue %>%
                      as.numeric %>%
                      diff.xts(lag = 4, log=TRUE)), color = 'black')+
      scale_x_date(date_breaks = "5 year", 
                   labels=date_format("%Y"),
                   limits = as.Date(c(df()$date %>% min,
                                      df()$date %>% max)))+
      geom_vline(aes(xintercept  = as.Date(df()$enddt %>% min)),
                 color = "red",linetype="dashed")+
      labs(title = "",
           y = "Изменение инвестиций (log)",
           x = "Дата")
      
    
    
  })
  
  
  
}


shinyApp(ui, server)



out_true %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4),
         true = exp(log(true_lag)+true),
         pred = exp(log(true_lag)+pred)
  ) %>%
  filter(startdt == max(startdt),
         lag == 1,
         forecastdate >= min(enddt)) %>%
  group_by(lag, h, model, startdt) %>%
  arrange(date) %>%
  mutate(true = SMA(true, 4),
         pred = SMA(pred, 4)) %>%
  na.omit %>%
  ggplot()+
  geom_line(aes(x = date, y = true))+
  geom_line(aes(x = date, y = pred, color=model))+
  facet_wrap(vars(forecastdate))+
  scale_x_date(limits = as.Date(c('2011-07-01', '2019-01-01')))
# server ----
# main plot
# metrics (rmse, r2, mae, mape)
# knowcasting

out_true <- data.frame(date = ytrue %>%
             time %>%
             as.Date,
           true = ytrue %>%
             as.numeric %>%
             diff.xts(lag = 4, log=TRUE),
           true_lag = ytrue %>%
             as.numeric %>%
             lag.xts(lag = 4)) %>%
  inner_join(out_short %>%
               group_by(model, lag, startdt, enddt, h) %>%
               mutate(pred = lag(pred, min(h))), by = 'date')

for(modeli in out_true$model %>% unique){
  print(modeli)
  plot <- out_true %>%
    filter(model == modeli,
           startdt == max(startdt),
           lag != 0,
           h != 0) %>%
    ggplot()+
    geom_line(aes(x = date, y = pred), color = 'red')+
    geom_line(aes(x = date, y = true))+
    geom_vline(aes(xintercept  = min(enddt)),
               color = "red",linetype="dashed")+
    labs(title = modeli) +
    facet_grid(vars(lag), vars(h))
  png(paste0('plot/facets/',modeli,".png"), width = 1000, height = 700)
  print(plot)
  dev.off()
}

# 
out_true %>%
  mutate(sdm = paste0(model, startdt)) %>%
  filter(!model %in% c('lasso', 'postlasso', 'rw'),
         lag != 0,
         h != 0) %>%
  ggplot()+
  geom_line(aes(x = date, y = pred, group = sdm),color='darkgreen', alpha = 0.1)+
  geom_line(aes(x = date, y = true), color = 'black', size = 0.6)+
  geom_vline(aes(xintercept  = min(enddt)),
             color = "red",linetype="dashed")+
  #scale_x_date(limits = c(min(enddt), max(date)))
  facet_grid(vars(lag), vars(h))

# проблема: модель lasso показывает плохие результаты (прогноз по среднему) 
# на горизонте прогнозирования 3-4 квартала

meanroundpercent <- function(x){
  (x %>% mean %>% round(4))*100
}
scoredf <- get.score(out_true %>%
            na.omit)%>% 
  select(-c(startdt, enddt))  %>%
  melt(id.vars = c('model', 'lag', 'h', 'type')) %>%
  filter(variable == 'rmse', type=='test') %>%
  mutate(
         h = as.factor(h)) %>%
  dcast(model+h+lag~variable,
        fun.aggregate = mean)
  
benchmarkscore <- scoredf %>%
  filter(model == 'rw')

# scoredf2bench <- scoredf %>% 
#   filter(h !=0) %>%
#   split(.$h) %>%
#   imap_dfr(function(x, i){
#     x$rmse <- x$rmse/(benchmarkscore$rmse[which(benchmarkscore$h==as.numeric(i))] %>% first)
#     x
#   }) #%>%
#   ggplot() +
#   geom_boxplot(aes(x = model, y = rmse))+
#   facet_grid( vars(h),vars(lag))
# 
# scoredf2bench %>%
#   dcast(model+lag~h) %>%
#   export(file='data/score.xlsx')

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


scoredf <- get.score(out_true %>%
            na.omit)%>%
  select( -c(enddt))  %>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt')) %>%
  filter(variable == 'rmse', type=='test') %>%
  dcast(model+lag+h+type+startdt~variable) %>%
  filter(h !=0) %>%
  split(.$h) %>%
  imap_dfr(function(x, i){
    x$rmse <- x$rmse/(benchmarkscore$rmse[which(benchmarkscore$h==as.numeric(i))] %>% first)
    x
  })

# график, на котром показано, как меняется rmse при изменении границ тренировочной выборки
# rmse в 1997-01-01 (первая дата) для соответствующего лага и соответствующего горизонта прогнозирования = 1
scoredf %>%
  filter(model != 'rw') %>%
  group_by(model, lag, h) %>%
  arrange(startdt) %>%
  mutate(rmse = rmse/first(rmse)) %>%
  ggplot() +
  geom_line(aes(x = startdt, y = rmse, linetype = model, color=model), size = 0.703)+
  facet_grid( vars(h),vars(lag))
# по графику видно, что модели LASSO и ARIMA очень чувствительна к уменьшению размеров выборки
# с уменьешением размеров их качество падает
# Осталные модели в среднем либо не ухудушают свое качество, либо значительно улучшают (ss)
scoredf %>%
  filter(model != 'rw') %>%
  group_by(model, lag, h) %>%
  arrange(startdt) %>%
  mutate(rmse = rmse/first(rmse)) %>%
  ggplot() +
  geom_line(aes(x = startdt, y = rmse, linetype = model, color=model), size = 0.703)+
  facet_grid( vars(h),vars(lag))


# вычислим значимость коэффициентов в модели spike and slab ----
rm(list=ls())
source('fun.R')
# ss data
load('data/out3.RData')

ssprob <- out3 %>% map_dfr(function(x){
  series <- x$series
  h <- x$h
  lag <- x$lag
  x$startdt
  x$enddt
  
  prednames <- x$model.fit$x %>% colnames
  melted <- x$model.fit$model %>%
    melt
  n <- nrow(melted)
  prob <- (melted %>%
    dcast(`L1`~value, fun.aggregate = length) %>%
    .[,-1] %>%
    colSums())/n %>% as.numeric
  data.frame(series = x$series,
             h = x$h,
             lag = x$lag,
             startdt = x$startdt,
             enddt = x$enddt,
             predictor = prednames,
             prob = prob)
})


ssprob %>%
  filter(h !=0, lag == 1) %>%
  mutate(h = as.factor(h),
         lag = as.factor(lag)) %>%
  group_by(h, lag, predictor) %>%
  filter(mean(prob)>0.05) %>%
  ggplot()+
  geom_line(aes(startdt, prob))+
  facet_grid(vars(predictor), vars(h))
save(ssprob, file='data/ssprob.Rda')


