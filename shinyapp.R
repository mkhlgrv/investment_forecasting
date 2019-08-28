rm(list=ls())
lib()
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



#

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



# округление до 4 знаков и умножение на 100
meanroundpercent <- function(x){
  (x %>% mean %>% round(4))*100
}



# таблица scoredf_raw (без деления на бенчмарки)
scoredf_raw <- get.score(out_true %>%
                           na.omit) %>% 
  select(-c(enddt))  %>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt')) %>%
  filter(variable == 'rmse') %>%
  mutate(
    h = as.factor(h)) %>%
  dcast(model+lag+h+type+startdt~variable,
        fun.aggregate = mean)

# техническая таблица
benchmarkscore <- scoredf_raw %>%
  filter(model == 'rw')

# таблица scoredf (rmse даны относительно rw)
scoredf <- get.score(out_true %>%
                       na.omit)%>%
  select( -c(enddt))  %>%
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt')) %>%
  filter(variable == 'rmse') %>%
  dcast(model+lag+h+type+startdt~variable) %>%
  filter(h !=0) %>%
  split(.$h) %>%
  imap_dfr(function(x, i){
    x$rmse <- x$rmse/(benchmarkscore$rmse[which(benchmarkscore$h==as.numeric(i))] %>% first)
    x
  })

# # ui----

source('lib.R')
source('fun.R')
ui <- pageWithSidebar(
  headerPanel('Прогнозирование инвестиций'),
  sidebarPanel(
    radioButtons('plottype',
                 'Выберите тип графика',
                 c('logdiff', 'cumulative')),
    selectizeInput('startdt', 'Выберите левую границу тренировочной выборки',
                   out_short$startdt %>% unique),
    
    numericInput('lag',
                 'Выберите количество лагов в модели (кварталов)',
                 value = out_short$lag %>% median,
                 min = out_short$lag %>% min, 
                 max = out_short$lag %>% max),
    numericInput('h',
                 'Выберите горизонт прогнозирования (кварталов)',
                 value =  out_short$h %>% median,
                 min = out_short$h %>% min, 
                 max = out_short$h %>% max),
    selectizeInput('model', 'Выберите модель',
                   choices = out_short$model %>% unique,
                   selected = out_short$model %>% unique %>% first, 
                   multiple = TRUE),
    checkboxInput('onlytrain', 'Показывать только тестовую выборку', value = TRUE),
    radioButtons('scoretype',
                 "Выберите тип представления метрик качества",
                 choices = c("Абсолютные значения" = 'absolute',
                             "Значения относительно базовой модели" = 'relate'), 
                 selected = 'absolute'),
    
    
    actionButton("update", "Произвести расчёты")
    ),
  mainPanel(column(12,
    plotOutput('forecast'),
    dataTableOutput('score')
    ))
  )

server <- function(input, output){
  
  df.true <- eventReactive(input$update,{
    
  })

  df.forecast <- eventReactive(input$update,{
    if(input$plottype == 'logdiff'){
      out_short %>%
        filter(model %in% input$model,
               lag == input$lag,
               startdt == input$startdt,
               h == input$h) %>%
        mutate(pred = pred %>% lag(n = input$h)) %>%
        filter(date >= c(ifelse(input$onlytrain,
                               (enddt %>% as.numeric() + 100) %>%
                          as.Date() %>%
                          as.yearqtr() %>%
                          as.Date,
                                date %>% min))) %>%
        na.omit
    } else if(input$plottype == 'cumulative'){
      out_short
    }
    
    

  })
  
  datebreaks <- eventReactive(input$update,{
      ifelse(input$onlytrain,
             '1 year',
             '5 years')
    }
  )
  
  vlinealpha <- eventReactive(input$update,{
    ifelse(input$onlytrain,
           0,
           1)
  }
  )
  
  limits <- eventReactive(input$update,{
    
    print(df.forecast()$date %>% max)
    
    x <- c(df.forecast()$date %>% min,
           df.forecast()$date %>% max)
    ytrue_cut <- ytrue %>%
      diff.xts(lag = 4, log=TRUE) %>%
      .[paste0(x[1], '/', x[2])] %>%
      as.numeric
    
   
    y <- c(min(df.forecast()$pred, ytrue_cut),
           max(df.forecast()$pred, ytrue_cut))
    list(x = x, y = y)
  }
  )
  
  
  

  
  output$forecast <- renderPlot({
    
    ggplot()+
      geom_line(data = df.forecast(),
                aes(x = date,
                    y = pred,
                    color = model)) +
      geom_line(data = NULL,
                aes(x = ytrue %>%
                      time %>%
                      as.Date,
                    y = ytrue %>%
                      as.numeric %>%
                      diff.xts(lag = 4, log=TRUE)), color = 'black')+
      scale_x_date(date_breaks = datebreaks(), 
                   labels=date_format("%Y"),
                   limits = limits()$x)+
      scale_y_continuous(limits = limits()$y)+
      
      
      geom_vline(aes(xintercept  = as.Date(df.forecast()$enddt %>% min)),
                 color = "red",linetype="dashed", alpha = vlinealpha())+
      labs(title = "",
           y = "Изменение инвестиций (log)",
           x = "Дата")
      
    
    
  })
  
  df.score <- eventReactive(input$update,{
    
    scoredf <- switch(input$scoretype,
                      'absolute' = scoredf_raw,
                      'relate' = scoredf)
    scoredf %>% 
      filter(type == 'test',
             model %in% input$model,
                  lag == input$lag,
             h == input$h,
                  startdt == input$startdt
                  ) %>%
      select(model, rmse)
    
  })
  
  output$score <- renderDataTable({
    df.score()
  })
  
  
  
}


shinyApp(ui, server)

# 2012 - 2014 полностью
# 13-15 full
# 14-16 full
# 15 - 17 full (enddt = 2014-4q)
# 16, 17, 18 (enddt =  2015-3q)

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

# все прогнозы вместе в виде зеленых нитей
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


