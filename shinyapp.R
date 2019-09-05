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

# save(out_short, file='data/out_short.RData')
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
  melt(id.vars = c('model', 'lag', 'h', 'type', 'startdt', 'enddt')) %>%
  filter(variable == 'rmse') %>%
  mutate(
    h = as.factor(h)) %>%
  dcast(model+lag+h+type+startdt+enddt~variable,
        fun.aggregate = mean)

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


out_cumulative <- out_true %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4),
         true = exp(log(true_lag)+true),
         pred = exp(log(true_lag)+pred)
  ) 


save(out_true, out_short, ytrue,scoredf, scoredf_raw, file = 'data/shinydata.RData')
# # ui----

source('lib.R')
source('fun.R')
ui <- pageWithSidebar(
  headerPanel('Прогнозирование инвестиций'),
  sidebarPanel(
    # radioButtons('plottype',
    #              'Выберите тип графика',
    #              c('logdiff', 'cumulative')),
    selectizeInput('startdt', 'Выберите левую границу тренировочной выборки',
                   out_short$startdt %>% unique %>% set_names(as.yearqtr(.)) ,
                   selected = out_short$startdt %>% unique %>% last),
    # selectizeInput('enddt', 'Выберите правую границу тренировочной выборки',
    #                out_short$enddt %>% unique %>% set_names(as.yearqtr(.))),
    sliderInput('enddt',  'Выберите правую границу тренировочной выборки',
                min = out_short$enddt %>% unique %>% min,
                max = out_short$enddt %>% unique %>% max,
                step = 92,value =out_short$enddt %>% unique %>% min,animate = TRUE
                    ),
    
    numericInput('lag',
                 'Выберите количество лагов в модели (кварталов)',
                 value = out_short$lag %>% median,
                 min = out_short$lag %>% min, 
                 max = out_short$lag %>% max),
    checkboxInput('optlag', 'Использовать для каждой модели
                  оптимальное на тренировочной выборке количество лагов', value = TRUE),
    numericInput('h',
                 'Выберите горизонт прогнозирования (кварталов)',
                 value =  out_short$h %>% median,
                 min = out_short$h %>% min, 
                 max = out_short$h %>% max),
    checkboxInput('cumulative', 'Показывать сумму прогнозов', value = TRUE),
    selectizeInput('model', 'Выберите модель',
                   choices = out_short$model %>% unique,
                   selected = out_short$model %>% unique %>% first, 
                   multiple = TRUE),
    checkboxInput('onlytrain', 'Показывать только тестовую выборку', value = TRUE),
    radioButtons('scoretype',
                 "Выберите тип представления RMSFE",
                 choices = c("Абсолютные значения" = 'absolute',
                             "Значения относительно базовой модели" = 'relate'), 
                 selected = 'relate'),
    
    helpText("Для корректного сравнения
             тренировочных выборок с разными границами 
             RMSFE рассчитывается только по первым 12 наблюдениям тестовой выборки
             (т.е. только для первых 3 лет)."),
    hr(),
    
    
    actionButton("update", "Произвести расчёты"),
    actionButton("animate", "animate")
    ),
  mainPanel(column(12,
    plotOutput('forecast'),
    dataTableOutput('score')
    ))
  )

server <- function(input, output){
  
  df.true <- eventReactive(input$update,{
    
  })
  
  observeEvent(input$update, {
    if(df.forecast() %>% nrow == 0){
      showModal(modalDialog(
        title = "",
        "К сожалению, для указанной тренировочной выборки в данный момент нет данных.",
        easyClose = TRUE
      ))
    }
    
  })
  


  # df.forecast <- eventReactive(input$update,{
  #   
  #   out <- out_short %>%
  #     filter(model %in% input$model,
  #            
  #            startdt == input$startdt,
  #            enddt == input$enddt, 
  #            h == input$h)
  #   
  #   
  #   if(input$optlag){
  #     optlag <- scoredf %>% 
  #       filter(type == 'train',
  #              model %in% input$model,
  #              h == input$h,
  #              startdt == input$startdt,
  #              enddt == input$enddt
  #       ) %>%
  #       group_by(model) %>%
  #       filter(rmse == min(rmse)) %>%
  #       filter(lag == min(lag)) %>%
  #       ungroup %>%
  #       select(model, lag) %>%
  #       unique
  #     
  #     
  #     out %<>%
  #       split(.$model) %>%
  #       map_dfr(function(x){
  #         x %>%
  #           filter(lag == optlag$lag[which(optlag$model ==
  #                                            (x$model %>% first))])
  #       })
  #     
  #     
  #     
  #   } else {
  #     
  #     out  %<>%
  #       filter(lag == input$lag)
  #       
  #   }
  #   if(nrow(out) != 0){
  #   
  #   out %>%
  #     mutate(pred = pred %>% lag(n = input$h)) %>%
  #     filter(date >= c(ifelse(input$onlytrain,
  #                             (enddt %>% as.numeric() + 100) %>%
  #                               as.Date() %>%
  #                               as.yearqtr() %>%
  #                               as.Date,
  #                             date %>% min))) %>%
  #     na.omit
  #   } else {
  #       data.frame()
  #     }
  # })
  
  
  df.forecast <- function(){

      out <- out_short %>%
        filter(model %in% input$model,

               startdt == input$startdt,
               enddt == input$enddt %>% as.yearqtr %>% as.Date,
               h == input$h)


      if(input$optlag){
        optlag <- scoredf %>%
          filter(type == 'train',
                 model %in% input$model,
                 h == input$h,
                 startdt == input$startdt,
                 enddt == input$enddt %>% as.yearqtr %>% as.Date
          ) %>%
          group_by(model) %>%
          filter(rmse == min(rmse)) %>%
          filter(lag == min(lag)) %>%
          ungroup %>%
          select(model, lag) %>%
          unique


        out %<>%
          split(.$model) %>%
          map_dfr(function(x){
            x %>%
              filter(lag == optlag$lag[which(optlag$model ==
                                               (x$model %>% first))])
          })



      } else {

        out  %<>%
          filter(lag == input$lag)

      }
      if(nrow(out) != 0){

      out %>%
        mutate(pred = pred %>% lag(n = input$h)) %>%
        filter(date >= c(ifelse(input$onlytrain,
                                (enddt %>% as.numeric() + 100) %>%
                                  as.Date() %>%
                                  as.yearqtr() %>%
                                  as.Date,
                                date %>% min))) %>%
        na.omit
      } else {
          data.frame()
        }
    }
  
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
    if(df.forecast() %>% nrow != 0){
      
      
      g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}
      
      p1 <- ggplot() + geom_line(data = NULL,
                                 aes(x = ytrue %>%
                                       time %>%
                                       as.Date,
                                     y = ytrue %>%
                                       as.numeric %>%
                                       diff.xts(lag = 4, log=TRUE),
                                     color='Исходный ряд',
                                 linetype = 'Исходный ряд',
                                 size = 'Исходный ряд'))+
        
        scale_color_manual(values = 'black')+
        scale_linetype_manual(values = 'dashed')+
        scale_size_manual(values = 1)+
        guides(colour = guide_legend(""),
               size = guide_legend(""),
               linetype = guide_legend(""))+
        theme(legend.position="right",
              legend.justification="left",
              legend.margin=ggplot2::margin(0,0,0,0),
              legend.box.margin=ggplot2::margin(10,10,10,10))
      
      p2 <- ggplot() +
        geom_line(data = df.forecast(),
                         aes(x = date,
                             y = pred,
                             color = model), size = 1) +
        scale_color_discrete(name = "Модель")+
        theme(legend.position="right",
              legend.justification="left",
              legend.margin=ggplot2::margin(0,0,0,0),
              legend.box.margin=ggplot2::margin(10,10,10,10))
        
      
      
      
      
      
      p <- ggplot() +
        geom_line(data = df.forecast(),
                               aes(x = date,
                                   y = pred,
                                   color = model), size = 1) +
        geom_line(data = NULL,
                    aes(x = ytrue %>%
                          time %>%
                          as.Date,
                        y = ytrue %>%
                          as.numeric %>%
                          diff.xts(lag = 4, log=TRUE)
                        ),
                  color='black',
                    size = 1, linetype="dashed")+
        scale_color_discrete(guide="none")+
        
        
        scale_x_date(date_breaks = datebreaks(),
                     labels=date_format("%Y"),
                     limits = limits()$x)+
        scale_y_continuous(limits = limits()$y)+


        geom_vline(aes(xintercept  = as.Date(df.forecast()$enddt %>% min)),
                   color = "red",linetype="dashed", alpha = vlinealpha())+
        geom_rect(aes(xmin=(df.forecast()$enddt %>% min + 100) %>% as.yearqtr %>% as.Date,
                  xmax=(df.forecast()$enddt %>% min +366*3) %>% as.yearqtr %>% as.Date,
                  ymin=-Inf, ymax=Inf),
                  fill="black", alpha=0.2)
      
        labs(title = "",
             y = "Изменение инвестиций (log)",
             x = "Дата")
      
      


      grid.arrange(p,
                   arrangeGrob(g_legend(p2),g_legend(p1), nrow=2),
                               ncol=2,widths=c(7,1))

      
    }
    
    
      
    
    
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
                  startdt == input$startdt,
             enddt == input$enddt %>% as.yearqtr %>% as.Date
                  ) %>%
      select(model, rmse)
    
  })
  
  output$score <- renderDataTable({
    datatable(df.score(),
              colnames = c('Модель', 'RMSFE'),
              rownames = FALSE,
    options = list(dom = 'tip', 
      
      order = list(list(1, 'asc'))
    )) %>%
      formatRound(columns=c(2), digits=3)
  })
  
  
  
}


shinyApp(ui, server)



# 2012 - 2014 полностью
# 13-15 full
# 14-16 full
# 15 - 17 full (enddt = 2014-4q)
# 16, 17, 18 (enddt =  2015-3q)



out_cumulative %>%
    filter(startdt == max(startdt),
         lag == 2,
         model == 'ss',
         enddt == min(enddt)) %>%
  group_by(lag, h, model, startdt, enddt, forecastdate) %>%
  arrange(date) %>%
  mutate(true = mean(true),
           pred = mean(pred)) %>%
  na.omit %>%
  ggplot()+
  geom_line(aes(x = date, y = true), color='grey', size = 3, linetype = 'dashed')+
  geom_line(aes(x = date, y = pred, group = as.factor(forecastdate)))+
  #facet_wrap(vars(forecastdate))+
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

# меняем startdt

scoredf %>%
  right_join(optlag, by = c('model', 'startdt', 'enddt', 'h', 'lag')) %>%
  filter(type == 'test', enddt == min(enddt)) %>%
  filter(model != 'Random Walk') %>%
  group_by(model, lag, h) %>%
  arrange(enddt) %>%
  #mutate(rmse = rmse/first(rmse)) %>%
  ggplot() +
  geom_line(aes(x = startdt, y = rmse, linetype = model, color=model), size = 0.703)+
  facet_wrap( vars(h), ncol = 2) +
  labs(x = 'Дата прогноза', y = 'RMSFE относительно RW') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))
theme()


# меняем enddt
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
  labs(x = 'Дата прогноза', y = 'RMSFE относительно RW') +
  guides(colour = guide_legend("Модель"),
         linetype = guide_legend("Модель"))
  theme()
# по графику видно, что модели LASSO и ARIMA очень чувствительна к уменьшению размеров выборки
# с уменьешением размеров их качество падает
# Осталные модели в среднем либо не ухудушают свое качество, либо значительно улучшают (ss)

# вычислим значимость коэффициентов в модели spike and slab ----
rm(list=ls())
source('fun.R')
# ss data
load('data/out3.RData')

ssprob <- out3 %>%
  map_dfr(function(x){
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


optlag <- scoredf %>% 
  filter(type == 'train') %>%
  group_by(model, startdt, enddt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  ungroup %>%
  select(model, startdt, enddt, h, lag) %>%
  unique

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


  