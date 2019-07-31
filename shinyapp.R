rm(list=ls())
trainout <- c(out1, out2, out3)

# out1 %>% 
#   walk(function(x){
#     if(x$model == 'lasso' &
#        x$h == 3 &
#        x$lag == 3){
#       print(x$startdt)
#       x$model_fit %>% predict(type='coefficients') %>% print
#       readline()
#     } else {
#       NULL
#     }
# 
#     
#   })



out_short <- trainout %>% 
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
load('data/out_short.RData')
load('data/raw.RData')
ytrue <- rawdata$investment


# # ui----

ui <- pageWithSidebar(
  headerPanel('Прогнозирование инвестиций'),
  sidebarPanel(
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

  train3 <- eventReactive(input$update,{
  out_short %>%
    filter(model == input$model,
           lag == input$lag,
           startdt == input$startdt,
           h == input$h) %>%
      mutate(pred = pred %>% lag(n = input$h))
  })

  
  output$forecast <- renderPlot({
    
    ggplot()+
      geom_line(data = train3(),
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
                   limits = as.Date(c(train3()$date %>% min,
                                      train3()$date %>% max)))+
      geom_vline(aes(xintercept  = as.Date(train3()$enddt %>% min)),
                 color = "red",linetype="dashed")+
      labs(title = "",
           y = "Изменение инвестиций (log)",
           x = "Дата")
      
    
    
  })
  
  
}


shinyApp(ui, server)


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

out_true %>%
  mutate(sdm = paste0(model, startdt)) %>%
  filter(!model %in% c('lasso', 'postlasso'),
         lag != 0,
         h != 0) %>%
  ggplot()+
  geom_line(aes(x = date, y = pred, group = sdm),color='darkgreen', alpha = 0.1)+
  geom_line(aes(x = date, y = true), color = 'black', size = 0.6)+
  geom_vline(aes(xintercept  = min(enddt)),
             color = "red",linetype="dashed")+
  facet_grid(vars(lag), vars(h))

# проблема: модель lasso показывает плохие результаты (прогноз по среднему) на горизонте прогнозирования 3-4 квартала


# вторая проблема при h = 0 в качестве регрессора участвует investment
get.score(out_true %>%
            na.omit)%>% 
  filter(
         type == 'test',
         h != 0) %>%
  select(-c(startdt, enddt, type))  %>%
  melt(id.vars = c('model', 'lag', 'h')) %>%
  filter(variable == 'rmse') %>%
  mutate(value = round(value*100, 2),
         h = as.factor(h)) %>%
  # dcast(model~h+lag+variable) %>% print
  ggplot() +
  geom_boxplot(aes(x = h, y = value, fill = model))+
  facet_grid(vars(lag))
  
