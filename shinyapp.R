rm(list=ls())
trainout <- c(out1, out2)


train2 <- trainout %>% 
  map_dfr(function(x){
      data.frame(model=x$model,
                 lag = x$lag,
                 startdt= x$startdt,
                 enddt= x$enddt,
                 h = x$h,
                 date = x$date,
                 pred=x$pred) 
    
  })


ytrue <- rawdata$investment


# # ui----

ui <- pageWithSidebar(
  headerPanel('Прогнозирование инвестиций'),
  sidebarPanel(
    selectizeInput('startdt', 'Start date',
                   train2$startdt %>% unique),
    
    numericInput('lag',
                 'Lag',
                 value = train2$lag %>% min,
                 min = train2$lag %>% min, 
                 max = train2$lag %>% max),
    numericInput('h',
                 'Horizon',
                 value =  train2$h %>% max,
                 min = train2$h %>% min, 
                 max = train2$h %>% max),
    selectizeInput('model', 'Model',
                   train2$model %>% unique),
    actionButton("update", "Update View")
    ),
  mainPanel(plotOutput('mainplot'))
  )

server <- function(input, output){

  train3 <- eventReactive(input$update,{
  train2 %>%
    filter(model == input$model,
           lag == input$lag,
           startdt == input$startdt,
           h == input$h) %>%
      mutate(pred = pred %>% lag(n = input$h))
  })

  
  output$mainplot <- renderPlot({

    ggplot()+
      geom_line(data = train3(),
                aes(x = date,
                    y = pred,
                    col= 'pred')) +
      geom_line(data = NULL,
                aes(x = ytrue %>%
                      time %>%
                      as.Date,
                    y = ytrue %>%
                      as.numeric %>%
                      diff.xts(lag = 4, log=TRUE), color = 'true'))+
      scale_x_date(date_breaks = "5 year", 
                   labels=date_format("%Y"),
                   limits = as.Date(c(train3()$date %>% min,
                                      train3()$date %>% max)))
  })
}


shinyApp(ui, server)
# start date
# end date
# lag
# h
# model
# features
# conf int (true or false)
# server ----
# main plot
# metrics (rmse, r2, mae, mape)
# knowcasting


