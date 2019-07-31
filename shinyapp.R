
trainout <- c(ou1, ou2, ou3)
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
  headerPanel('Investment in Russia'),
  sidebarPanel(
    # sliderInput('startdt',
    #                        "Start date",
    #                        min = train2$startdt %>% min,
    #                        max = train2$startdt %>% max,
    #                        value = train2$startdt %>% max,
    #                        timeFormat="%Y %b",
    #             step = 91.3
    #                       ),
    selectizeInput('startdt', 'Start date',
                   train2$startdt %>% unique),
    
    numericInput('lag',
                 'Lag',
                 value = 4,
                 min = train2$lag %>% min, 
                 max = train2$lag %>% max),
    numericInput('h',
                 'Horizon',
                 value = 4,
                 min = train2$h %>% min, 
                 max = train2$h %>% max),
    selectizeInput('model', 'Model',
                   train2$model %>% unique),
    actionButton("update", "Update View")
    ),
  mainPanel(tableOutput('main'))
  )

server <- function(input, output){

  
  output$main <- renderTable({
    
    print(train2 %>%
            filter(model == input$model,
                   lag == input$lag,
                   startdt == input$startdt))
    train2 %>% 
      filter(model == input$model,
             lag == input$lag,
             startdt == input$startdt,
             h == input$h) %>%
      head
  })
  
  # output$mainplot <- renderPlot({
  #   
  #   ggplot()+
  #     geom_line(data = train3(),
  #               aes(x = date,
  #                   y = pred %>% lag(n = input$h),
  #                   col= 'pred')) +
  #     geom_line(data = NULL,
  #               aes(x = ytrue %>%
  #                     time %>%
  #                     as.Date,
  #                   y = ytrue %>%
  #                     as.numeric %>%
  #                     diff.xts(lag = 4, log=TRUE), color = 'true'))
  # })

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


