function(input, output){
  
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
  
  
  
  df.forecast <- eventReactive(input$update,{
    
    out <- out_true %>%
      filter(model %in% input$model,
             
             startdt == input$startdt,
             enddt == input$enddt,
             h == input$h)
    
    
    if(input$optlag){
      optlag <- scoredf_raw %>%
        filter(type == 'train',
               model %in% input$model,
               h == input$h,
               startdt == input$startdt,
               enddt == input$enddt
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
        filter(date >= c(ifelse(input$onlytrain,
                                enddt %>%
                                   as.Date %>%
                                   as.yearqtr %>%
                                   add(1/4) %>%
                                   as.yearqtr %>%
                                  as.Date,
                                date %>% min)))
    } else {
      data.frame()
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
        geom_rect(aes(xmin=(df.forecast()$enddt %>% min + 100) %>% as.yearqtr %>% as.Date,
                      xmax=(df.forecast()$enddt %>% min + 366*2) %>% as.yearqtr %>% as.Date,
                      ymin=-Inf, ymax=Inf,
                      fill="Тестовая выборка"),
                  alpha=0.2)+
        
        scale_fill_manual(values = 'black')+
        
        scale_color_manual(values = 'black')+
        scale_linetype_manual(values = 'dotted')+
        scale_size_manual(values = 1)+
        guides(colour = guide_legend(""),
               size = guide_legend(""),
               linetype = guide_legend(""),
               fill = guide_legend(" "))+
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
                  size = 1, linetype="dotted")+
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
                  fill="black", alpha=0.2)+
        
        labs(title = "",
             y = "Изменение инвестиций (log)",
             x = "Дата")+
        theme_minimal()
      
      
      
      
      grid.arrange(p,
                   arrangeGrob(g_legend(p2),g_legend(p1), nrow=2),
                   ncol=2,widths=c(7,1))
      
      
    }
    
    
    
    
    
  })
  
  
  
  df.score <- eventReactive(input$update,{
    
    
    
    scoredf <- switch(input$scoretype,
                      'absolute' = scoredf_raw,
                      'relate' = scoredf)
    scoredf %<>% 
      filter(type == 'test',
             model %in% input$model,
             h == input$h,
             startdt == input$startdt,
             enddt == input$enddt
      )
    if(input$optlag) {
      scoredf %<>%
        inner_join(optlag,
                   by = c('startdt', 'enddt', 'h', 'model', 'lag'))
    } else{
      scoredf %<>%
        filter(lag == input$lag) %>% print
    }
    scoredf %>%
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
  
  df.hair <- eventReactive(input$update_hair, {
    out_hair %>%
      mutate(qdiff = (date %>% as.yearqtr %>% as.numeric)-
               (forecastdate %>% as.yearqtr %>% as.numeric)) %>%
      filter(qdiff >= input$h_hair[1]/4,qdiff <=
               input$h_hair[2]/4) %>%
      filter(startdt == input$startdt_hair,
             model %in% input$model_hair) %>%
      
      
      (function(x) {if(input$actualforecast){
        x %>%
          mutate(group=paste0(forecastdate, model) %>%
                   as.factor)
      }  else {
        x %>%
          mutate(group = paste0(forecastdate, model, enddt) %>%
                   as.factor )
      }
                      }) %>%
      group_by(h, forecastdate, model) %>%
      mutate(end_max = max(enddt)) %>%
      ungroup %>%
      (function(x) {if(input$actualforecast) x %>% filter(enddt == end_max) else x }) 
      
      
  })
  
  output$hair <- renderPlot({
    
    print(input$forecastdate)
    {(df.hair() %>%
      filter(forecastdate <= input$forecastdate %>% as.yearqtr %>% as.Date) %>%
        # (function(x) {
        #  x %>% write_rds('ssdatatest.RDS')}) %>%
        ggplot()+
      
      geom_line(aes(x = date, y = true), color='black', size = 1, linetype = 'dashed')+
      
      labs(title = "",
           y = "Изменение инвестиций (log)",
           x = "Дата")+
      scale_size_manual(values = 1) +
      guides(colour = guide_legend(""),
             size = guide_legend(""),
             linetype = guide_legend(""),
             fill = guide_legend(" "))
     )} %>%
    (function(x) {if(input$facet == 'divide') {x +
        geom_line(aes(x = date, y = pred, group = group),
                                               color = 'cornflowerblue',
                                               alpha = 0.8,
                                               size = 0.8)+
        facet_wrap(vars(model))} 
      else if(input$facet == 'together'){x +
        geom_line(aes(x = date, y = pred, group = group, color = model),
                  alpha = 0.8,
                  size = 0.8)} 
      else{x+
        stat_summary(aes(x = date, y = pred, group = forecastdate),
                     geom = 'line',
                     linetype = 'dashed',
                     color = 'cornflowerblue',
                     fun.y=mean)
        
      }})
      
  })
  
  
}
