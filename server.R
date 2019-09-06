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
    
    out <- out_short %>%
      filter(model %in% input$model,
             
             startdt == input$startdt,
             enddt == input$enddt,
             h == input$h)
    
    
    if(input$optlag){
      optlag <- scoredf %>%
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
        
        scale_color_manual(values = 'black')+
        scale_linetype_manual(values = 'dotted')+
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
             enddt == input$enddt
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
  
  df.hair <- eventReactive(input$update_hair, {
    out_hair %>%
      # inner_join(optlag, by = c('model', 'lag', 'h', 'startdt', 'enddt')) %>%
      filter(startdt == input$startdt_hair,
             model %in% input$model_hair
      ) %>%
      mutate(fdme=paste0(forecastdate, model, enddt) %>% as.factor)
      
  })
  
  output$hair <- renderPlot({
    df.hair() %>%
      filter(forecastdate <= input$forecastdate,
             forecastdate > min(enddt)) %>%
    ggplot()+
      geom_line(aes(x = date, y = pred, group = fdme
                    # , color = datediff
                    )
                ,color = 'cornflowerblue',
                alpha = 0.5)+
      geom_line(aes(x = date, y = true), color='black', size = 1, linetype = 'dashed')#+
      # stat_summary(aes(x = date, y = pred),
      #              fill = 'darkblue',
      #              size = 1,
      #              fun.data = "mean_se",
      #              geom="ribbon",
      #              alpha = 0.5)+
      # facet_wrap(vars(model), drop = TRUE) +
      # scale_colour_gradient(low = "cornflowerblue", high = "white", limits = c(0,2000),
      #                       oob = scales::squish)
  })
  
  
}
