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
      dplyr::filter(model %in% input$model,
             
             startdt == input$startdt,
             enddt == input$enddt,
             h == input$h)
    
    
    if(input$optlag){
      optlag <- scoredf_raw %>%
        dplyr::filter(type == 'train',
               model %in% input$model,
               h == input$h,
               startdt == input$startdt,
               enddt == input$enddt
        ) %>%
        group_by(model) %>%
        dplyr::filter(rmse == min(rmse)) %>%
        dplyr::filter(lag == min(lag)) %>%
        ungroup %>%
        select(model, lag) %>%
        unique
      
      
      out %<>%
        split(.$model) %>%
        map_dfr(function(x){
          x %>%
            dplyr::filter(lag == optlag$lag[which(optlag$model ==
                                             (x$model %>% first))])
        })
      
      
      
    } else {
      
      out  %<>%
        dplyr::filter(lag == input$lag)
      
    }
    if(nrow(out) != 0){
      
      out %>%
        dplyr::filter(date >= c(ifelse(input$onlytrain,
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
      dplyr::filter(type == 'test',
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
        dplyr::filter(lag == input$lag) %>% print
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
  
  df.hair <- function(){
    out_hair %>%
      filter(model %in% input$model_hair) %>%
    dplyr::filter(
             startdt %in%  (input$startdt_hair %>% as.Date),
             h >= input$h_hair[1],
             h <= input$h_hair[2]
             )
  }
  # реализует gif через animate
  hairplot <- function(){
    df <- df.hair()
    
    
    {(df %>%
        ggplot()+
        
        geom_line(data = df %>%
                    na.omit
                  ,mapping = aes(x = date,
                                 y = true),
                  color='grey',
                  size = 2,
                  linetype = 'solid',
                  alpha = 0.5)
    )} %>%
      # оформление
      hair.theme() %>%
      # два facet
      hair.model.forecast() %>%
      # если динамический, то через gganimate
      (function(x){
        if(input$play == 'dinamic'){
          x+ 
            transition_reveal(giftime) +
            ease_aes('linear')
        } else {
          x
        }
      })
    
    
    
  }
  
  hair.model.forecast <- function(x) {if(input$startdt_type_hair == 'divide' & input$model_type_hair == 'divide')
  {x +
      geom_line(aes(x = date,
                    y = pred,
                    group = forecastdate),
                linetype = 'dashed',
                color = 'cornflowerblue',
                alpha = 0.8,
                size = 0.8)+
      facet_grid(startdt~model)
  } 
    else if(input$startdt_type_hair == 'divide' & input$model_type_hair == 'together'){x +
        geom_line(aes(x = date, y = pred,
                      group = interaction(forecastdate, model),
                      color = model),
                  linetype = 'dashed',
                  alpha = 0.8,
                  size = 0.8)+
        
        facet_wrap(.~startdt)} 
    else if(input$startdt_type_hair == 'divide' & input$model_type_hair == 'mean')
    {x+
        stat_summary(aes(x = date, y = pred, group = forecastdate),
                     geom = 'line',
                     linetype = 'dashed',
                     color = 'cornflowerblue',
                     alpha = 0.8,
                     size = 0.8,
                     fun.y=mean)+
        facet_wrap(.~startdt)
      
    }
    else if(input$startdt_type_hair == 'together' & input$model_type_hair == 'divide'){
      x+ geom_line(aes(x = date, y = pred,
                       group = interaction(forecastdate, startdt),
                       linetype = as.factor(startdt)),
                   alpha = 0.8,
                   color = 'cornflowerblue',
                   size = 0.8)+
        
        facet_wrap(model~.)+
        scale_linetype_manual(name="Type",values=c(2,3), guide="none")
    }
    else if(input$startdt_type_hair == 'together' & input$model_type_hair == 'together'){
      x+ geom_line(aes(x = date, y = pred,
                       group = interaction(forecastdate, startdt, model),
                       color = model,
                       linetype = as.factor(startdt)),
                   alpha = 0.8,
                   size = 0.8)+
        scale_linetype_manual(name="Type",values=c(2,3), guide="none")
    }
    else if(input$startdt_type_hair == 'together' & input$model_type_hair == 'mean'){
      x+
        stat_summary(aes(x = date, y = pred, 
                         group = interaction(forecastdate, startdt),
                         linetype = as.factor(startdt)),
                     geom = 'line',
                     alpha = 0.8,
                     size = 0.8,
                     color = 'cornflowerblue',
                     fun.y=mean)+
        scale_linetype_manual(name="Type",values=c(2,3), guide="none")
    }
    else if(input$startdt_type_hair == 'mean' & input$model_type_hair == 'divide'){
      x+
        stat_summary(aes(x = date, y = pred, group = forecastdate),
                     geom = 'line',
                     linetype = 'dashed',
                     color = 'cornflowerblue',
                     alpha = 0.8,
                     size = 0.8,
                     fun.y=mean)+
        facet_wrap(model~.)
    }
    else if(input$startdt_type_hair == 'mean' & input$model_type_hair == 'together'){
      x+
        stat_summary(aes(x = date, y = pred, 
                         group = interaction(forecastdate, model), color = model),
                     geom = 'line',
                     linetype = 'dashed',
                     alpha = 0.8,
                     size = 0.8,
                     fun.y=mean)
    }
    else if(input$startdt_type_hair == 'mean' & input$model_type_hair == 'mean'){
      x+
        stat_summary(aes(x = date, y = pred, 
                         group = interaction(forecastdate)),
                     color = 'cornflowerblue',
                     geom = 'line',
                     linetype = 'dashed',
                     alpha = 0.8,
                     size = 0.8,
                     fun.y=mean)
    }
  }
  hair.theme <- function(x){
    df <- df.hair()
    x+
      
      labs(title = "",
           y = "Изменение инвестиций (log)",
           x = "Дата")+
      scale_size_manual(values = 1) +
      guides(colour = guide_legend(""),
             size = guide_legend(""),
             linetype = guide_legend(""),
             fill = guide_legend(" "))+
      theme(legend.position="bottom")+
      theme_minimal()+
      scale_x_date(limits = c(df %>% pull(date) %>% min,
                              df %>% pull(date) %>% max)) +
      scale_y_continuous(limits = c(df %>% select(true, pred) %>% unlist %>% na.omit %>% min,
                                    df %>% select(true, pred) %>% unlist %>% na.omit %>% max))
    
  }
  
  # реализует gif для функции saveFIG в виде отдельных фреймов для использования цикла по ним
  hair.frameplot <- function(i){
    df <- df.hair() %>% filter(giftime <= i)
    
    {(df %>%
        ggplot()+
        
        geom_line(data = df %>%
                    na.omit
                  ,mapping = aes(x = date,
                                 y = true),
                  color='grey',
                  size = 2,
                  linetype = 'solid',
                  alpha = 0.5)
    )} %>%
      # оформление
      hair.theme() %>%
      # два facet
      hair.model.forecast()
  }
  
  #set up function to loop through the draw.a.plot() function
  loop.animate <- function() {
    frames <- df.hair() %>%
      pull(giftime) %>%
      unique %>%
      sort()
    lapply(frames, function(i) {
      hair.frameplot(i) %>% print
    })
  }
  
  
  hairimage <- eventReactive(input$update_hair,{
    if(input$play == 'dinamic'){
      outfile <- tempfile(fileext='.gif')
    } else {
      outfile <- tempfile(fileext='.png')
    }
    
    p <- hairplot() %>%
      arrangeGrob()
    
    class(p) <- c("gridplot", class(p))
    
    print.gridplot <- function(x, ...) {
      cat("printing")
      gridExtra::grid.arrange(x+
                                theme(legend.position = 'none'),
                              arrangeGrob (g_legend(x)),
                              ncol = 2,
                              width = 1)
    }
    
    
    
    if(input$play == 'dinamic'){
      anim_save("outfile.gif", animate(p)) 
      
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
    } else {
      
      
      # Generate a png
      png(outfile, width=400, height=400)
      print(p)
      dev.off()
      
      # Return a list
      list(src = outfile,
           alt = "This is alternate text")
    }
  })
  
  
  output$hair <- renderImage({
    hairimage()
    
    
  }, deleteFile = TRUE)
  
  output$testgif = downloadHandler(
    filename = 'output.gif',
    content  = function(file) {
      saveGIF(
        loop.animate(), movie.name = 'output.gif', interval = 0.1)
      file.rename('output.gif', file)
    })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      paste('outfile',
                                  if(input$play == 'dinamic'){
                                    '.gif'
                                  } else {
                                    '.png'
                                  }
                                  , sep='')
      },
    content = function(file) {
      #ggsave(file,hairplot())
      p <- df.hair() %>%
        ggplot()+
        
        geom_line(data = df.hair() %>%
                    na.omit
                  ,mapping = aes(x = date,
                                 y = true),
                  color='grey',
                  size = 2,
                  linetype = 'solid',
                  alpha = 0.5)+
        
        labs(title = "",
             y = "Изменение инвестиций (log)",
             x = "Дата")+
        scale_size_manual(values = 1) +
        guides(colour = guide_legend(""),
               size = guide_legend(""),
               linetype = guide_legend(""),
               fill = guide_legend(" "))+
        theme(legend.position="bottom")+
        theme_minimal()+
        # transition_reveal(giftime) +
        ease_aes('linear')+
        transition_states(giftime,
          transition_length = 2,
          state_length = 1
        ) +
        enter_fade() + 
        exit_shrink()
      
      image <- animate(p, nframes = 5, renderer = gifski_renderer("gganim.gif"))
      
      #> Linking to ImageMagick 6.9.9.39
      #> Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
      #> Disabled features: fftw, ghostscript, x11
      
      image_write(image, 'test.gif')
    }
  )
  
}
  

