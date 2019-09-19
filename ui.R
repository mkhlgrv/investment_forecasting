navbarPage('Прогнозирование инвестиций',
           tabPanel('Сравнение моделей',
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput('startdt', 'Выберите левую границу тренировочной выборки',
                                       out_short$startdt %>% unique %>% set_names(as.yearqtr(.)) ,
                                       selected = out_short$startdt %>% unique %>% last),
                        selectizeInput('enddt', 'Выберите правую границу тренировочной выборки',
                                       out_short$enddt %>% unique %>% set_names(as.yearqtr(.))),
                        
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
                        
                        
                        actionButton("update", "Произвести расчёты")
                      ),
                      mainPanel(
                        plotOutput('forecast'),
                        dataTableOutput('score')
                      ))),
           tabPanel('Все прогнозы',
                    sidebarLayout(sidebarPanel(
                      selectizeInput('startdt_hair', 'Выберите левую границу тренировочной выборки',
                                     out_short$startdt %>% unique %>% set_names(as.yearqtr(.)) ,
                                     selected = out_short$startdt %>% unique %>% last),
                      helpText('Для каждой модели используется оптимальное количество лагов'),
                      sliderInput('h_hair',
                                   'Выберите  горизонт прогнозирования (кварталов)',
                                   value =  c(out_short$h %>% min, out_short$h %>% median),
                                   min = out_short$h %>% min, 
                                   max = out_short$h %>% max,
                                  step = 1),
                      
                      checkboxInput('actualforecast',
                                    'Показывать только наиболее актуальные прогнозы', value = TRUE),
                      
                      
                      selectizeInput('model_hair', 'Выберите модель',
                                     choices = out_short$model %>% unique,
                                     selected = out_short$model %>% unique %>% first, 
                                     
                                     multiple = TRUE),
                      # checkboxInput('onlytrain_hair', 'Показывать только тестовую выборку', value = TRUE),
             #          radioButtons('scoretype_hair',
             #                       "Выберите тип представления RMSFE",
             #                       choices = c("Абсолютные значения" = 'absolute',
             #                                   "Значения относительно базовой модели" = 'relate'), 
             #                       selected = 'relate'),
             #          helpText("Для корректного сравнения
             # тренировочных выборок с разными границами 
             # RMSFE рассчитывается только по первым 12 наблюдениям тестовой выборки
             # (т.е. только для первых 3 лет)."),
             
             
             sliderValues(
               inputId = "forecastdate", label = "Дата прогноза", width = "100%",
               values = choises_q,
               from = choises_q[1],
               to = NULL,
               dragRange = TRUE,
               grid = FALSE,
               animate = animationOptions(interval = 1500)
             ),
            
             # sliderInput('forecastdate', 'Дата прогноза',
             #             min = out_hair$forecastdate[which(out_hair$forecastdate > min(out_hair$enddt))] %>% min,
             #             max = out_hair$forecastdate %>% max,
             #             value = out_hair$forecastdate[which(out_hair$forecastdate > min(out_hair$enddt))] %>% min,
             #             step = 92,animate = TRUE,
             #             timeFormat = '%b %Y'),
                      hr(),
                      actionButton("update_hair", "Произвести расчёты")
                      
                      
                      
                    ),
                    mainPanel(plotOutput('hair') )))
)