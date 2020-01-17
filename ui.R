navbarPage('Прогнозирование инвестиций',
           # tabPanel('Сравнение моделей',
           #          sidebarLayout(
           #            sidebarPanel(
           #              selectizeInput('startdt', 'Выберите левую границу тренировочной выборки',
           #                             out_short$startdt %>% unique %>% set_names(as.yearqtr(.)) ,
           #                             selected = out_short$startdt %>% unique %>% last),
           #              selectizeInput('enddt', 'Выберите правую границу тренировочной выборки',
           #                             out_short$enddt %>% unique %>% set_names(as.yearqtr(.))),
           #              
           #              numericInput('lag',
           #                           'Выберите количество лагов в модели (кварталов)',
           #                           value = out_short$lag %>% median,
           #                           min = out_short$lag %>% min, 
           #                           max = out_short$lag %>% max),
           #              checkboxInput('optlag', 'Использовать для каждой модели
           #        оптимальное на тренировочной выборке количество лагов', value = TRUE),
           #              numericInput('h',
           #                           'Выберите горизонт прогнозирования (кварталов)',
           #                           value =  out_short$h %>% median,
           #                           min = out_short$h %>% min, 
           #                           max = out_short$h %>% max),
           #              selectizeInput('model', 'Выберите модель',
           #                             choices = out_short$model %>% unique,
           #                             selected = out_short$model %>% unique %>% first, 
           #                             multiple = TRUE),
           #              checkboxInput('onlytrain', 'Показывать только тестовую выборку', value = TRUE),
           #              radioButtons('scoretype',
           #                           "Выберите тип представления RMSFE",
           #                           choices = c("Абсолютные значения" = 'absolute',
           #                                       "Значения относительно базовой модели" = 'relate'), 
           #                           selected = 'relate'),
           #              
           #              helpText("Для корректного сравнения
           #   тренировочных выборок с разными границами 
           #   RMSFE рассчитывается только по первым 12 наблюдениям тестовой выборки
           #   (т.е. только для первых 3 лет)."),
           #              hr(),
           #              
           #              
           #              actionButton("update", "Произвести расчёты")
           #            ),
           #            mainPanel(
           #              plotOutput('forecast'),
           #              dataTableOutput('score')
           #            ))),
           tabPanel('Все прогнозы',
                      sidebarLayout(sidebarPanel(
                      selectizeInput('startdt_hair', 'Выберите левую границу тренировочной выборки',
                                     out_short$startdt %>% unique %>% set_names(as.yearqtr(.)) ,
                                     selected = out_short$startdt %>% unique %>% last,
                                     multiple = TRUE),
                      conditionalPanel('false',
                      radioButtons('startdt_type_hair', '',
                                   choices = c('на отдельном графике' = 'divide',
                                               'на одном графике' = 'together',
                                               'среднее' = 'mean')
                      )
                                   ),
                      hr(),
                      selectizeInput('model_hair', 'Выберите модель',
                                     choices = out_short$model %>% unique,
                                     selected = out_short$model %>% unique %>% first, 
                                     multiple = TRUE,
                                     options = list(maxItems = 2)),
                      helpText('Можно выбрать не больше 2 моделей'),
                      conditionalPanel('false',
                      radioButtons('model_type_hair', '',
                                   choices = c('на отдельном графике' = 'divide',
                                               'на одном графике' = 'together',
                                               'среднее' = 'mean'),
                                   selected = 'divide'
                                   )
                      ),
                      
                      sliderInput('h_hair',
                                   'Выберите горизонт прогнозирования (кварталов)',
                                   value =  c(out_short$h %>% min, out_short$h %>% median),
                                   min = out_short$h %>% min, 
                                   max = out_short$h %>% max,
                                  step = 1),
                      conditionalPanel('false',
                      radioButtons('play','Воспроизведение',
                                   choices = c('Статика' = 'stat',
                                               'Динамика' = 'dinamic'))
                      ),
                      # downloadButton('downloadgif', 'Загрузить gif'),
                      downloadButton('downloadpng', 'Загрузить png'),
                      # checkboxInput('actualforecast',
                      #               'Показывать только наиболее актуальные прогнозы', value = TRUE),
                      # 
                      # selectizeInput('model_hair', 'Выберите модель',
                      #                choices = out_short$model %>% unique,
                      #                selected = out_short$model %>% unique %>% first, 
                      #                
                      #                multiple = TRUE),

                      # radioButtons('facet', 'Тип представления',choices = c('Каждую модель на отдельном графике' = 'divide',
                      #                                                       'Все модели на одном графике' = 'together',
                      #                                                       'Усредненный прогноз' = 'mean')
                      #              , selected = 'divide'),
                      
                      
                      
             #                       selected = 'relate'),
             #          helpText("Для корректного сравнения
             # тренировочных выборок с разными границами 
             # RMSFE рассчитывается только по первым 12 наблюдениям тестовой выборки
             # (т.е. только для первых 3 лет)."),
             
             # sliderTextInput(inputId = "forecastdate", 
             #                 label = "Дата прогноза",
             #                 choices = choises_q,selected = choises_q[1],
             #                 animate = animationOptions(interval = 700)
             #                  ),
      
                      hr(),
                      actionButton("update_hair", "Обновить")
                      
                      
                      
                    ),
                    mainPanel(imageOutput('hair') )))
)