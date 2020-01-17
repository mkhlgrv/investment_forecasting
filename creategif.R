

create_gif <- function(models = 'Boosting', startdt = '1996-01-01', h_min = 0L, h_max = 4L){
df <- out_hair %>%
    dplyr::filter(
      model %in% models,
      startdt %in%  (startdt %>% as.Date),
      h >= h_min,
      h <= h_max
    )

df_true <- out_hair %>%
  dplyr::filter(
    model %in% models,
    startdt %in%  (startdt %>% as.Date),
    h <= h_max
  ) %>% 
  na.omit

print(df)
  df %>%
      ggplot()+
      
      geom_line(data = df %>%
                  na.omit
                ,mapping = aes(x = date,
                               y = true),
                color='grey',
                size = 2,
                linetype = 'solid',
                alpha = 0.5)#+
    # geom_line(aes(x = date,
    #               y = pred,
    #               group = forecastdate),
    #           linetype = 'dashed',
    #           color = 'cornflowerblue',
    #           alpha = 0.8,
    #           size = 0.8)+
    # facet_grid(startdt~model)+
    #   labs(title = "",
    #        y = "Изменение инвестиций (log)",
    #        x = "Дата")+
    #   scale_size_manual(values = 1) +
    #   guides(colour = guide_legend(""),
    #          size = guide_legend(""),
    #          linetype = guide_legend(""),
    #          fill = guide_legend(" "))+
    #   theme_minimal()+
    #   theme(legend.position = "none") +
    #   scale_x_date(limits = c(df %>% pull(date) %>% min,
    #                           df %>% pull(date) %>% max)) +
    #   scale_y_continuous(limits = c(df %>% select(true, pred) %>% unlist %>% na.omit %>% min,
    #                                 df %>% select(true, pred) %>% unlist %>% na.omit %>% max))+
    #       transition_reveal(giftime) +
    #       ease_aes('linear')
    # 
  
  
}
create_gif()
