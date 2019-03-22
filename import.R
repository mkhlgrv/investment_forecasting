source("lib.R")

# AR data import ----


# Во-первых, очистим данные от сезонности и тренда

# addictive 
y_dec <-sophisthse(series.name = "UNEMPL_M_SH", output = "zoo") %>%
    #window(start = zoo::as.yearmon("2002-01-01"),
    #     end = zoo::as.yearmon("2016-12-01")) %>% 
  decompose()
y <- y_dec$x - y_dec$seasonal
autoplot(y)
# adf test выбираем addictive seasoanlity
y <- log(y) - log(stats::lag(y, -1, na.pad = TRUE))
autoplot(y)
# кажется этого достаточно
acf(y, lag.max = length(y))
pacf(y, lag.max = length(y))
adf.test(y)
# гипотеза о единичном корне отвергается


y_dates <- zoo::as.yearmon(time(y))
save(y, y_dates, file = "rawdata_ar.RData")



# Panel data import ----

# Загрузим дата фрейм (предварительно), что бы было легче определить, какие переменные будем использовать
series <- series_info %>%
  filter(freq == 12)

df_raw <- sophisthse(series.name = series$table %>% unique, output = "data.frame")
df <- df_raw %>%
  filter(!is.na(UNEMPL_M_SH),
         `T` >= zoo::as.yearmon("2001-11-01"),
         `T` <= zoo::as.yearmon("2017-01-01"))

nonmis <- sapply(df, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` == 0) %>%
  pull(tname)


df %<>% select(nonmis)
# пропущенных значений нет
missmap(df)
# теперь определим ряды, у которых уже есть пара в виде очищенного от сезонности ряда
nonsa <- expand.grid(cn1 = colnames(df[,-1]), cn2 = colnames(df[,-1]), stringsAsFactors = FALSE) %>%
  as.tibble %>%
  mutate(remove = ifelse(cn1 == paste0(cn2, "_SA"),cn2, ifelse(cn2 == paste0(cn1, "_SA"),cn1, NA))) %>%
  na.omit %>%
  pull(remove) %>%
  unique # добавим время и UNEMPL_M
df %<>% select(-nonsa)

# получим 84 ряда

# эти ряды будем использовать при скачивании данных формате zoo

df <- sophisthse(series.name = nonmis[which(!nonmis %in% c("T","UNEMPL_M"))], output = "zoo") %>%
  window(start = zoo::as.yearmon("2001-11-01"),
       end = zoo::as.yearmon("2017-01-01"))
# удалим те ряды, у которые есть пара, уже очищенная от сезонности
df %<>% .[,which(!names(.) %in% nonsa)]
# сохраним сырые данные
save(df, file = "rawdata_panel.RData")
# Трансформируем ряды (приведём к стационарном виду). Это делать не обязательно, поэтому позднее для
# некоторых моделей попробуем не использовать трансформацию
# ряд GKO_M имеет пропуск
df$GKO_M <- na.locf(df$GKO_M)
get.stationary.panel <- function(df){
  dates <- time(df)
  result <- df %>%
    as.xts %>%
    as.list %>%
    imap(function(x,i){
      x_dec <- decompose(x)
      x <- x_dec$x - x_dec$seasonal
      # проводим adf test для 
      # проводим adf test для простой разности
      d0 <- adf.test(x) %>% .$p.value
      if(d0<=0.05){
        type <-  "d0"
        x_stat <- x
      } else{
        d1 <- adf.test(diff.xts(x) %>% na.omit) %>% .$p.value
        if(d1<=0.05){
          type <-  "d1"
          x_stat <- diff.xts(x)
        } else{
          log_d1 <- 1
          try({
            log_d1 <- adf.test(diff.xts(log(x)) %>% na.omit) %>% .$p.value
          })
          if(log_d1<=0.05){
            type <- "log_d1"
            x_stat <- diff.xts(log(x))
          } else{
            d2 <- adf.test(diff.xts(x, differences = 2) %>% na.omit) %>% .$p.value
            if(d2<=0.05){
              type <- "d2"
              x_stat <- diff.xts(diff.xts(x))
            } else{
              # сообщений нет
              message(i)
              message(min(c(d0, d1, log_d1, d2)))
              x_stat = x
            }
          }
        }
      }
      list(typedf = data.frame(tsname = i, type = type, stringsAsFactors = FALSE),
           statdf = data.frame(x_stat, stringsAsFactors = FALSE) %>% set_names(i))
    })
  statdf <- result %>% map_dfc(function(x){
    x$statdf
  }) %>% as.zoo(order.by = dates)
  typedf <- result %>% map_dfr(function(x){
    x$typedf
  })
  list(df = statdf, type = typedf)
}
# получаем транcформированные ряды
df_tf <- get.stationary.panel(df)$df
# данные о типе трансформации
df_tf_type <- get.stationary.panel(df)$type

save(df_tf, df_tf_type, file = "tfdata_panel.RData")
rm(list=ls())
