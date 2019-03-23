rm(list = ls())
source("lib.R")
source("fun.R")
# Data import ----

## Pre-import ----
# Загрузим дата фрейм (предварительно), что бы было легче определить, какие переменные будем использовать
series <- series_info %>%
  filter(freq == 12)

df_raw <- sophisthse(series.name = series$table %>% unique, output = "data.frame")
df <- df_raw %>%
  filter(!is.na(UNEMPL_M_SH),
         `T` >= zoo::as.yearmon("2002-11-01"),
         `T` <= zoo::as.yearmon("2015-12-01"))

missmap(df)
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
  as_tibble %>%
  mutate(remove = ifelse(cn1 == paste0(cn2, "_SA"),cn2, ifelse(cn2 == paste0(cn1, "_SA"),cn1, NA))) %>%
  na.omit %>%
  pull(remove) %>%
  unique # добавим время и UNEMPL_M
df %<>% select(-nonsa)

# получим 84 ряда

# эти ряды будем использовать при скачивании данных формате zoo
## Import ----
df <- sophisthse(series.name = nonmis[which(!nonmis %in% c("T","UNEMPL_M"))], output = "zoo") %>%
  window(start = zoo::as.yearmon("2001-11-01"),
       end = zoo::as.yearmon("2017-12-01"))
# удалим те ряды, у которые есть пара, уже очищенная от сезонности
df %<>% .[,which(!names(.) %in% nonsa)]
# сохраним сырые данные
save(df, file = "rawdata.RData")
load("rawdata.RData")

## Transfrom ----
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
      x %<>% na.omit()
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
        } else
          {
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
      x_stat <- as.xts(x_stat)
      names(x_stat) <- i
      list(typedf = data.frame(tsname = i, type = type, stringsAsFactors = FALSE),
           statdf = x_stat)
    })
  statdf <- do.call(merge.xts, result %>% map(function(x){
    x$statdf
  }))
  typedf <- result %>% map_dfr(function(x){
    x$typedf
  })
  list(df = statdf, type = typedf)
}
stat_out <- get.stationary.panel(df)
# получаем транcформированные ряды
df_tf <- stat_out$df
# данные о типе трансформации
df_tf_type <- stat_out$type
# Пропущенные значения (есть по краям, ничего страшного (крайние данные были нужны только для безработицы))
missmap(df_tf)


# Попрбобуем трансформировать данные еще сильнее. Добавим лагированные переменные
# все переменные залагованы на 6
df_tf_lag <- create.lagv(df_tf, nlag = 6L)

save(df_tf, df_tf_type,df_tf_lag, file = "tfdata.RData")
rm(list=ls())
