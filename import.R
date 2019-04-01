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
