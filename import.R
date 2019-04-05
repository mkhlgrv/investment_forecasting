rm(list = ls())
source("lib.R")
source("fun.R")
# Data import ----

## Pre-import ----
# Росстат - ВВП по использованию - в постоянных ценах + поквартальный цепной индекс валовые инвестиции в основной капитал
investment_raw <- xts(read.csv("investment.csv", dec = ","),
                      order.by = seq(as.Date("1995-01-01"), as.Date("2018-12-31"), by = "quarter") %>% as.yearqtr()) 
# Индекс реальных инвестиций считается квартальный только с 2007 года, до этого считался только месячный, поэтому совместим их
# plot(stl(investment_raw, s.window = 4))
# 
# 
# #INVFC_Q_DIRIAW
# invy <- sophisthse(series.name = c("INVFC_Y_DIRI"), output = "zoo")[,1]
# invq <- sophisthse(series.name = c("INVFC_Q_DIRI"), output = "zoo")[,1]
# invm <- sophisthse(series.name = c("INVFC_M_DIRI"), output = "zoo")[,1]
# autoplot(invq)
# autoplot(invm)
# invm2q <- zoo(x = NA, order.by = zoo::as.yearqtr((time(invm))) %>% unique)
# for(i in seq(1,length(invm),3)){
#   invm2q[(i-1)/3+1] <- sum(invm[i:(i+2)])
# }
# 
# invest <- c(as.xts(invm2q*87.1/452.6) %>%
#   window(start = as.yearqtr("1994-01"),
#          end = as.yearqtr("2006-04")),
# as.xts(invq))
# 
# plot(stl(df$invest, s.window = 4))
# Загрузим дата фрейм (предварительно), что бы было легче определить, какие переменные будем использовать
tsnames <- series_info$table %>%
  split(seq(length(series_info$table))) %>%
  map_dfr(function(x){
    tn <- strsplit(x, "_")[[1]]
    if(length(which(tn %in% c("M", "Q", "Y"))) != 0){
      place <- which(tn %in% c("M", "Q", "Y"))[1] -1 
    } else{
      place <- 1
    }
    tibble(table = x, tabname = paste0(tn[1:place], collapse = "_"))
  }) %>% inner_join(series_info) %>%
  group_by(tabname) %>%
  #group_by(table) %>%
  mutate(minf = min(freq), maxf = max(freq), meaf = median(freq)) %>%
  filter(!(minf == 1& maxf == 1)) %>%
  #filter(minf == 12) %>%
  filter(!grepl("20", unit),
         !grepl("SA", tsname)) %>%
  #filter(!grepl(pattern = "_Y_|_Y$", table)) %>%
  unique()
tsnames4 <- tsnames %>% filter(freq == 4)
tsnames12 <-tsnames %>% filter(minf == 12) 
tsnames4 %>% group_by(tabname) %>% filter(n()>1) %>% View
df_raw <- sophisthse(series.name = series$tsname %>% unique, output = "zoo")%>% as.xts()
missmap(df_raw)
df_raw %>% as.xts %>% .['1995-01/2018-04'] %>% missmap

nonmis <- sapply(df, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` == 0) %>%
  pull(tname)


df[,nonmis] %>% missmap
# пропущенных значений нет
missmap(df)
# теперь определим ряды, у которых уже есть пара в виде очищенного от сезонности ряда
nonsa <- expand.grid(cn1 = colnames(df[,-1]), cn2 = colnames(df[,-1]), stringsAsFactors = FALSE) %>%
  as_tibble %>%
  mutate(remove = ifelse(cn1 == paste0(cn2, "_SA"),cn2, ifelse(cn2 == paste0(cn1, "_SA"),cn1, NA))) %>%
  na.omit %>%
  pull(remove) %>%
  unique # добавим время и UNEMPL_M
df <- df[,which(colnames(df) %in% nonmis &! colnames(df) %in% c(nonsa,"INVFC_Q_DIRI_SA"))]

# получим 35 рядов

save(df, file = "rawdata.RData")
load("rawdata.RData")

## Transfrom ----
# Трансформируем ряды (приведём к стационарном виду).

# Попрбобуем трансформировать данные еще сильнее. Добавим лагированные переменные
# все переменные залагованы на 6
df_tf_lag <- create.lagv(df_tf, nlag = 6L)

save(df_tf, df_tf_type,df_tf_lag, file = "tfdata.RData")
rm(list=ls())
