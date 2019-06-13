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
  filter(
         !grepl("SA", tsname)) %>%
  #filter(!grepl(pattern = "_Y_|_Y$", table)) %>%
  unique()
tsnames4 <- tsnames %>% filter(!grepl("20", unit),freq == 4)
tsnames12 <- tsnames %>% filter(minf == 12,!grepl("19", unit))
# tsnames4 %>% group_by(tabname) %>% filter(n()>1) %>% View
# список квартальных переменных, которые надо исключить из рассмотрения
# c("UNEMPL_Q","GDP_Q_C", "IND_Q", "CNSTR_Q_M", "CONSTR_C_Q", "TRP_Q_CARG", "RTRD_Q", "WAG_C_Q", "HHI_Q", "	INVFC_Q", "INVFC_Q_DIRI")
# такой же список для месячных переменных
# tsnames12 %>% View
# "OILREF_C" в названии ошибка поэтому надо использовать "OILREF_C_SA"

df_raw4 <- sophisthse(series.name = tsnames4$tsname %>% unique, output = "zoo")
df4 <- df_raw4[,which(colnames(df_raw4) %in% (tsnames4$tsname %>% unique)&
                 !colnames(df_raw4) %in% c("UNEMPL_Q","GDP_Q_C", "IND_Q", "CNSTR_Q_M",
                                           "CONSTR_C_Q", "TRP_Q_CARG", "RTRD_Q", "WAG_C_Q",
                                           "HHI_Q", "INVFC_Q", "INVFC_Q_DIRI"))] %>%
  as.xts %>% .['1994-01/2019-01'] 

nonmis_long <- sapply(df4, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` <2) %>%
  pull(tname)
df_long <- df4[,nonmis_long]
df4_20 <- df4 %>% .['2000-01/2019-01']
# работа в двух вариантах с использованием рядов с 2000 (df_short) и без использования (df_long)

df_raw12 <- sophisthse(series.name = tsnames12$tsname %>% unique, output = "zoo")
df12 <- df_raw12[,which(colnames(df_raw12) %in% c(tsnames12$tsname %>% unique))] %>%
  as.xts %>% .['2000-01/2019-01']
df_short <- merge.xts(df4_20, df12 %>%
  as.list() %>%
  imap(function(y, namei){
  out <- zoo(x = NA, order.by = zoo::as.yearqtr((time(df4_20))))
  for(i in seq(1,length(y),3)){
    out[(i-1)/3+1] <- sum(y[i:(i+2)])
  } 
  out %<>% xts()
  names(out) <- namei
  out
}) %>% do.call.pipe(merge.xts))
nonmis_short <- sapply(df_short, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` <10) %>%
  pull(tname)

df_short <- df_short[, nonmis_short]




# gdp
gdp_raw <- sophisthse(c("GDP_Q_DIRI", "GDPEA_Q"), output = 'zoo') 
gdp_raw$GDPEA_Q_DIRI[which(is.na(gdp_raw$GDPEA_Q_DIRI))] <-
  gdp_raw$GDP_Q_DIRI[which(is.na(gdp_raw$GDPEA_Q_DIRI))]/1.1515
gdp <-gdp_raw%>% as.xts %>% .[,"GDPEA_Q_DIRI"]

# merge with gdp
df_long <- merge.xts(df_long, gdp) 
df_short <- merge.xts( df_short, gdp['2000-01/2019-01'])


# merge with investment
df_long <- merge.xts(investment_raw, df_long) 
df_short <- merge.xts(investment_raw['2000-01/2019-01'], df_short)

# rts

rts <- read_csv("RTSI-dailyhistory.csv") %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y") %>% as.yearqtr()) %>%
  group_by(Date) %>% filter(row_number()==1) %>% select(1,4) %>% set_names(c("date", "rts"))
df_long <- merge.xts(df_long, xts(rts[,2], order.by = rts$date))


# russian 3 month bills

gov <- read_delim("gov.csv", ";", quote = "'", 
                  escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(`"Дата` = seq.Date(from = as.Date("2001-04-01"),
                         to = as.Date("2019-06-01"),
                         by = "month") %>% rev %>%
           as.yearqtr()) %>%
  group_by(`"Дата`) %>% filter(row_number()==1) %>% select(1,2) %>% ungroup %>% set_names(c("date", "gov"))
df_long <- merge.xts(df_long, xts(gov[,2], order.by = gov$date))

save(df_long, df_short, file = "rawdata.RData")
load("rawdata.RData")
rm(list=ls())


