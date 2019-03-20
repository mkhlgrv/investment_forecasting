source("lib.R")

# AR data import ----


# Во-первых, очистим данные от сезонности и тренда

# addictive 
y_dec <- sophisthse(series.name = "UNEMPL_M_SH", output = "zoo") %>%
    #window(start = zoo::as.yearmon("2002-01-01"),
    #     end = zoo::as.yearmon("2018-12-01")) %>% 
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

series <- series_info %>%
  filter(freq == 12)

df_raw <- sophisthse(series.name = series$table %>% unique, output = "data.frame")
df <- df_raw %>%
  filter(!is.na(UNEMPL_M_SH),
         `T` >= zoo::as.yearmon("2002-01-01"),
         `T` <= zoo::as.yearmon("2016-12-01"))

nonmis <- sapply(df, function(y) sum(length(which(is.na(y))))) %>%
  data.frame %>%
  rownames_to_column("tname") %>%
  filter(`.` == 0) %>%
  pull(tname)


df %<>% select(nonmis)
# пропущенных значений нет
missmap(df)
save(df, file = "rawdata_panel.RData")


# Но многие ряды повторяются. Исключим ряды с сильно коррелированными значениями
# Позднее мы проверим некоторые модели и на неочищенных данных

# корреляции
corpairs <- list()
cormat <- cor(df[,-1])
for(i in 1:nrow(cormat)){
  for(j in i:ncol(cormat)){
    if(cormat[i,j]>=0.99)
      corpairs <- rlist::list.append(corpairs, data.frame(cn1 = colnames(cormat)[i],cn2= colnames(cormat)[j]))
  }
}
corpairs %>%
  bind_rows %>%
  arrange(cn1) %>% 
  inner_join(series_info %>% select(tsname, fullname), by = c("cn1" = "tsname")) %>% setnames("fullname", "n1")%>%
  inner_join(series_info %>% select(tsname, fullname), by = c("cn2" = "tsname")) %>% setnames("fullname", "n2")
