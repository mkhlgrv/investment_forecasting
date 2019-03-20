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



# корреляции
# corpairs <- list()
# cormat <- cor(df[,-1])
# for(i in 1:nrow(cormat)){
#   for(j in i:ncol(cormat)){
#     if(i != j & cormat[i,j]>=0.9)
#       corpairs <- rlist::list.append(corpairs, data.frame(correl = cormat[i,j], cn1 = colnames(cormat)[i],cn2= colnames(cormat)[j]))
#   }
# }
nonsa <- expand.grid(cn1 = colnames(df[,-1]), cn2 = colnames(df[,-1]), stringsAsFactors = FALSE) %>%
  as.tibble %>%
  mutate(remove = ifelse(cn1 == paste0(cn2, "_SA"),cn2, ifelse(cn2 == paste0(cn1, "_SA"),cn1, NA))) %>%
  na.omit %>%
  pull(remove) %>%
  unique %>%
  c(. , "UNEMPL_M")
df %<>% select(-nonsa)
missmap(df)
