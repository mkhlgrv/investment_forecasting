source('lib.R')
load('data/raw.Rdata')

# # доходность ртс, процентные ставки на межбанковском рынке, спреды по облигациям,
# темпы роста номинального эффективного курса, реального эффективного курса, прирост цен на нефть,
# 4 разность логарифма ВВП и ИПЦ, отношение номинальных инвестиций к номинальному ввп + лаги
series <- c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO', #'gov_1y',
            'reer', 'neer', 'oil', 'rts',
            'CPI_Q_CHI', 'GDPEA_Q_DIRI', 'GDPEA_C_Q', 'INVFC_Q')
df <- rawdata[,series]
# отношение номинальных инвестиций к номинальному ВВП
df$gdp2invest <- df$GDPEA_C_Q/df$INVFC_Q

# удалить лишнее
df$GDPEA_C_Q <- df$INVFC_Q <- NULL

# вставить gko в 6m gov bond yield
df$gov_6m[which(is.na(df$gov_6m))] <- 
  df$GKO[which(is.na(df$gov_6m))]

df$GKO <- NULL

# нестационарные ряды без сезонности (разность) ----
for(i in c('reer','neer','oil','rts')){
  df[,i] %<>% diff.xts(log=TRUE)
}

# нестационарные ряды с сезонностью (4-ая разность) ----
for(i in c('investment', 'CPI_Q_CHI',
           'GDPEA_Q_DIRI')){
  df[,i] %<>% diff.xts(lag = 4, log=TRUE)
}

# (4-я разность)
df$gdp2invest %<>% diff.xts(lag = 4)

create_lag <- function(df, lag){
  out <- df
  for(i in 1:lag){
    x <- lag.xts(df, k = i)
    colnames(x) <- paste0(colnames(x), "_lag", i)
    out %<>% merge.zoo(x)
  }
  return(out %>% as.xts)
}
df_lag <- create_lag(df)
