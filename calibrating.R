source('lib.R')
load('data/raw.Rdata')

# # доходность ртс, процентные ставки на межбанковском рынке, спреды по облигациям,
# темпы роста номинального эффективного курса, реального эффективного курса, прирост цен на нефть,
# 4 разность логарифма ВВП и ИПЦ, отношение номинальных инвестиций к номинальному ввп + лаги
series <- c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO', #'gov_1y',
            'reer', 'neer', 'oil', 'rts',
            'CPI_Q_CHI', 'GDPEA_Q_DIRI',
            'GDPEA_C_Q', 'INVFC_Q',
            'EMPLDEC_Q'
            )
df <- rawdata[,series]
# отношение номинальных инвестиций к номинальному ВВП
df$invest2gdp <- df$INVFC_Q/df$GDPEA_C_Q

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
           'invest2gdp',
           'GDPEA_Q_DIRI',
           
           'EMPLDEC_Q')){
  df[,i] %<>% diff.xts(lag = 4, log=TRUE)
}


save(df, file = 'data/stationary_data.RData')



