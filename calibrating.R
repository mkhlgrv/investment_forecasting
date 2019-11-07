source('lib.R')
load('data/raw.Rdata')

# # доходность ртс, процентные ставки на межбанковском рынке, спреды по облигациям,
# темпы роста номинального эффективного курса, реального эффективного курса, прирост цен на нефть,
# 4 разность логарифма ВВП и ИПЦ, отношение номинальных инвестиций к номинальному ввп + лаги
series <- c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO', #'gov_1y',
            'reer', 'neer', 'oil', 'rts',
            'CPI_Q_CHI', 'GDPEA_Q_DIRI',
            'GDPEA_C_Q', 'INVFC_Q',
            'EMPLDEC_Q',
          
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


# extended series ----

eseries <- c('investment', 'mkr_1d','mkr_7d','gov_6m','GKO',
  'reer', 'neer', 'oil', 'rts',
  
  'GDPEA_C_Q', 'INVFC_Q', # служебные для invest2gdp
  
  
  'CPI_Q_CHI',# ипц
  'GDPEA_Q_DIRI', # ввп
  
  'UNEMPL_Q_SH',# безработица
  'EMPLDEC_Q', # заявленная потребность в работника
  'CONSTR_Q_NAT', # индекс строительно-монтажных работ, 
  'WAG_Q', # зарплата
  'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
  'CTI_Q_CHI', # индекс тарифов на грузовые перевозки
  'AGR_Q_DIRI', # индекс сельхоз производства
  #'CNSTR_Q_DIRI',# индекс работ в строительстве
  'RTRD_Q_DIRI', # оборот розничной торговли
  'HHI_Q_DIRI',# индекс реальных денежных доходов населения
  'M0_Q', # M0
  'M2_Q',# М2
  'CBREV_Q',# доходы конс. бюджета 
  'CBEX_Q',# расходы конс. бюджета
  'FBREV_Q',# доходы фед. бюджета
  'FBEX_Q',# расходы фед. бюджета
  'RDEXRO_Q',# официальный курс доллара
  'RDEXRM_Q',# курс доллара на ммвб
  'LIAB_T_Q',# кредиторская задолженность в среднем за период
  'LIAB_UNP_Q',# просроченная кредиторская задолженность в среднем за период
  'LIAB_S_Q',# кредиторская задолженность поставщикам в среднем за период
  'LIAB_B_Q',# кредиторская задолженность в бюджет в среднем за период
  'DBT_T_Q',#дебиторская задолженность в среднем за период
  'DBT_UNP_Q',#просроченная дебиторская задолженность в среднем за период
  # 'DBT_P_Q',# дебиторская задолженность покупателей в среднем за период
  'EX_T_Q',# экспорт
  'IM_T_Q',# импорт
  'PPI_EA_Q' # (после 2004-01)
  )


df <- rawdata[,eseries]
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
           # 'deflator', только с 1996
           'GDPEA_Q_DIRI',
           
           'EMPLDEC_Q',
           'UNEMPL_Q_SH',
           
           'CONSTR_Q_NAT', 
           ###### 'TRP_Q_PASS_DIRI', 
           'WAG_Q', 
           'CONI_Q_CHI', 
           'CTI_Q_CHI', 
           'AGR_Q_DIRI', 
           'RTRD_Q_DIRI', 
           'HHI_Q_DIRI',
           'M0_Q', 
           'M2_Q',
           ####   'IR_Q',
           #### 'ICR_Q',
           'CBREV_Q',
           'CBEX_Q',
           'FBREV_Q',
           'FBEX_Q',
           'RDEXRO_Q',# официальный курс доллара
           'RDEXRM_Q',# курс доллара на ммвб
           'LIAB_T_Q',# кредиторская задолженность в среднем за период
           'LIAB_UNP_Q',# просроченная кредиторская задолженность в среднем за период
           'LIAB_S_Q',# кредиторская задолженность поставщикам в среднем за период
           'LIAB_B_Q',# кредиторская задолженность в бюджет в среднем за период
           'DBT_T_Q',#дебиторская задолженность в среднем за период
           'DBT_UNP_Q',#просроченная дебиторская задолженность в среднем за период
           ########## 'DBT_P_Q',# дебиторская задолженность покупателей в среднем за период
           'EX_T_Q',# экспорт
           'IM_T_Q',# импорт
           'PPI_EA_Q' # (после 2004-01)
           
          
           
           )){
  print(i)
  df[,i] %<>% diff.xts(lag = 4, log=TRUE)
}
#df['1996-01/2019-06'] %>% missmap()

save(df, file = 'data/stationary_data_ext.RData')


