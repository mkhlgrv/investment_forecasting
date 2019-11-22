rm(list = ls())
source("lib.R")
source("fun.R")
load('data/stationary_data_ext.RData')

# investment level -----
invest_plot <-
  ggplot(df %>% na.omit) +
  geom_line(aes(y = investment, x = time(df %>% na.omit)))+
  labs(title = "",
       y = "Валовое накопление основного капитала",
       x = "Дата") +
  theme_bw()
cairo_pdf("plot/invest_plot.pdf", width = 10, height = 5)
print(invest_plot)
dev.off()


## all vars plot ----
all_plot <-
  df %>%
  na.omit %>%
  as.data.frame %>%
  rownames_to_column('date') %>%
  melt(id.vars = 'date') %>%
  group_by(variable) %>%
  mutate(value = scale(value),
         date = as.Date(as.yearqtr(date))) %>%
  ggplot() +
  geom_line(aes(date, value, group=variable,
                alpha = ifelse(variable %in% c('investment'), 1, 0.3)), show.legend = F)+
  labs(title = "",
       y = "",
       x = "") +
  scale_x_date(limits = c('1996-01-01', '2019-01-01') %>% as.Date)+
  geom_vline(xintercept =c('1996-01-01','2000-01-01') %>% as.Date, linetype='dashed')+
  scale_y_continuous(limits = c(-5, 3))+
  labs(x = 'Дата',
       y='')+
  theme_bw()
cairo_pdf("plot/allvars.pdf", width = 10, height = 5)
print(all_plot)
dev.off()



##### rmsfe table -----
load('shinydata.RData')


scoredf$model <- factor(scoredf$model,
                        levels = c("Random Walk","AR","Adaptive LASSO",'Boosting',
                                   "Elastic Net","LASSO","Post-LASSO",
                                   "Random Forest","Ridge","Spike and Slab"))


scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '1996-01-01') %>%
  filter(lag==0) %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)

scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '2000-01-01') %>%
  filter(lag==0) %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)

#### dm test ----


dmdf <- get.dm(out_true %>%filter(lag==0) %>% na.omit)


dmdf %>% 
  mutate(pvalue_sign = paste0(round(pvalue,3), '(', ifelse(stat < 0,
                                                           '+',
                                                           '-'),')')) %>%
  dcast(model~h) %>%
  xtable %>%
  print(include.rownames = FALSE)


dmsd <- dmdf %>%
  filter(model != 'Random Walk') %>%
  mutate(Изменение = ifelse(pvalue > 0.05,
                            '0',
                            ifelse(stat < 0,
                                   '+',
                                   '-')
  )) %>%
  ggplot(aes( model,factor(h))) +
  geom_tile(aes(fill = Изменение),color='grey')+
  theme_bw()+
  labs(x = 'Модель',
       y = 'Горизонт прогнозирования')

cairo_pdf('plot/dmsd.pdf')
print(dmsd)
dev.off()



# lasso coefs ----
# сначала надо найти sd каждой переменной в каждой тренировочной выборке и поделить на него коэффициент
load('data/stationary_data_ext.RData')
sddata <- expand.grid(startdt = c(as.Date('1996-01-01'), as.Date('2000-01-01')),
                      enddt = seq(as.Date('2012-10-01'), as.Date('2018-10-01'), by = 'quarter')
) %>%
  split(seq(1:nrow(.))) %>%
  map_dfr(function(x){
    
    df %>% na.omit %>% as.data.frame() %>%
      rownames_to_column('date')%>%
      mutate(date = as.yearqtr(date) %>% as.Date) %>%
      filter(date >= x$startdt, date <= x$enddt) %>%
      select(-date) %>%
      sapply( sd) %>%
      as.data.frame %>%
      t %>%
      as.tibble %>%
      mutate(enddt = x$enddt,
             startdt = x$startdt, .)
    
  })


# вычисление предикторов lasso ----
source('fun.R', encoding = 'utf-8')
source('lib.r')

load('out/full/out_lasso.RData')
lasso_beta <- 
  out_lasso %>%
  plyr::compact()%>%
  map_dfr(
    
    function(x, i){
      
      if(x$h == 0){
        actsd <- sddata %>% filter(startdt == x$startdt,
                                   enddt == x$enddt) %>%
          select(-c(investment, startdt, enddt, invest2gdp))
      } else{
        actsd <- sddata %>% filter(startdt == x$startdt,
                                   enddt == x$enddt) %>%
          select(-c(startdt, enddt))
      }
      
      
      
      betaval = x$model_fit$beta
      
      if(!all((betaval%>% rownames) ==(actsd %>% colnames))){
        print(actsd %>% colnames)
        print(betaval%>% rownames)
        stop()
      }
      data.frame(model = x$model,
                 h = x$h,
                 startdt=x$startdt,
                 enddt = x$enddt,
                 predictor = betaval%>% rownames,
                 beta = (betaval%>% as.numeric)/(actsd[1,] %>% as.numeric)
      )
      
    }
  )



lasso_nonzero <- lasso_beta %>%
  mutate(startdt = factor(startdt, c('2000-01-01','1996-01-01'),
                          labels = c('2000.I', '1996.I'))) %>%
  group_by( h, startdt, enddt) %>%
  summarise(nz = sum(beta != 0)) %>%
  ggplot(aes(enddt, nz, linetype = startdt))+
  geom_line()+
  labs(title = "",
       y = "Количество переменных",
       x = "Дата",
       color = '',
       linetype = 'Левая граница\nвыборки')+
  facet_wrap(vars(h), scales = 'free_y')+
  theme_bw()

# количество переменных

cairo_pdf('plot/lasso_nonzero.pdf')
print(lasso_nonzero)
dev.off()


lasso_p <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
    startdt== '2000-01-01'
    ,
    predictor %in% c(
      'investment',
      'mkr_1d',
      'mkr_7d',
      'gov_6m',
      'GKO',
      'invest2gdp',
      'oil', 
      'rts',
      'GDPEA_Q_DIRI'
      #,
      #'RTRD_Q_DIRI',
      #'EMPLDEC_Q',
      #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
      #'CNSTR_Q_DIRI'# индекс работ в строительств
    )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta, color=interaction(predictor,startdt)))+
  facet_wrap(vars(h))

plotly::ggplotly(lasso_p)

# lasso coefs h <=4
lasso_beta %>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(predictor, h, startdt) %>%
  
  filter(startdt== '2000-01-01') %>%
  filter(h<=4) %>%
  group_by(predictor, h) %>%
  summarise(beta = mean(beta)/100) %>%
  ungroup %>%
  group_by(h) %>%
  arrange(desc(abs(beta))) %>%
  mutate(rn = row_number(),
         pred_beta = paste0(predictor,' ', round(beta,3))) %>%
  filter(rn<=5) %>%
  ungroup %>%
  dcast(rn~h, value.var = 'pred_beta') %>%
  xtable %>%
  print(include.rownames = FALSE)

# lasso coefs h >4
lasso_beta %>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(predictor, h, startdt) %>%
  
  filter(startdt== '2000-01-01') %>%
  filter(h>4) %>%
  group_by(predictor, h) %>%
  summarise(beta = mean(beta)/100) %>%
  ungroup %>%
  group_by(h) %>%
  arrange(desc(abs(beta))) %>%
  mutate(rn = row_number(),
         pred_beta = paste0(predictor,' ', round(beta,3))) %>%
  filter(rn<=5) %>%
  ungroup %>%
  dcast(rn~h, value.var = 'pred_beta') %>%
  xtable %>%
  print(include.rownames = FALSE)




# ВВП

gdp <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h<2,
         startdt== '2000-01-01',
         predictor %in% c(
           'GDPEA_Q_DIRI'
           #,
           #'RTRD_Q_DIRI',
           #'EMPLDEC_Q',
           #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
           #'CNSTR_Q_DIRI'# индекс работ в строительств
         )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)/100) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(rows = vars(h), scales = 'free')+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/gdp.pdf')
print(gdp)
dev.off()




invest <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h==1,
         startdt== '2000-01-01'
         ,
         predictor %in% c(
           'investment'#,
           #    'mkr_1d',
           #    'mkr_7d',
           #    'gov_6m',
           #    'GKO',
           #    'invest2gdp',
           # 'oil', 
           # 'rts',
         )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)/100) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(rows = vars(h), scales = 'free')+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/invest.pdf')
print(invest)
dev.off()



mkr_7d <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h>0,h<4,
         startdt== '2000-01-01'
         ,
         predictor %in% c(
           'mkr_7d'
           #    'mkr_7d',
           #    'gov_6m',
           #    'GKO',
           #    'invest2gdp',
           # 'oil', 
           # 'rts',
         )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)/100) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(rows = vars(h), scales = 'free')+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/mkr_7d.pdf')
print(mkr_7d)
dev.off()


invest2gdp <-
  lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h>6,
         startdt== '2000-01-01'
         ,
         predictor %in% c(
           'invest2gdp'
           # 'oil'
           # 'rts'
         )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)/100) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(rows = vars(h), scales = 'free')+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/invest2gdp.pdf')
print(invest2gdp)
dev.off()




rts <-   lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h>0, h<4,
         startdt== '2000-01-01'
         ,
         predictor %in% c(
           
           'rts'
         )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)/100) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  #filter(row_number()<=5) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(rows = vars(h), scales = 'free')+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/rts.pdf')
print(rts)
dev.off()


# список рядов ----

load('data/stationary_data_ext.RData')
tibble(name = df %>% names()) %>%
  mutate(Название = correct.names.pred(name),
                 Трансформация = ifelse(name %in% c('reer','neer','oil','rts'),
                                        '1', 
                                        ifelse(name %in% c('investment', 'CPI_Q_CHI',
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
                                                           
                                        ), '2', '0'
                                        
                                        )),
                 Источник = ifelse(name %in% c('mkr_1d', 'mkr_7d'),
                                   'Банк России',
                                   ifelse(name %in% c('reer', 'neer',
                                                      'oil', 'rts'),
                                          'Bloomberg',
                                          ifelse(name == 'invest2gdp','Расчеты автора',
                                                 'Росстат'
                                          ))
                                   
                 )) %>% select(-name) %>%
  arrange(Название) %>%
  xtable %>%
  print(include.rownames = FALSE)
