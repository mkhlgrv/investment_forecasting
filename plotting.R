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
                        levels = c("Случайное блуждание","AR",
                                   "Adaptive LASSO",
                                   "Elastic Net",
                                   "LASSO",
                                   "Post-LASSO",
                                   "Ridge",
                                   "Spike and Slab",
                                   
                                   'Бустинг (eta = 0,1)',
                                   'Бустинг (eta = 0,2)',
                                   'Бустинг (eta = 0,3)',
                                   'Бустинг (eta = 0,4)',
                                   
                                   'Случайный лес (N = 100)',
                                   'Случайный лес (N = 500)',
                                   'Случайный лес (N = 1000)',
                                   'Случайный лес (N = 2000)'
                                   
                                  
                                   ))


scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '1996-01-01') %>%
  filter(lag==0, h > 0) %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)

scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '2000-01-01') %>%
  filter(lag==0, h > 0) %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)

#### dm test ----


dmdf <- get.dm(out_true %>%
                 filter(lag==0, h > 0) %>%
                 na.omit)

dmdiff <- scoredf %>%
  filter(type == 'test') %>%
  group_by(model, lag, h) %>%
  arrange(startdt) %>%
  summarise(diff = rmse[1]-rmse[2]) %>%
  inner_join(dmdf, by =c('model', 'lag', 'h'))

dmdiff %>%
  filter(!model %in% c('Случайное блуждание')) %>%
  mutate(diff = paste0(format(round(diff,3), nsmall = 2),
                       
                       ifelse(pvalue <= 0.1,
                                                   ifelse(pvalue > 0.05, '.',
                                                          ifelse (pvalue > 0.01, "*", 
                                                                  ifelse(pvalue > 0.001, '**', '***'))),'')),
                       pvalue = paste0(' (',format(round(pvalue,3), nsmall = 2),')'),
         lastrow = '') %>%
  melt(id.vars = c('model', 'h'), measure.vars = c('diff', 'pvalue', 'lastrow')) %>%
  mutate(model_id = model,
         model = ifelse(variable == 'diff',model, ifelse(variable == 'pvalue', ' ', ''))) %>%
  mutate(model = factor(model, levels = model %>% unique %>% sort %>% rev)) %>%
  dcast(model_id+model~h) %>%
  select(-model_id) %>%
  # select(-model) %>%
  # add_column('h =' = '', .after = 1) %>%
  xtable %>%
  print(include.rownames = FALSE)




dmsd <- dmdf %>%
  filter(!model %in% c('Случайное блуждание'), h>0) %>%
  mutate(Изменение = ifelse(pvalue > 0.05,
                            '0',
                            ifelse(stat < 0,
                                   '+',
                                   '-')
  )) %>%
  ggplot(aes(factor(h), factor(model, levels = rev(unique(model))))) +
  geom_tile(aes(fill = Изменение),color='grey')+
  theme_bw()+
  labs(y = 'Модель',
       x = 'Горизонт прогнозирования')+
  theme(legend.position="bottom")

cairo_pdf('plot/dmsd.pdf')
print(dmsd)
dev.off()





#### dm test 2 (между разными моделями) ----
IMat <-out_true%>%
  filter(h > 0) %>%
  na.omit %>%
  filter(date > as.Date(as.yearqtr( enddt)+h/4),
    date <= as.Date(as.yearqtr( enddt)+(h+1)/4))

n_models <- IMat$model %>% unique %>% length

outmat <- expand.grid(i = 1:n_models,
            j = 1:n_models,
            startdt = c('1996-01-01','2000-01-01'),
            h = 1L:8L) %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    
    i <- x$i
    j <- x$j
    
    inmat <- IMat %>%
      filter(h == x$h,
             startdt == as.character(x$startdt)) %>%
      dcast(date~ model, value.var = 'pred') %>%
      select(-date) %>%
      as.matrix
    
    realized <- IMat %>%
      filter(h == x$h,
             startdt == as.character(x$startdt),
             model == 'LASSO') %>%
      na.omit %>% 
      pull(true)
    
    
    h1 <- ifelse((inmat[,i]-realized)^2 <
                   (inmat[ ,j]-realized)^2, 'more', 'less')
    if(i != j){
      data.frame(model_column = colnames(inmat)[j],
                 model_row = colnames(inmat)[i],
                 h1 = h1,
                 pvalue =DM.test(inmat[, i],inmat[, j],
                                 realized,loss.type="SE",
                                 c=TRUE,H1=h1)  %>%
                   .$p.value ,
                 h = x$h,
                 startdt = x$startdt)

    } else{
      data.frame(model_column = colnames(inmat)[j],
                 model_row = colnames(inmat)[i],
                 h1 = 'same',
                 pvalue =1,
                 h = x$h,
                 startdt = x$startdt)
    }
    
  }
)

h.labs <- c(#'h = 0',
            "h = 1", 'h = 2', "h = 3", 'h = 4',"h = 5", 'h = 6', "h = 7", 'h = 8')
names(h.labs) <- c(#"0",
                   "1", '2','3', '4', '5', '6', '7', '8')

dm_96_toplot <- outmat %>%
  mutate(pvalue = ifelse(is.nan(pvalue), 1, pvalue)) %>%
  filter(startdt == '1996-01-01'#,
         # model_column != 'Случайное блуждание',
         # model_row != 'Случайное блуждание'
         ) %>%
  mutate(
    # Изменение = ifelse(pvalue > 0.05,
    #                         ifelse(pvalue == 1,
    #                                '0', 
    #                                '0'),
    #                         ifelse(h1 == 'less', '-', '+')),
    Изменение = ifelse(pvalue > 0.1,
                       'не значимо',
                       ifelse(pvalue > 0.05,
                              ifelse(h1 == 'less', '-.','+.'),
                              ifelse(
                              pvalue > 0.01,
                              ifelse(h1 == 'less', '-*','+*'), ifelse(h1 == 'less', '-**','+**')))
                       ),
    
                  
                  model_column=factor(model_column, 
                                      

                                      levels = c("AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                                      levels = c('Случайное блуждание',
                                                 "AR","Adaptive LASSO",
                                                 "Elastic Net","LASSO","Post-LASSO","Ridge",

                                                 "Spike and Slab",
                                                 "Бустинг (eta = 0,1)",
                                                 "Бустинг (eta = 0,2)",
                                                 "Бустинг (eta = 0,3)",
                                                 "Бустинг (eta = 0,4)",
                                                 "Случайный лес (N = 100)","Случайный лес (N = 500)" 
                                                 ,"Случайный лес (N = 1000)",
                                                 "Случайный лес (N = 2000)"))),
                  model_row=factor(model_row,
                                   
                                   

                                   levels =  c("AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                                 levels =  c('Случайное блуждание',
                                               "AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",

                                                       "Spike and Slab",
                                                       "Бустинг (eta = 0,1)",
                                                       "Бустинг (eta = 0,2)",
                                                       "Бустинг (eta = 0,3)",
                                                       "Бустинг (eta = 0,4)",
                                                       "Случайный лес (N = 100)","Случайный лес (N = 500)" 
                                                       ,"Случайный лес (N = 1000)",
                                                       "Случайный лес (N = 2000)") %>% rev))) %>%
  mutate(Изменение = factor(Изменение, levels = c('+.', '-.', '+*','-*', '+**', '-**', 'не значимо')))
dm_96 <- dm_96_toplot%>%

  ggplot(aes(model_column, model_row)) +
  geom_tile(aes(fill = Изменение),color='grey')+
  theme_bw()+
  labs(x = '',
       y = '')+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(angle = 90, size=8))+
  facet_wrap(~h,
             labeller = labeller(h = h.labs))+
  scale_fill_manual(values = c("#add1a9",
                               '#db696f',
                               '#71d466',
                               '#d9454d',
                               "#2bd918",
                               '#d60f1a',
                               'white'))+
  scale_x_discrete(labels = c('Случайное блуждание',"AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                              "Spike and Slab",
                              "Бустинг (0.1)",
                              "Бустинг (0.2)",
                              "Бустинг (0.3)",
                              "Бустинг (0.4)",
                              "Случайный лес (100)" ,"Случайный лес (500)" 
                              ,"Случайный лес (1000)",
                              "Случайный лес (2000)"))+
  scale_y_discrete(labels = c('Случайное блуждание',"AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                              "Spike and Slab",
                              "Бустинг (0.1)",
                              "Бустинг (0.2)",
                              "Бустинг (0.3)",
                              "Бустинг (0.4)",
                              "Случайный лес (100)" ,
                              "Случайный лес (500)" 
                              ,"Случайный лес (1000)",
                              "Случайный лес (2000)") %>% rev)


cairo_pdf('plot/dm96.pdf')
print(dm_96)
dev.off()


dm_00_toplot <- outmat %>%
  mutate(pvalue = ifelse(is.nan(pvalue), 1, pvalue)) %>%
  filter(startdt == '2000-01-01'#,
         # model_column != 'Случайное блуждание',
         # model_row != 'Случайное блуждание'
  ) %>%
  mutate(
    # Изменение = ifelse(pvalue > 0.05,
    #                         ifelse(pvalue == 1,
    #                                '0', 
    #                                '0'),
    #                         ifelse(h1 == 'less', '-', '+')),
    Изменение = ifelse(pvalue > 0.1,
                       'не значимо',
                       ifelse(pvalue > 0.05,
                              ifelse(h1 == 'less', '-.','+.'),
                              ifelse(
                                pvalue > 0.01,
                                ifelse(h1 == 'less', '-*','+*'), ifelse(h1 == 'less', '-**','+**')))
    ),
    
    
    model_column=factor(model_column, 
                        
                        levels = c("AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                        levels = c('Случайное блуждание',
                                   "AR","Adaptive LASSO",
                                   "Elastic Net","LASSO","Post-LASSO","Ridge",
                                   "Spike and Slab",
                                   "Бустинг (eta = 0,1)",
                                   "Бустинг (eta = 0,2)",
                                   "Бустинг (eta = 0,3)",
                                   "Бустинг (eta = 0,4)",
                                   "Случайный лес (N = 100)","Случайный лес (N = 500)" 
                                   ,"Случайный лес (N = 1000)",
                                   "Случайный лес (N = 2000)"))),
    model_row=factor(model_row,
                     
                     
                     levels =  c("AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                     levels =  c('Случайное блуждание',
                                 "AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
"Spike and Slab",
                                 "Бустинг (eta = 0,1)",
                                 "Бустинг (eta = 0,2)",
                                 "Бустинг (eta = 0,3)",
                                 "Бустинг (eta = 0,4)",
                                 "Случайный лес (N = 100)","Случайный лес (N = 500)" 
                                 ,"Случайный лес (N = 1000)",
                                 "Случайный лес (N = 2000)") %>% rev))) %>%
  mutate(Изменение = factor(Изменение, levels = c('+.', '-.', '+*','-*', '+**', '-**', 'не значимо')))
dm_00 <- dm_00_toplot%>%
  
  ggplot(aes(model_column, model_row)) +
  geom_tile(aes(fill = Изменение),color='grey')+
  theme_bw()+
  labs(x = '',
       y = '')+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(angle = 90, size=8))+
  facet_wrap(~h,
             labeller = labeller(h = h.labs))+
  scale_fill_manual(values = c("#add1a9",
                               '#db696f',
                               '#71d466',
                               '#d9454d',
                               "#2bd918",
                               '#d60f1a',
                               'white'))+
  scale_x_discrete(labels = c('Случайное блуждание',"AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                              "Spike and Slab",
                              "Бустинг (0.1)",
                              "Бустинг (0.2)",
                              "Бустинг (0.3)",
                              "Бустинг (0.4)",
                              "Случайный лес (100)" ,"Случайный лес (500)" 
                              ,"Случайный лес (1000)",
                              "Случайный лес (2000)"))+
  scale_y_discrete(labels = c('Случайное блуждание',"AR","Adaptive LASSO","Elastic Net","LASSO","Post-LASSO","Ridge",
                              "Spike and Slab",
                              "Бустинг (0.1)",
                              "Бустинг (0.2)",
                              "Бустинг (0.3)",
                              "Бустинг (0.4)",
                              "Случайный лес (100)" ,
                              "Случайный лес (500)" 
                              ,"Случайный лес (1000)",
                              "Случайный лес (2000)") %>% rev)


cairo_pdf('plot/dm00.pdf')
print(dm_00)
dev.off()


# lasso coefs ----
# сначала надо найти sd каждой переменной в каждой тренировочной выборке и поделить на него коэффициент
load('data/stationary_data_ext.RData')
# load('data/stationary_data_ext.RData')
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
      as_tibble %>%
      mutate(enddt = x$enddt,
             startdt = x$startdt, .)
    
  })


# вычисление предикторов lasso ----
source('fun.R', encoding = 'utf-8')
source('lib.r')

load('out/full/out_lasso.RData')

load('out/full/out_zero.RData')



lasso_beta <- 
  c(#out_zero[151:200],
    out_lasso[-c(1:50)]
    ) %>%
  plyr::compact()%>%
  map_dfr(
    
    function(x, i){
      x$startdt = as.character(x$startdt)
      x$startdt = ifelse(x$startdt == '1996-04-01', '1996-01-01', x$startdt)
      x$startdt = as.Date(x$startdt)
      
      if(x$h == 0){
        actsd <- sddata %>% filter(startdt == x$startdt,
                                   enddt == x$enddt) %>%
          select(-c(investment, startdt, enddt, invest2gdp, GDPEA_Q_DIRI))
        # s.d. of y
        ysd <- sddata %>% filter(startdt == x$startdt,
                                 enddt == x$enddt) %>%
          pull(investment) %>%
          as.numeric
          
      } else{
        actsd <- sddata %>% filter(startdt == x$startdt,
                                   enddt == x$enddt) %>%
          select(-c(startdt, enddt, gdplag, investmentlag, invest2gdplag))
        ysd <- actsd[1,1] %>%
          as.numeric
      }
      
      
      betaval = x$model_fit$beta
      
      if(!all((betaval%>% rownames) ==(actsd %>% colnames))){
        print(actsd %>% colnames)
        print(betaval%>% rownames)
        stop()
      }
      if(length(x$model) == 0|
         length(x$h) == 0|
         length(x$startdt) == 0|
         length(x$enddt) == 0|
         length(betaval%>% rownames)==0|
         length((betaval%>% as.numeric)/(actsd[1,] %>% as.numeric)*(ysd)) == 0){
        print(actsd[1,])
        stop()
      }

      data.frame(model = x$model,
                 h = x$h,
                 startdt=x$startdt,
                 enddt = x$enddt,
                 predictor = betaval%>% rownames,
                 beta = (betaval%>% as.numeric)/(actsd[1,] %>% as.numeric)*(ysd)
      )
      
    }
  )


h.labs <- c(#'h = 0',
            "h = 1", 'h = 2', "h = 3", 'h = 4',"h = 5", 'h = 6', "h = 7", 'h = 8')
names(h.labs) <- c(#"0",
                   "1", '2','3', '4', '5', '6', '7', '8')


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
  facet_wrap(~h, scales = 'free',
             labeller = labeller(h = h.labs))+
  theme_bw()+
  theme(legend.position="bottom")

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
      'GDPEA_Q_DIRI',
      'gdplag', 'investmentlag', 'invest2gdplag'
      #,
      #'RTRD_Q_DIRI',
      #'EMPLDEC_Q',
      #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
      #'CNSTR_Q_DIRI'# индекс работ в строительств
    )
  ) %>%
  ungroup%>%
  #mutate(predictor = correct.names.pred(predictor)) %>%
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
  summarise(beta = mean(beta)) %>%
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




lasso_beta %>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(predictor, h, startdt) %>%
  
  filter(startdt== '2000-01-01') %>%
  filter(h %in% c(0,1,2,7,8 )) %>%
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

# ВВП -----




gdp <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h<2,
         startdt== '2000-01-01',
         predictor %in% c(
           'GDPEA_Q_DIRI',
           'gdplag'
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
  ungroup() %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta))+
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/gdp.pdf')
print(gdp)
dev.off()

lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h<3,
         startdt== '2000-01-01',
         predictor %in% c(
           'oil'
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
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()


invest <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(h < 2,
         startdt== '2000-01-01'
         ,
         predictor %in% c(
           'investment',
           'investmentlag'
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
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
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
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
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
           'invest2gdp',
           'invest2gdplag'
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
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
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
  facet_grid(h~., scales = 'free',
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()

cairo_pdf('plot/rts.pdf')
print(rts)
dev.off()



# in 9

gdp <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
         startdt== '2000-01-01',
         #' predictor %in% c(
         #'   'GDPEA_Q_DIRI'
         #'   #,
         #'   #'RTRD_Q_DIRI',
         #'   #'EMPLDEC_Q',
         #'   #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
         #'   #'CNSTR_Q_DIRI'# индекс работ в строительств
         #' )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  ungroup() %>%
  group_by(h, predictor, startdt) %>%
  filter(mean(abs(beta))>0) %>%
  ungroup %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta, group = predictor),
            alpha = 0.1)+
  facet_wrap(h~.,
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank())+
  scale_y_continuous(trans= 'asinh')+
  
  
    geom_line(data = lasso_beta %>%
                group_by(predictor, h, startdt) %>%
                filter(
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
                mutate(beta_mean = mean(beta)) %>%
                ungroup %>%
                group_by(h, startdt, enddt) %>%
                arrange(desc(abs(beta_mean))) %>% 
                ungroup() %>%
                group_by(h, predictor, startdt) %>%
                filter(mean(abs(beta))>0) %>%
                ungroup %>%
                mutate(h = as.factor(h)),
                mapping = aes(enddt, beta),
              size =1)
  
cairo_pdf('plot/gdp9.pdf')
print(gdp)
dev.off()


invest <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
    startdt== '2000-01-01',
    #' )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  ungroup() %>%
  group_by(h, predictor, startdt) %>%
  filter(mean(abs(beta))>0) %>%
  ungroup %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta, group = predictor),
            alpha = 0.1)+
  facet_wrap(h~., 
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank())+
  scale_y_continuous(trans= 'asinh')+
  
  
  geom_line(data = lasso_beta %>%
              group_by(predictor, h, startdt) %>%
              filter(
                startdt== '2000-01-01',
                predictor %in% c(
                  'investment'
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
              ungroup() %>%
              group_by(h, predictor, startdt) %>%
              filter(mean(abs(beta))>0) %>%
              ungroup %>%
              mutate(h = as.factor(h)),
            mapping = aes(enddt, beta),
            size =1)

cairo_pdf('plot/invest9.pdf')
print(invest)
dev.off()



invest2gdp <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
    startdt== '2000-01-01',
    #' predictor %in% c(
    #'   'GDPEA_Q_DIRI'
    #'   #,
    #'   #'RTRD_Q_DIRI',
    #'   #'EMPLDEC_Q',
    #'   #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
    #'   #'CNSTR_Q_DIRI'# индекс работ в строительств
    #' )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>% 
  ungroup() %>%
  group_by(h, predictor, startdt) %>%
  filter(mean(abs(beta))>0) %>%
  ungroup %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta, group = predictor),
            alpha = 0.1)+
  facet_wrap(h~., 
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )+
  scale_y_continuous(trans= 'asinh')+
  
  
  geom_line(data = lasso_beta %>%
              group_by(predictor, h, startdt) %>%
              filter(
                startdt== '2000-01-01',
                predictor %in% c(
                  'invest2gdp'
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
              ungroup() %>%
              group_by(h, predictor, startdt) %>%
              filter(mean(abs(beta))>0) %>%
              ungroup %>%
              mutate(h = as.factor(h)),
            mapping = aes(enddt, beta),
            size =1)

cairo_pdf('plot/invest2gdp9.pdf')
print(invest2gdp)
dev.off()



rts <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
    startdt== '2000-01-01',
    #' predictor %in% c(
    #'   'GDPEA_Q_DIRI'
    #'   #,
    #'   #'RTRD_Q_DIRI',
    #'   #'EMPLDEC_Q',
    #'   #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
    #'   #'CNSTR_Q_DIRI'# индекс работ в строительств
    #' )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>%
  ungroup() %>%
  group_by(h, predictor, startdt) %>%
  filter(mean(abs(beta))>0) %>%
  ungroup %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta, group = predictor),
            alpha = 0.1)+
  facet_wrap(h~.,
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank())+
  scale_y_continuous(trans= 'asinh')+


  geom_line(data = lasso_beta %>%
              group_by(predictor, h, startdt) %>%
              filter(
                startdt== '2000-01-01',
                predictor %in% c(
                  'rts'
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
              ungroup() %>%
              group_by(h, predictor, startdt) %>%
              filter(mean(abs(beta))>0) %>%
              ungroup %>%
              mutate(h = as.factor(h)),
            mapping = aes(enddt, beta),
            size =1)

cairo_pdf('plot/rts9.pdf')
print(rts)
dev.off()



mkr <- lasso_beta %>%
  group_by(predictor, h, startdt) %>%
  filter(
    startdt== '2000-01-01',
    #' predictor %in% c(
    #'   'GDPEA_Q_DIRI'
    #'   #,
    #'   #'RTRD_Q_DIRI',
    #'   #'EMPLDEC_Q',
    #'   #'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
    #'   #'CNSTR_Q_DIRI'# индекс работ в строительств
    #' )
  ) %>%
  ungroup%>%
  mutate(predictor = correct.names.pred(predictor)) %>%
  group_by(h, predictor) %>%
  mutate(beta_mean = mean(beta)) %>%
  ungroup %>%
  group_by(h, startdt, enddt) %>%
  arrange(desc(abs(beta_mean))) %>%
  ungroup() %>%
  group_by(h, predictor, startdt) %>%
  filter(mean(abs(beta))>0) %>%
  ungroup %>%
  mutate(h = as.factor(h)) %>%
  ggplot()+
  geom_line(aes(enddt, beta, group = predictor),
            alpha = 0.1)+
  facet_wrap(h~.,
             labeller = labeller(h = h.labs))+
  labs(title = "",
       y = "Коэффициент",
       x = "Дата") +
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),         panel.grid.major = element_blank(),         panel.grid.minor = element_blank())+
  scale_y_continuous(trans= 'asinh')+


  geom_line(data = lasso_beta %>%
              group_by(predictor, h, startdt) %>%
              filter(
                startdt== '2000-01-01',
                predictor %in% c(
                  'mkr_7d'
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
              ungroup() %>%
              group_by(h, predictor, startdt) %>%
              filter(mean(abs(beta))>0) %>%
              ungroup %>%
              mutate(h = as.factor(h)),
            mapping = aes(enddt, beta),
            size =1)

cairo_pdf('plot/mkr9.pdf')
print(mkr)
dev.off()




library(scales)
asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

# список рядов ----

source('fun.R', encoding = 'utf-8')
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



# investment RTRD_Q_DIRI GDPEA_Q_DIRI UNEMPL_Q_SH CPI_Q_CHI



load('data/raw.RData')
# med forecast -----

out_cumulative_med <- out_cumulative

out_cumulative_med$model <- factor(out_cumulative_med$model,
                        levels = c("Случайное блуждание","AR",
                                   "Adaptive LASSO",
                                   "Elastic Net",
                                   "LASSO",
                                   "Post-LASSO",
                                   "Ridge",
                                   "Spike and Slab",
                                   
                                   'Бустинг (eta = 0,1)',
                                   'Бустинг (eta = 0,2)',
                                   'Бустинг (eta = 0,3)',
                                   'Бустинг (eta = 0,4)',
                                   
                                   'Случайный лес (N = 100)',
                                   'Случайный лес (N = 500)',
                                   'Случайный лес (N = 1000)',
                                   'Случайный лес (N = 2000)'))

out_cumulative_med$model <- plyr::mapvalues(out_cumulative_med$model, from = c("Случайное блуждание","AR",
                            "Adaptive LASSO",
                            "Elastic Net",
                            "LASSO",
                            "Post-LASSO",
                            "Ridge",
                            "Spike and Slab",
                            
                            'Бустинг (eta = 0,1)',
                            'Бустинг (eta = 0,2)',
                            'Бустинг (eta = 0,3)',
                            'Бустинг (eta = 0,4)',
                            
                            'Случайный лес (N = 100)',
                            'Случайный лес (N = 500)',
                            'Случайный лес (N = 1000)',
                            'Случайный лес (N = 2000)'), to = c("Случайное блуждание","AR",
                                                                "Adaptive LASSO",
                                                                "Elastic Net",
                                                                "LASSO",
                                                                "Post-LASSO",
                                                                "Ridge",
                                                                "Spike and Slab",
                                                                
                                                                'Бустинг (0,1)',
                                                                'Бустинг (0,2)',
                                                                'Бустинг (0,3)',
                                                                'Бустинг (0,4)',
                                                                
                                                                'Случайный лес (100)',
                                                                'Случайный лес (500)',
                                                                'Случайный лес (1000)',
                                                                'Случайный лес (2000)'))


med_forecast <- import('data/med_forecast.csv', encoding = 'UTF-8', header = TRUE) %>%
  melt %>%
  set_names(c('fctname', 'year', 'value')) %>%
  mutate(year = as.character(year) %>% as.numeric) %>%
  mutate(fctyear = substr(fctname, 1, 4) %>% as.numeric) %>%
  filter(fctyear < year)



my_forecast <-
  out_cumulative_med %>%
  dplyr::group_by(forecastdate, model, h) %>%
  filter(enddt == forecastdate) %>%
  ungroup() %>%
  filter(h!=0) %>%
  filter(h > 1, h < 6) %>%
  mutate(year = year(date),
         h_year = if_else(h<=4, 1, 2)) %>%
  dplyr::group_by(model,h_year, year, startdt, forecastdate) %>%
  summarise(pred = sum(pred_cumulative),
            true_lag = sum(true_lag),
            true = sum(true_cumulative)) %>%
  mutate(pred = 100*(pred/ true_lag - 1),
         true = 100*(true/ true_lag - 1)) %>%
  ungroup %>% select(-forecastdate)



raw_y <- rawdata$investment %>%
  as.data.frame() %>%
  rownames_to_column('year') %>%
  mutate(year = year(as.yearqtr(year))) %>%
  group_by(year) %>%
  summarise(investment = sum(investment)) %>%
  mutate(investment = 100*(investment/lag(investment)-1))


forec_vs <- my_forecast %>%
  select(-c(true_lag, true)) %>%
  filter(h_year ==1, startdt == max(startdt)) %>%
  filter(!is.na(pred)) %>%
  filter(!model %in% c('Случайное блуждание', 'AR',
                       'Бустинг (0,1)',
                       'Бустинг (0,3)',
                       'Бустинг (0,4)',
                       
                       'Случайный лес (100)',
                       'Случайный лес (500)',
                       'Случайный лес (1000)'

                       
                       
                       ))

plot1 <- forec_vs %>% ggplot()+
  geom_bar(aes(year, pred, fill = model),
           stat="identity",
           # fill = 'white',
           position = 'dodge',
           
           #position = position_dodge2(width = 0.9, preserve = "single"),
           color='black')+
  scale_fill_discrete(name = "Модель")+
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(10,10,10,10))

plot2 <- ggplot()+
  
  geom_bar(aes(year, value, group=fctname,
               fill = 'Прогноз МЭР',
               alpha ='Прогноз МЭР'), med_forecast %>%
             group_by(year) %>%
             filter(fctyear== max(fctyear)) %>%
             filter(year <2019, year > 2013 ) %>%
             mutate(fctname = factor(fctname,
                                     levels = c('2013 (консервативный)',
                                                '2013(базовый) ',
                                                '2014',
                                                '2015',
                                                '2016 (базовый +) ',
                                                '2016 (базовый)',
                                                
                                                '2017')))
           ,
           stat="identity",
           position = 'dodge'
  )+
  scale_alpha_manual(values = 0.4)+
  scale_fill_manual(values = 'blue')+
  guides(fill = guide_legend(" "),
         alpha = guide_legend(" "))+
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(10,10,10,10))


plot3 <- ggplot()+
  geom_point(aes(year, investment, color = 'Наблюдаемые значения'),
             data = raw_y %>% filter(year <2019,year >2013),
              size = 2)+
  geom_line(aes(year, investment, color = 'Наблюдаемые значения'),
            data = raw_y %>% filter(year <2019,year >2013)
            )+
  scale_size_manual(values = 2)+
  scale_color_manual(values = 'black')+
  guides(size = guide_legend("  "),
         color = guide_legend("  "))+
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(10,10,10,10))

p <- forec_vs %>% ggplot()+
  geom_bar(aes(year, pred, fill = model),
           stat="identity",
           # fill = 'white',
           position = 'dodge',
           
           #position = position_dodge2(width = 0.9, preserve = "single"),
           color='black')+
  
  geom_bar(aes(year, value, group=fctname,
  ),
  fill = 'blue',
  alpha =0.4,
  med_forecast %>%
    group_by(year) %>%
    filter(fctyear== max(fctyear)) %>%
    filter(year <2019, year > 2013 )%>%
    mutate(fctname = factor(fctname,
                            levels = c('2013 (консервативный)',
                                       '2013(базовый) ',
                                       '2014',
                                       '2015',
                                       '2016 (базовый +) ',
                                       '2016 (базовый)',
                                       
                                       '2017')))
  ,
  stat="identity",
  position = 'dodge'
  )+geom_point(aes(year, investment),
               data = raw_y %>% filter(year <2019,year >2013),
               color = 'black', size = 2)+
  geom_line(aes(year, investment),
            data = raw_y %>% filter(year <2019,year >2013),
            color = 'black')+
  
  scale_fill_discrete(guide="none")+
  
  labs(#title = 'Инвестиции в России: прогнозы МЭР и прогнозы автора',
    #subtitle = 'Горизонт прогнозирования - один год',
    x = 'Дата',
    y = 'Изменение валового накопления основного капитала,\n в % к прошлому году')+
  theme_bw()



blank <- grid.rect(gp=gpar(col="white"))
grid.arrange(p,
                      arrangeGrob(g_legend(plot1),
                                  g_legend(plot2),
                                  g_legend(plot3),
                                  blank,
                                  nrow=4),
                      ncol=2,widths=c(7,3))



all_for <- bind_rows(med_forecast %>%
                       group_by(year) %>%
                       filter(fctyear== max(fctyear)) %>%
                       filter(year <2019, year > 2013) %>%
                       mutate(model = 'МЭР',
                              pred = value) %>%
                       filter(!fctname %in% c('2013(базовый)', '2016 (базовый)')) %>%
                       select(model, year, pred),
                     forec_vs %>% select(model, year, pred)) %>%
  ungroup %>%
  inner_join(raw_y, by = 'year') %>%
  group_by(model) %>%
  summarise(rmse = sqrt(sum((pred-investment)^2)))
all_for
#ggsave(file="plot/med_forecast.pdf", medfor)

# волосы для всех прогнозов



# вариант 2 сумма квадратов ошибок на каждую дату прогноза
# с ростом h растет и абсолютная ошибка,
# поэтому делим ошибку одной модели на среднюю ошибку для каждого h

# na.omit %>%
# filter(h<=2) %>%
# mutate(error = (pred - true)^2) %>%
# group_by(h) %>%
# mutate(error = (error-mean(error))/sd(error)) %>%
# ungroup %>%
# group_by(forecastdate, model, startdt) %>%
# summarise(sse = mean(error)) %>%
# ggplot()+
# geom_line(aes(forecastdate, sse,
#               color = factor(startdt)))+
# facet_wrap(vars(model))



##### рисуем не прогноз, а текущее объяснение
out_true %>%
  filter(enddt < as.Date(as.yearqtr(date)-h/4)) %>%
  group_by(date, h, model, startdt) %>%
  filter(enddt == max(enddt)) %>%
  ungroup %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  filter(#startdt == '2000-01-01',
         forecastdate <='2019-01-01',
         h==0,
         model != 'Random Walk') %>% 
  
  # вариант 1 просто рисуем прогнозы
  
  ggplot()+
  stat_summary(aes(x = date, y = true),
               fun.y=mean, geom='line', alpha = 0.5, size = 4, color = 'grey')+
  geom_line(aes(date, pred),
            linetype = 'dashed')+
  facet_wrap(vars(model))+
  scale_y_continuous(limits = c(-0.2, 0.15))+
  labs(x = 'Дата',
    y = 'Квартальное изменение валового накопления\nосновного капитала')


#### ошибки во времени
out_true %>%
  filter(enddt < as.Date(as.yearqtr(date)-h/4)) %>%
  group_by(date, h, model, startdt) %>%
  filter(enddt == max(enddt)) %>%
  ungroup %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  mutate(pred = ifelse(h == 0, true, pred)) %>%
  filter(#startdt == '1996-01-01',
    model != 'AR',
         forecastdate <='2019-01-01',
         h>0,
         model != 'Random Walk') %>% 
  
  # вариант 2 сумма квадратов ошибок на каждую дату прогноза
  # с ростом h растет и абсолютная ошибка,
  # поэтому делим ошибку одной модели на среднюю ошибку для каждого h
  
na.omit %>%
filter(h<=2) %>%
mutate(error = (pred - true)^2) %>%
group_by(h) %>%
mutate(error = (error-mean(error))/sd(error)) %>%
ungroup %>%
group_by(forecastdate, model, startdt) %>%
summarise(sse = mean(error)) %>%
ggplot()+
geom_line(aes(forecastdate, sse,
              color = factor(startdt)))+
facet_wrap(vars(model))


### gif прогнозы
library(gapminder)

# Charge libraries:
library(gganimate)

library(gapminder)
library(gganimate)
library(gifski)
library(png)



out_hair <- out_true %>%
  filter(enddt < as.Date(as.yearqtr(date)-h/4)) %>%
  group_by(date, h, model, startdt) %>%
  filter(enddt == max(enddt)) %>%
  ungroup %>%
  mutate(forecastdate = as.Date(as.yearqtr(date) -h/4)) %>%
  mutate(pred = ifelse(h == 0, true, pred)) %>%
  mutate(giftime =as.numeric(forecastdate)+0.2*((date -forecastdate) %>% as.numeric())) %>%
  filter(forecastdate <='2019-01-01') %>%
  mutate(true = ifelse(date <= forecastdate, true, NA))


fordata <- out_hair %>%
  filter(startdt ==
           '2000-01-01',
         h<5,
         ! model %in%
           c('Случайное блуждание',
             'Бустинг (eta = 0,1)','Бустинг (eta = 0,3)',
                                       'Бустинг (eta = 0,4)',
             'Случайный лес (N = 100)',
                                               'Случайный лес (N = 500)',
                                               'Случайный лес (N = 1000)'))
fordata$model <- factor(fordata$model,
                                   levels = c("Случайное блуждание","AR",
                                              "Adaptive LASSO",
                                              "Elastic Net",
                                              "LASSO",
                                              "Post-LASSO",
                                              "Ridge",
                                              "Spike and Slab",
                                              
                                              # 'Бустинг (eta = 0,1)',
                                              'Бустинг (eta = 0,2)',
                                              # 'Бустинг (eta = 0,3)',
                                              # 'Бустинг (eta = 0,4)',
                                              
                                              # 'Случайный лес (N = 100)',
                                              # 'Случайный лес (N = 500)',
                                              # 'Случайный лес (N = 1000)',
                                              'Случайный лес (N = 2000)'))

fordata$model <- plyr::mapvalues(fordata$model, from = c("Случайное блуждание","AR",
                                                                               "Adaptive LASSO",
                                                                               "Elastic Net",
                                                                               "LASSO",
                                                                               "Post-LASSO",
                                                                               "Ridge",
                                                                               "Spike and Slab",
                                                                               
                                                                               # 'Бустинг (eta = 0,1)',
                                                                               'Бустинг (eta = 0,2)',
                                                                               # 'Бустинг (eta = 0,3)',
                                                                               # 'Бустинг (eta = 0,4)',
                                                                               # 
                                                                               # 'Случайный лес (N = 100)',
                                                                               # 'Случайный лес (N = 500)',
                                                                               # 'Случайный лес (N = 1000)',
                                                                               'Случайный лес (N = 2000)'), 
                                 to = c("Случайное блуждание","AR",
                                                                                                                   "Adaptive LASSO",
                                                                                                                   "Elastic Net",
                                                                                                                   "LASSO",
                                                                                                                   "Post-LASSO",
                                                                                                                   "Ridge",
                                                                                                                   "Spike and Slab",
                                                                                                                   
                                                                                                                   # 'Бустинг (0,1)',
                                                                                                                   'Бустинг (0,2)',
                                                                                                                   # 'Бустинг (0,3)',
                                                                                                                   # 'Бустинг (0,4)',
                                                                                                                   # 
                                                                                                                   # 'Случайный лес (100)',
                                                                                                                   # 'Случайный лес (500)',
                                                                                                                   # 'Случайный лес (1000)',
                                                                                                                   'Случайный лес (2000)'))
  


for(modeli in (fordata$model %>% unique)){
  myplot <- ggplot(fordata  %>%
                     filter(model == modeli) %>%
                     mutate(true_na = ifelse(date <= forecastdate, true, NA)))+
    geom_path(data = fordata  %>%
                filter(model == modeli) %>%
                mutate(true_na = ifelse(date <= forecastdate, true, NA)) %>% na.omit,
              aes(date, true_na), alpha = 0.5, size = 2, color = 'grey')+
    geom_line(aes(date, pred,   color = forecastdate,
                  group = interaction(startdt,
                                      forecastdate)),
              #size = 1,
              show.legend = FALSE,
              linetype = 'dashed'
    )+
    #facet_wrap(vars(model))+
    scale_y_continuous(limits = c(-0.2, 0.3))+
    labs(x = 'Дата',
         y = 'Квартальное изменение валового накопления\nосновного капитала (разность логарифмов)')+
    transition_reveal(giftime) +
    ease_aes('linear')+
    theme_minimal()
  
  animate(myplot, duration = 10, fps = 20, width = 1000, height = 1000, renderer = gifski_renderer())
  anim_save(paste0("plot/gif/",modeli,".gif"))
}

myplot <- ggplot(fordata  %>%
                   mutate(true_na = ifelse(date <= forecastdate, true, NA)))+
  geom_path(data = fordata  %>%
              mutate(true_na = ifelse(date <= forecastdate, true, NA)) %>% na.omit,
            aes(date, true_na), alpha = 0.5, size = 2, color = 'grey')+
  geom_line(aes(date, pred,   color = forecastdate,
                group = interaction(startdt,
                                    forecastdate)),
            #size = 1,
            show.legend = FALSE,
            linetype = 'dashed'
  )+
  facet_wrap(vars(model))+
  scale_y_continuous(limits = c(-0.2, 0.3))+
  labs(x = 'Дата',
       y = 'Квартальное изменение валового накопления\nосновного капитала (разность логарифмов)')+
  transition_reveal(giftime) +
  ease_aes('linear')+
  theme_minimal()

animate(myplot, duration = 10, fps = 5, width = 200, height = 200, renderer = gifski_renderer())
anim_save(paste0("plot/gif/",modeli,".gif"))

 # static hair plot ----
hair <- ggplot(fordata%>%
         mutate(true_na = ifelse(date <= forecastdate, true, NA)) %>%
           filter())+
  geom_path(data = fordata  %>%
              mutate(true_na = ifelse(date <= forecastdate, true, NA)) %>% na.omit,
            
            
            aes(date, true_na,
                alpha = 'Наблюдаемые\nзначения',
                color = 'Наблюдаемые\nзначения',
                size = 'Наблюдаемые\nзначения',
                linetype = 'Наблюдаемые\nзначения'))+
  geom_line(aes(date, pred,
                group = interaction(startdt,
                                    forecastdate),
                                    alpha = 'Прогноз',
                                    color = 'Прогноз',
                                    size = 'Прогноз',
                linetype = 'Прогноз')
  )+
  facet_wrap(vars(model))+
  scale_y_continuous(limits = c(-0.2, 0.15))+
  labs(x = 'Дата',
       y = 'Квартальное изменение валового
       накопления\nосновного капитала (разность логарифмов)')+
  theme_bw()+
  # scale_alpha_manual(values = c(0.5, 1))+
  # scale_size_manual(values = c(2,0.7))+
  # scale_color_manual(values = c('grey', 'black'))+
  scale_colour_manual(name="",
                      values=c('grey', 'black'),
                      guide = guide_legend(override.aes=list(linetype=c(1,2),
                                                             alpha = c(0.5, 1),
                                                             size = c(2, 0.6)))) + 
  scale_size_manual(name="Size",values=c(2,0.6), guide="none") +
  scale_alpha_manual(name="Alpha",values=c(0.5,1), guide="none") +
  scale_linetype_manual(name="Type",values=c(1,2), guide="none") +
  theme(legend.position="bottom")

cairo_pdf("plot/hair.pdf")
print(hair)
dev.off()

library(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")
cairo_pdf(p)
print(p)
dev.off()


#####


p <- fordata %>%
  accumulate_by(~giftime) %>%
  ggplot()+
  
  geom_line(aes(x = date,
                y = true,
                frame = frame),
            color='grey',
            size = 2,
            linetype = 'solid',
            alpha = 0.5)


ggplotly(p) %>%
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  )

### штрафная функция для методов регуляризации ----
load('out/full/out_adalasso.RData')
load('out/full/out_elnet.RData')
load('out/full/out_lasso.RData')
load('out/full/out_postlasso.RData')
load('out/full/out_ridge.RData')

load('out/full/out_zero.RData')



regular_norm <- 
  c(out_zero[c(1:50, 101:250, 301:350)],
    out_adalasso[-c(1:50)],
    out_elnet[-c(1:50)],
    out_lasso[-c(1:50)],
    out_postlasso[-c(1:50)],
    out_ridge[-c(1:50)]
    
  ) %>%
  plyr::compact()%>%
  map_dfr(function(x){
    
      if(x$model=='postlasso'){
    norm <-  x$model_fit$coefficients %>% abs %>% sum
      } else if(x$model %in% c('lasso', 'adalasso')){
        norm <-  x$model_fit$beta %>% abs %>% sum
      } else if(x$model == 'ridge'){
        norm <-  x$model_fit$beta^2 %>% sum
      } else if(x$model == 'elnet'){
        norm <- 0.5*(x$model_fit$beta^2 %>% sum +
          x$model_fit$beta %>% abs %>% sum)
        }
      data.frame(
        model = x$model,
          startdt =x$startdt,
        enddt =x$enddt,
        h = x$h,
        norm =  norm
      )
     
    })

regular_norm %>%
  ggplot()+
  geom_line(aes(enddt, norm, color = factor(startdt)))+
  facet_grid(model~h, scales = 'free_y')
# вывод не оч
