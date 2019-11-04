
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
                alpha = ifelse(variable %in% c('GDPEA_Q_DIRI' ,'investment'), 1, 0.3)), show.legend = F)+
  labs(title = "",
       y = "",
       x = "") +
  scale_x_date(limits = c('2001-01-01', '2019-01-01') %>% as.Date)+
  scale_y_continuous(limits = c(-5, 3))+
  theme_transparent()
cairo_pdf("plot/allvars.pdf", width = 10, height = 5)
print(all_plot)
dev.off()



##### rmsfe table -----
load('data/stationary_data.RData')

load('data/raw.RData')



library(xtable)


scoredf$model <- factor(scoredf$model,
                        levels = c("Random Walk","AR","Adaptive LASSO",
                                   "Elastic Net","LASSO","Post-LASSO",
                                   "Random Forest","Ridge","Spike-and-Slab"))
scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '1997-01-01') %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup %>%
  group_by(model, startdt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)

scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '2001-01-01') %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup %>%
  group_by(model, startdt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  dcast(model ~ h) %>%
  xtable %>%
  print(include.rownames = FALSE)



scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '1997-01-01') %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup %>%
  group_by(model, startdt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  dcast(model ~ h, value.var ='lag') %>%
  xtable %>%
  print(include.rownames = FALSE)

scoredf %>%
  filter(type == 'test') %>%
  filter(startdt == '2001-01-01') %>%
  group_by(model, lag, h, startdt) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup %>%
  group_by(model, startdt, h) %>%
  filter(rmse == min(rmse)) %>%
  filter(lag == min(lag)) %>%
  dcast(model ~ h, value.var ='lag') %>%
  xtable %>%
  print(include.rownames = FALSE)

#### dm test ----


dmdf <- get.dm(out_true %>% na.omit)

dmsd <- dmdf %>%
  filter(model != 'Random Walk') %>%
  mutate(change = ifelse(pvalue > 0.01,
                         '0',
                         ifelse(stat < 0,
                                '+',
                                '-')
  )) %>%
  ggplot(aes( model,interaction(h, lag))) +
  geom_raster(aes(fill = change))

cairo_pdf('plot/dmsd.pdf')
print(dmsd)
dev.off()


dmtest97 <- get.dm.lag(out_true %>% na.omit) %>%
  filter(!model %in% c('Random Walk', 'AR'),
         startdt == '1997-01-01') %>%
  # mutate(change = ifelse(pvalue > 0.01,
  #                        '0',
  #                        ifelse(stat < 0,
  #                               '+',
  #                               '-')
  # )) %>%
  mutate(less = ifelse(pvalue >0.05 , FALSE, TRUE)) %>%
  ggplot(aes(lag, interaction(model))) +
  geom_raster(aes(fill = less))+
  facet_wrap(vars(h))

cairo_pdf('plot/dmtest97.pdf')
print(dmtest97)
dev.off()


dmtest01 <- get.dm.lag(out_true %>% na.omit) %>%
  filter(!model %in% c('Random Walk', 'AR'),
         startdt == '2001-01-01') %>%
  # mutate(change = ifelse(pvalue > 0.01,
  #                        '0',
  #                        ifelse(stat < 0,
  #                               '+',
  #                               '-')
  # )) %>%
  mutate(less = ifelse(pvalue >0.05 , FALSE, TRUE)) %>%
  ggplot(aes(lag, interaction(model))) +
  geom_raster(aes(fill = less))+
  facet_wrap(vars(h))

cairo_pdf('plot/dmtest01.pdf')
print(dmtest01)
dev.off()




# вычисление предикторов adalasso ----
source('fun.R')
source('lib.r')

load('out/full/out_lasso.RData')
load('out/full/out_adalasso.RData')


lasso_beta <- ovut_lasso %>%
  plyr::compact()%>%
  map_dfr(
    function(x){
      betaval = x$model_fit$beta 
      x$model_fit$beta
      data.frame(model = x$model,
                 lag = x$lag,
                 h = x$h, 
                 startdt=x$startdt,
                 enddt = x$enddt,
                 predictor = betaval%>% rownames(),
                 beta = betaval%>% as.numeric
      )
      
    }
  )

# количество переменных

adalasso_beta <- out_adalasso %>%
  plyr::compact()%>%
  map_dfr(
  function(x){
    betaval = x$model_fit$beta 
    x$model_fit$beta
    data.frame(model = x$model,
               lag = x$lag,
               h = x$h, 
               startdt=x$startdt,
               enddt = x$enddt,
               predictor = betaval%>% rownames(),
               beta = betaval%>% as.numeric
    )
    
  }
)

# количество переменных
ada_nonzero <-  adalasso_beta %>%
  filter(lag == 4) %>%
  mutate(startdt = factor(startdt, c('2001-01-01','1997-01-01'))) %>%
  group_by(lag, h, startdt, enddt) %>%
  summarise(nz = sum(beta != 0)) %>%
  ggplot(aes(enddt, nz, linetype = startdt))+
  geom_line()+
  labs(title = "",
       y = "Количество переменных",
       x = "Дата",
       color = '')+
  facet_wrap(vars(h))+
  theme_bw()



cairo_pdf('plot/ada_nonzero.pdf')
print(ada_nonzero)
dev.off()


adalasso_beta %>% group_by(predictor, lag, h, startdt) %>%
  filter(lag==0,startdt== '2001-01-01') %>%
  filter(h<2) %>%
  group_by(predictor, h) %>%
  summarise(beta = mean(beta)) %>%
  ungroup %>%
  group_by(h) %>%
  arrange(desc(abs(beta))) %>%
  mutate(rn = row_number(),
         pred_beta = paste0(predictor, round(beta,3))) %>%
  filter(rn<=40) %>%
  ungroup %>%
  dcast(rn~h, value.var = 'pred_beta') %>%
  xtable %>%
  print(include.rownames = FALSE)


adalasso_beta %>% group_by(predictor, lag, h, startdt) %>%
  filter(lag==4,startdt== '2001-01-01') %>%
  filter(h>4) %>%
  group_by(predictor, h) %>%
  summarise(beta = mean(beta)) %>%
  ungroup %>%
  group_by(h) %>%
  arrange(desc(abs(beta))) %>%
  mutate(rn = row_number(),
         pred_beta = paste0(predictor, round(beta,3))) %>%
  filter(rn<=10) %>%
  ungroup %>%
  dcast(rn~h, value.var = 'pred_beta') %>%
  xtable %>%
  print(include.rownames = FALSE)
  
  mutate(predictor = factor(predictor, predictor %>% unique)) %>%
  
  ggplot()+
  #geom_line(aes(h, includes, color=predictor))
  geom_segment(aes(x = 0, xend = includes, y = predictor, yend = predictor, color=as.factor(sign)))+
  facet_wrap(vars(h))





