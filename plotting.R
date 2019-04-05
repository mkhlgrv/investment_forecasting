
rm(list = ls())
load("reglist.RData")
load("sslist.RData")
load("rflist.RData")
load("bstlist.RData")
load("arlist.RData")
load("rawdata.RData")
load("tfdata.RData")
source("lib.R")
source("fun.R")
unemp.true <- df$UNEMPL_M_SH %>%
  as.data.frame() %>%
  rownames_to_column %>%
  setNames(c("date","y.true")) %>%
  mutate(date = as.yearmon(date))
# unemp
unemp_level <-
  ggplot(unemp.true) +
  geom_line(aes(y = y.true, x = date))+
  labs(y = "Уровень безработицы, %",
       x = "Дата") + theme_bw()
cairo_pdf("plot/unemp_level.pdf", width = 10, height = 5)
print(unemp_level)
dev.off()

pca <- 
  ggplot(data = df_pc, aes(y = PC2, x = PC1, label = year(time(df_pc))))+
  geom_text(aes(colour = as.factor(year(time(df_pc)))), show.legend = FALSE) +
  geom_path(alpha = 0.3) +
    theme_bw()
  
cairo_pdf("plot/pca.pdf", width = 10, height = 5)
  print(pca)
dev.off()

# график в уровнях arma ----
arma <- 
  arlist[[2]] %>%
  filter(nlead <= 18) %>%
  group_by(model, nlead, date) %>%
  mutate(y.pred = mean(y.pred)) %>%
  ungroup() %>%
  select(-c(y.true)) %>%
  unique %>%
  inner_join(unemp.true, by = "date") %>%
  group_by(model, nlead) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% 
  na.omit %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "Безработица"), size =1) +
  geom_line(aes(x = date, y = y.pred, colour = "ARMA(p,q)"), size = 1)+
  labs(y = "Уровень безработицы, %",
       x = "Дата") + theme_bw()+
  guides(colour = guide_legend(title = ""))+
  facet_wrap(vars(nlead), scales = "free")

cairo_pdf("plot/arma.pdf", width = 10, height = 5)
print(arma)
dev.off()


# LASSO

rbind(do.call(rbind, reglist) %>%
        get.score(type = "regular"))
ar.score <- get.score(arlist[[2]], "ar")
reg.score1 <- 
  rbind(do.call(rbind, reglist) %>%
                    get.score(type = "regular")) %>%
  #filter(model!= "lasso_pc_lag") %>%
  filter(model %in% c("lasso", "elnet", "ridge")) %>%
  ungroup %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  mutate(lasty = rmse[length(rmse)-2]) %>%
  ungroup() %>%
  #filter(model != "elnet") %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  labs(y = "RMSE",
       x = "Горизонт прогноза",
       title = "Ошибка прогноза для методов с регуляризацией") +
  theme_bw()+
  #guides(colour = guide_legend(title = ""))+
  theme(legend.title = element_blank())


reg.score2 <- 
  rbind(do.call(rbind, reglist) %>%
          get.score(type = "regular")) %>%
  #filter(model!= "lasso_pc_lag") %>%
  filter(model %in% c("lasso", "lasso_pc", "lasso_lag", "lasso_adaptive", "post_lasso")) %>%
  ungroup %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  ungroup() %>%
  #filter(model != "elnet") %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  labs(y = "RMSE",
        x = "Горизонт прогноза",
        title = "Ошибка прогноза для методов с регуляризацией") +
  theme_bw()+
  #guides(colour = guide_legend(title = ""))+
  theme(legend.title = element_blank())


cairo_pdf("plot/reg_score1.pdf", width = 10, height = 5)
print(reg.score1)
dev.off()

cairo_pdf("plot/reg_score2.pdf", width = 10, height = 5)
print(reg.score2)
dev.off()


# RF and SS ----
load("rflist.RData")
load("sslist.Rdata")
ar.score <- get.score(arlist[[2]], "ar")
rfss.score <- 
  do.call(rbind, rflist) %>%
  get.score(type = "rf") %>%
  rbind(do.call(rbind, sslist) %>%
          get.score(type = "ss")) %>%
  rbind(do.call(rbind, bstlist) %>%
          get.score(type = "rw")) %>%
  ungroup %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  mutate(lasty = rmse[length(rmse)-2]) %>%
  ungroup() %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  labs(y = "RMSE",
       x = "Горизонт прогноза",
       title = "Ошибка прогноза для ансамблевых и байесовских методов") +
  theme_bw()+
  #guides(colour = guide_legend(title = ""))+
  theme(legend.title = element_blank())

cairo_pdf("plot/rfss_score.pdf", width = 10, height = 5)
print(rfss.score)
dev.off()



# Best level ----

best_level1 <- 
  rbind(do.call(rbind,map(reglist[c(1,5,4)], function(df) {df%>% select(-c(bestal, bestlam, nonzero, nzvars))})),
        do.call(rbind,map(rflist[2], function(df){df %>% select(-mtry) }))) %>%
  filter(nlead <= 9) %>%
  group_by(model, nlead, date) %>%
  mutate(y.pred = mean(y.pred)) %>%
  ungroup() %>%
  select(-c(y.true)) %>%
  unique %>%
  inner_join(unemp.true, by = "date") %>%
  group_by(model, nlead) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% 
  na.omit %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "Безработица"), size =1.5) +
  geom_line(aes(x = date, y = y.pred, colour = model), size = 0.8)+
  labs(title = "Предсказания безработицы (1-9 мес.)",y = "Уровень безработицы, %",
       x = "Дата") + theme_bw()+
  #guides(colour = guide_legend(title = ""))+
  theme(legend.title = element_blank())+
  facet_wrap(vars(nlead), scales = "free")

cairo_pdf("plot/bl1.pdf", width = 10, height = 5)
print(best_level1)
dev.off()


best_level2 <- 
  rbind(do.call(rbind,map(reglist[c(1,5,4)], function(df) {df%>% select(-c(bestal, bestlam, nonzero, nzvars))})),
        do.call(rbind,map(rflist[2], function(df){df %>% select(-mtry) }))) %>%
  filter(nlead > 9) %>%
  group_by(model, nlead, date) %>%
  mutate(y.pred = mean(y.pred)) %>%
  ungroup() %>%
  select(-c(y.true)) %>%
  unique %>%
  inner_join(unemp.true, by = "date") %>%
  group_by(model, nlead) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% 
  na.omit %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "Безработица"), size =1.5) +
  geom_line(aes(x = date, y = y.pred, colour = model), size = 0.8)+
  labs(title = "Предсказания безработицы (10-18 мес.)",y = "Уровень безработицы, %",
     x = "Дата") + theme_bw()+
  #guides(colour = guide_legend(title = ""))+
  theme(legend.title = element_blank())+
  facet_wrap(vars(nlead), scales = "free")

cairo_pdf("plot/bl2.pdf", width = 10, height = 5)
print(best_level2)
dev.off()


score_df <- rbind(do.call(rbind, c( reglist)) %>%
                      get.score(type = "regular"),
                  do.call(rbind, c( rflist)) %>%
                    get.score(type = "rf"),
                  do.call(rbind, c( bstlist)) %>%
                    get.score(type = "rw"),
                  do.call(rbind, c( sslist)) %>%
                    get.score(type = "ss")) %>%
                      ungroup %>%
  filter(model != "lasso_pc_lag") %>%
                      inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
                      mutate(rmse = rmse.x/rmse.y) %>%
  select(nlead, model, rmse) %>%
  dcast(model~nlead) %>% print(digits = 2)



## nonzero coefs
# reglist[c(1,4,5)]
nonzerotime <- rbind(do.call(rbind, reglist)) %>%
  filter(!model %in% c("lasso_pc_lag","lasso_lag", "ridge", "post_lasso")) %>%
  #filter(model == "lasso") %>%
  ungroup %>%
  group_by(model, nlead, date) %>%
  summarise(nonzero = mean(nonzero)) %>% 
  ungroup() %>%
  ggplot()+
  geom_line(aes(x = date, y = nonzero, colour = model), size = 0.8)+
  facet_wrap(vars(nlead), scales = "free")+
  labs(title = "Количество ненулевых коэффициентов \nдля моделей с регуляризацией",y = "Количество ненулевых коэффициентов",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = ""))

cairo_pdf("plot/nonzerotime.pdf", width = 10, height = 5)
print(nonzerotime)
dev.off()


nonzeroerror <-
  rbind(do.call(rbind, reglist)) %>%
  filter(!model %in% c("lasso_pc_lag","lasso_lag", "ridge")) %>%
 #filter(model == "lasso") %>%
  ungroup %>%
    mutate(error = (y.true - y.pred)^2) %>%
    inner_join(arlist[[2]] %>% mutate(ar.error = (y.true - y.pred)^2), by = c("nlead", "date"), suffix = c("", ".y")) %>%
    #mutate(error = error / ar.error) %>%
  group_by(model, nlead, date) %>%
  summarise(nonzero = mean(nonzero),
            error = mean(error)) %>%
    ungroup() %>%
    mutate(nlead = as.numeric(nlead)) %>%
  ggplot()+
  geom_point(aes(y = error, x = nonzero, colour = model), size = 1, alpha = 0.5)+
    geom_smooth(aes(y = error, x = nonzero,colour = model), se = FALSE, method = "lm")+
  facet_wrap(vars(nlead), scales = "free")+
  labs(title = "Распределение ошибок в зависимости от количества включенных \nпеременных для моделей с регуляризацией",x = "Количество ненулевых коэффициентов",
       y = "Квадрат ошибки (логарифмическая шкала)") +
  theme_bw()+
    scale_y_continuous(trans='log10')+
  guides(colour = guide_legend(title = ""))

cairo_pdf("plot/nonzeroerror.pdf", width = 14, height = 7)
print(nonzeroerror)
dev.off()



nzmat <- rbind(do.call(rbind, reglist)) %>%
  select(date, model, nlead, nzvars) %>%
  arrange(date) %>%
  unique

for(j in 2:ncol(df_tf)){
  nzmat[j+3] <- 0
  for(i in 1:nrow(nzmat)){
    if(grepl(paste0(" ",j-1, " "),nzmat$nzvars[i])){
      nzmat[i,j+3] <- 1
    } 
  }
}
colnames(nzmat)[-c(1:4)] <- colnames(df_tf)[-1]
nzheatmap1 <- nzmat %>% 
  filter(model == "lasso",nlead %in% c(1:6)) %>%
  select(-nzvars) %>%
  melt(id.vars = c("date", "model", "nlead")) %>%
  group_by(date, model, nlead, variable) %>%
  summarise(value = any(value)) %>%
  group_by(model, nlead, variable) %>%
  mutate(nnz = sum(value)) %>% 
  ungroup %>%
  group_by(model, nlead) %>%
  filter(nnz != 0 & nnz >= quantile(nnz, 0.6)) %>%
  ggplot() +
  geom_tile(aes(y = variable, x = date, fill = value),colour = "grey") +
  scale_fill_manual(values = c("white", "steelblue")) +
  facet_wrap(vars(nlead), scales = "free_y")  +
  labs(x = "Дата", y = "Переменная", title = "Включение переменных в модель LASSO для горизонта прогноза от 1 до 6 мес.")

cairo_pdf("plot/nzheatmap1.pdf", width = 12, height = 7)
print(nzheatmap1)
dev.off()

# error histogram
#errordens <-
  do.call(rbind, rflist) %>%
    select(model, nlead, date, y.true, y.pred) %>%
  rbind(do.call(rbind, sslist) %>%
    select(model, nlead, date, y.true, y.pred)) %>%
  rbind(do.call(rbind, bstlist) %>%
    select(model, nlead, date, y.true, y.pred) )%>%
  rbind(do.call(rbind, reglist) %>%
    select(model, nlead, date, y.true, y.pred)) %>%
  #filter(!model %in% c("lasso_pc_lag","lasso_lag", "ridge")) %>%
    #filter(model %in% c("lasso","lasso_lag", "lasso_adaptive", "post_lasso")) %>%
    #filter(model %in% c("lasso", "ridge", "elnet")) %>%
    filter(model %in% c("rf", "rf_unt", "boost")) %>%
    
  #filter(model == "lasso") %>%
  ungroup %>%
  mutate(error = abs(y.pred - y.true)) %>% 
  ggplot() +
  stat_summary(fun.y = "median",aes(y = error, x = date, colour = model), geom = "line", size = 0.9)+
  #geom_smooth(aes(y = error, x = date, colour = model)) +
  facet_wrap(vars(nlead), scales = "free")

print(errordens)

# #glmneterror <- 
#   do.call(rbind, reglist) %>%
#   select(-c(window, horizon)) %>%
#   filter(nlead <=6) %>%
#   group_by(date,  nlead, model) %>%
#   mutate(y.pred = mean(y.pred)) %>%
#   mutate(error = y.pred - y.true) %>%
#   #dplyr::filter(date == max(date)) %>%
#   ungroup %>%
#   ggplot() +
#   geom_density(aes(x = error, colour = model), size = 0.4)+
#   #geom_line(aes(x = date, y = y.true, colour = "true"), size =0.9) +
#   #geom_line(aes(x = date, y = abs(y.true - y.pred), colour = model), size = 0.9)+
#   labs(title = "Forecast error distribution",y ="unemploymnet")+
#   facet_grid(rows =vars(nlead), cols = vars(lambda), labeller = label_both)
# 
# pdf("plot/glmnet_error.pdf")
# print(glmneterror)
# dev.off()
# load("rawdata_panel.RData")
# unemp.true <- df$UNEMPL_M_SH %>%
#   as.data.frame() %>%
#   rownames_to_column %>%
#   setNames(c("date","y.true")) %>%
#   mutate(date = as.yearmon(date))
# # график в уровнях ----
# p <- do.call(rbind, lasso_list) %>%
#   filter(nlead %in% 2:4, lambda %in% 2:4) %>%
#   group_by(model, nlead, lambda, date) %>%
#   mutate(y.pred = mean(y.pred),
#          nonzero = mean(nonzero)) %>%
#   ungroup() %>%
#   select(-c(y.true, pred.date)) %>%
#   unique %>%
#   inner_join(unemp.true, by = "date") %>%
#   group_by(model, nlead, lambda) %>%
#   mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
#     nlag <- nlead[n]
#     lag(y.true,nlag)[n]
#   },nlead = nlead,y.true = y.true) + y.pred) %>% 
#   ggplot() +
#   geom_smooth(aes(x = date, y = y.true, colour = "true"), size =0.5, span = 0.7, se = F) +
#   geom_smooth(aes(x = date, y = y.pred, colour = model), size = 0.5, span = 0.7, se = F)+
#   ylab("unemploymnet")+
#   facet_grid(rows =vars(nlead),cols = vars(lambda))
# 
# pdf("LASSO_forecast.pdf")
# print(p)
# dev.off()
# 
# 
# 
# 
# load("reglist.RData")
# load("regpclist.RData")
# load("arlist.RData")
# load("rflist.RData")
# 
# 
# ar.score <- get.score(arlist[[2]], "ar")
# score_df <- rbind(do.call(rbind, c(regpclist, reglist)) %>%
#   get.score(type = "regular"),
#   get.score(rflist[[1]], type = "rf")) %>%
#   ungroup %>%
#   inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
#   mutate(rmse = rmse.x/rmse.y)
# score_df %>% 
#   #filter(!model %in% c("elnet", "ridge", "ridge_pc", "post_lasso", "post_lasso_lag","post_lasso_pc", "post_lasso_pc_lag")) %>%
#   group_by(model) %>%
#   mutate(lasty = last(rrse)) %>%
#   ungroup() %>%
#   #filter(model != "ridge_pc") %>%
#   ggplot()+
#   geom_point(aes(x = nlead, y = rmse, colour = model),height = 0, size = 2)#+
#   geom_label(aes(x = 24, y = lasty, label = model))
#   #geom_area(data = ar.score, aes(x = nlead, y = mae, fill = "ARMA(1,2)"), alpha= 0.1)+
# score_info <- score_df %>%
#   group_by(model, nlead) %>%
#   summarise(mean = mean(rmse),
#             sd = sd(rmse))
# score_df %>%
#   mutate(nlead=as.factor(nlead)) %>%
#   ggplot()+
#   geom_point(aes(y = rmse, x = nlead, colour = model), position = position_dodge(width = 0.9))#+
#   #geom_errorbar(data = score_info, aes(x = nlead, y = mean, ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.9))
