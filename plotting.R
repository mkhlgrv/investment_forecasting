
load("rawdata_panel.RData")
unemp.true <- df$UNEMPL_M_SH %>%
  as.data.frame() %>%
  rownames_to_column %>%
  setNames(c("date","y.true")) %>%
  mutate(date = as.yearmon(date))
# unemp
load("rawdata.RData")
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

load("reglist.RData")
ar.score <- get.score(arlist[[2]], "ar")
reg.score <- 
  rbind(do.call(rbind, reglist) %>%
                    get.score(type = "regular")) %>%
  ungroup %>%
  filter(nlead <=18) %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  mutate(lasty = rmse[length(rmse)-2]) %>%
  ungroup() %>%
  filter(model != "elnet") %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  geom_label(aes(x = 16, y = lasty, label = model, colour = model)) +
    labs(y = "RMSE",
         x = "Горизонт прогноза") +
    theme_bw()+
    guides(colour = guide_legend(title = ""))

cairo_pdf("plot/reg_score.pdf", width = 10, height = 5)
print(reg.score)
dev.off()


# LASSO with PC
load("regpclist.RData")
ar.score <- get.score(arlist[[2]], "ar")
regpc.score <- 
rbind(do.call(rbind, regpclist) %>%
        get.score(type = "regular")) %>%
  ungroup %>%
  filter(nlead <=18) %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  mutate(lasty = rmse[length(rmse)-2]) %>%
  ungroup() %>%
  filter(model != "elnet") %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  geom_label(aes(x = 16, y = lasty, label = model, colour = model)) +
  labs(y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  theme(legend.position = "none")

cairo_pdf("plot/regpc_score.pdf", width = 10, height = 5)
print(regpc.score)
dev.off()


# RF and SS ----
load("rflist.RData")
load("sslist.Rdata")
ar.score <- get.score(arlist[[2]], "ar")
rfss.score <- 
  rbind(get.score(rflist[[1]],type = "rf"),
        get.score(sslist[[1]],type = "ss")) %>%
  ungroup %>%
  filter(nlead <=18) %>%
  inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
  mutate(rmse = rmse.x/rmse.y) %>%
  group_by(model) %>%
  mutate(lasty = rmse[length(rmse)-2]) %>%
  ungroup() %>%
  filter(model != "elnet") %>%
  ggplot()+
  geom_point(aes(x = nlead, y = rmse, colour = model), size = 2)+
  geom_line(aes(x = nlead, y = rmse, colour = model), size = 1)+
  geom_label(aes(x = 16, y = lasty, label = model, colour = model)) +
  labs(y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  theme(legend.position = "none")

cairo_pdf("plot/rfss_score.pdf", width = 10, height = 5)
print(rfss.score)
dev.off()



# Best level ----

best_level1 <- 
  rbind(reglist[[3]] %>% select(-c(bestal, bestlam, nonzero)), sslist[[1]], rflist[[1]] %>% select(-mtry)) %>%
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
  geom_line(aes(x = date, y = y.true, colour = "Безработица"), size =1) +
  geom_line(aes(x = date, y = y.pred, colour = model), size = 0.6)+
  labs(y = "Уровень безработицы, %",
       x = "Дата") + theme_bw()+
  guides(colour = guide_legend(title = ""))+
  facet_wrap(vars(nlead), scales = "free")

cairo_pdf("plot/bl1.pdf", width = 10, height = 5)
print(best_level1)
dev.off()


best_level2 <- 
  rbind(reglist[[3]] %>% select(-c(bestal, bestlam, nonzero)), sslist[[1]], rflist[[1]] %>% select(-mtry)) %>%
  filter(nlead > 9, nlead <=18) %>%
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
  geom_line(aes(x = date, y = y.pred, colour = model), size = 0.6)+
  labs(y = "Уровень безработицы, %",
       x = "Дата") + theme_bw()+
  guides(colour = guide_legend(title = ""))+
  facet_wrap(vars(nlead), scales = "free")

cairo_pdf("plot/bl2.pdf", width = 10, height = 5)
print(best_level2)
dev.off()


score_df <- rbind(do.call(rbind, c(regpclist, reglist)) %>%
                      get.score(type = "regular"),
                      get.score(rflist[[1]], type = "rf"),
                  get.score(sslist[[1]], type = "ss")) %>%
                      ungroup %>%
                      inner_join(ar.score %>% ungroup %>% select(nlead, rmse), by= "nlead") %>%
                      mutate(rmse = rmse.x/rmse.y) %>%
  select(nlead, model, rmse) %>%
  dcast(model~nlead)


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
