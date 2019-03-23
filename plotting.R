load("regurarlist.RData")
glmneterror <- 
  do.call(rbind, regurarlist) %>%
  select(-c(window, horizon)) %>%
  filter(nlead <=6) %>%
  group_by(date, lambda, nlead, model) %>%
  mutate(y.pred = mean(y.pred),
         nonzero = mean(nonzero)) %>%
  mutate(error = y.pred - y.true) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_density(aes(x = error, colour = model), size = 0.4)+
  #geom_line(aes(x = date, y = y.true, colour = "true"), size =0.9) +
  #geom_line(aes(x = date, y = abs(y.true - y.pred), colour = model), size = 0.9)+
  labs(title = "Forecast error distribution",y ="unemploymnet")+
  facet_grid(rows =vars(nlead), cols = vars(lambda), labeller = label_both)

pdf("plot/glmnet_error.pdf")
print(glmneterror)
dev.off()
load("rawdata_panel.RData")
unemp.true <- df$UNEMPL_M_SH %>%
  as.data.frame() %>%
  rownames_to_column %>%
  setNames(c("date","y.true")) %>%
  mutate(date = as.yearmon(date))
# график в уровнях ----
p <- do.call(rbind, lasso_list) %>%
  filter(nlead %in% 2:4, lambda %in% 2:4) %>%
  group_by(model, nlead, lambda, date) %>%
  mutate(y.pred = mean(y.pred),
         nonzero = mean(nonzero)) %>%
  ungroup() %>%
  select(-c(y.true, pred.date)) %>%
  unique %>%
  inner_join(unemp.true, by = "date") %>%
  group_by(model, nlead, lambda) %>%
  mutate(y.pred = sapply(seq_along(nlead), function(n,nlead, y.true){
    nlag <- nlead[n]
    lag(y.true,nlag)[n]
  },nlead = nlead,y.true = y.true) + y.pred) %>% 
  ggplot() +
  geom_smooth(aes(x = date, y = y.true, colour = "true"), size =0.5, span = 0.7, se = F) +
  geom_smooth(aes(x = date, y = y.pred, colour = model), size = 0.5, span = 0.7, se = F)+
  ylab("unemploymnet")+
  facet_grid(rows =vars(nlead),cols = vars(lambda))

pdf("LASSO_forecast.pdf")
print(p)
dev.off()



lasso.out.lag %>%
  
  group_by(date, lambda) %>%
  mutate(y.pred = mean(y.pred)) %>%
  #dplyr::filter(date == max(date)) %>%
  ungroup %>%
  ggplot() +
  geom_line(aes(x = date, y = y.true, colour = "true")) +
  geom_line(aes(x = date, y = y.pred, colour = "pred"))+
  facet_wrap(vars(lambda))

# Теперь используем Post-LASSO




get.score <- function(df){
  df %>% 
    group_by(horizon, window, lambda, nlead, model) %>%
    summarise(rmspe = RMSPE(y.pred, y.true),
              rmse = RMSE(y.pred, y.true),
              mae= MAE(y.pred, y.true),
              r2 = R2_Score(y.pred, y.true),
              nonzero = mean(nonzero)) %>%
    ungroup
}


score_df <- rbind(get.score(lasso.out),
                  get.score(lasso.out.lag) %>% mutate(model = "lasso_lag"))
score_df
score_info <- score_df %>%
  group_by(model, nlead) %>%
  summarise(mean = mean(rmse),
            sd = sd(rmse))
score_df %>%
  mutate(nlead=as.factor(nlead)) %>%
  ggplot()+
  geom_point(aes(y = rmse, x = nlead, colour = model), position = position_dodge(width = 0.9))+
  geom_errorbar(data = score_info, aes(x = nlead, y = mean, ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.9))

