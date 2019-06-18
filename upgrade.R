df_full <- df_short#['2001-01/2018-12']
for(i in 4:nrow(df_full)){
  df_full$gdp_current[i] <- sum(df_short$gdp_current[(i-3):i], na.rm = TRUE)
  df_full$investment_current[i] <- sum(df_short$investment_current[(i-3):i], na.rm = TRUE)
}
df_y <- df_full[which(quarter(time(df_full))==4),]

df_full <- df_short

df_full$net_capital <- (df_full$capital/1000*(1-df_full$depreciation/100))
df_full$q <- df_full$rts/df_full$net_capital
df_full$qcap <- df_full$q*lag(df_full$net_capital)

df_full$cost = df_full$deflator/100 *
  (df_full$gov/100 +df_full$delta- df_full$deflator/lag(df_full$deflator)+1)



df_full$pyc <- df_full$gdp_current/df_full$cost
df_full$pyc_lag <- lag(df_full$gdp_current)/df_full$cost
df_full$depr <- df_full$delta*lag(df_full$capital)/400000

df_full$cost_lag <- lag(df_full$cost)
# df_full$gdp_current <- df_full$GDPEA_Q_DIRI


df_full$gdp_lag <- lag(df_full$gdp)
df_full$investment_lag <-  lag(df_full$investment_current)
df_full$qcap_lag <- lag(df_full$qcap)
df_full$qcap_lag2 <- lag(df_full$qcap,2)

#df_full <- df_full#['2001-01/2018-12']
df <- df_full#['1999-01/2018-12'] # train set
# простая акселераторная модель
# I_net(t) ~ Y(t) - Y(t-1)

# годовые данные
# df_y <- df_full[which(quarter(time(df_full))==4),]

df_y$net_capital<- (df_y$capital/1000*(1-df_y$depreciation/100))
acc_m <- lm(diff.xts(net_capital)~0+diff.xts(gdp_current), data = df_y, na.action = na.exclude)
acc_m %>% summary

# mu = 0.6583
ggplot() + geom_point(aes(time(df_y), resid(acc_m)))

ggplot() + geom_line(aes(time(df_y), fitted(acc_m), color = "fitted"))+
  geom_line(aes(time(df_y), diff.xts(df_y$net_capital), color = "true"))


# оценка величины mu
((df_y$net_capital/df_y$gdp_current)) %>% plot()
# 1.045598

df_y$deflator <- cumprod(df_y$deflator)
# акселераторная модель с лагами - 1

# I(t) = mu lambda Y(t) - (1-delta) mu lambda Y(t-1) + (1-lambda)I(t-1)

# очистка от экзогенности
df_y$gdp_net <- NA
df_y$gdp_net['1996-01/2018-12'] <- (lm(diff.xts(gdp_current)~diff.xts(oil)+lag(diff.xts(gdp_current)), df_y)$fitted.values %>% as.numeric %>% cumsum)+2007.825



mu_df_yearly <- (1:10) %>% map_dfr(function(i){
  acc_m <- lm(investment_current~1+
                gdp_net + lag(gdp_current)+
                lag(investment_current), data = df_y[1:(15+i),], na.action = na.exclude)
  data.frame(date = time(df_y)[15+i],
             mu = coef(acc_m)[2]/(1-coef(acc_m)[4]))
})

ggplot() + geom_line(data = mu_df_yearly, aes(x = date, y = mu)) +
  geom_line(data = df_y['2000-01/2018-12'],
            aes(x = time(df_y['2000-01/2018-12']), y = net_capital/gdp_current))



#linearHypothesis(acc_m,c("gdp_net = 0.07196"," lag(investment_current) = 0.87785"),test="F")



# годовые показатели
# lambda = 0.5266
# mu = 0.3590011
# delta = 0.6120603




ggplot() + geom_line(aes(time(df_y), diff.xts(fitted(acc_m),4,log = TRUE), color = "fitted"))+
  geom_line(aes(time(df_y), diff.xts(df_y$investment_current,4, log = TRUE), color = "true"))


acc_e <- lm(investment_current~0+
              gdp_current + lag(gdp_current)+lag(gdp_current,2)+
              lag(net_capital), data = df_y, na.action = na.exclude)
acc_e %>% summary

ggplot()+geom_line(aes(x = row.names(acc_e$model) %>% as.yearqtr, y = acc_e$model[,1]))+
  geom_line(aes(x = row.names(acc_e$model) %>% as.yearqtr, y =acc_e$fitted.values, color = "fitted"))




# неоклассическая модель
# I(t) ~ sum(p) (P*Y / C)(t-p) + K(t-1)
# C = J(t-1) * (r + delta  - (J(t)-J(t-1)/J(t-1))) издержки владения капиталом
# r = i - pi реальная ставка
# надо не cpi а дефлятор
# J - индекс цен на стройматериалы и работы CONI_Q_CHI

# df$cost = df$price_capital/100 *
#   (df$gov/100 +df$delta- df$price_capital/lag(df$price_capital)+1)
# 
# df$pyc <- df$gdp_current/df$cost
# df$pyc_lag <- lag(df$gdp_current)/df$cost
# df$depr <- df$delta*lag(df$capital)/100000

neocl_m <- lm(diff.xts(investment_current,4, log = TRUE)~diff.xts(pyc,4, log = TRUE)+
                diff.xts(pyc_lag,4, log = TRUE)+diff.xts(depr,4, log = TRUE),
              data = df['1999-12/2014-12'])
# очень большая разница очень круто выглядит
neocl_m <- lm(diff.xts(investment_current,4, log = TRUE)~diff.xts(gdp_current,4, log = TRUE)+
                lag(diff.xts(gdp_current,4, log = TRUE))+
                diff.xts(cost,4, log = TRUE)+
                lag(diff.xts(cost,4, log = TRUE))+
                lag(diff.xts(investment_current,4, log = TRUE))+
                diff.xts(depr,4, log = TRUE)  ,
              data = df['1999-12/2014-12'])

neocl_m %>% summary()

ggplot()+geom_line(aes(x = row.names(neocl_m$model) %>% as.yearqtr, y = neocl_m$model[,1]))+
  geom_line(aes(x = row.names(neocl_m$model) %>% as.yearqtr, y =neocl_m$fitted.values, color = "fitted"))


# Tobin q model

# df$net_capital <- (df$capital/1000*(1-df$depreciation/100))
# df$q <- df$moex/df$net_capital
# df$qcap <- df$q*lag(df$net_capital)
q_m <- lm(diff.xts(investment_current,4, log = TRUE)~diff.xts(qcap,4, log = TRUE)+
            lag(diff.xts(qcap,4, log = TRUE))+
            lag(diff.xts(qcap,4, log = TRUE),2), data = df)
q_m %>% summary

ggplot()+geom_line(aes(x = row.names(q_m$model) %>% as.yearqtr, y = q_m$model[,1]))+
  geom_line(aes(x = row.names(q_m$model) %>% as.yearqtr, y =q_m$fitted.values, color = "fitted"))



# SARIMA model 

auto.arima(diff.xts(df$investment_current['2000-01/2014-12'],4, log = TRUE))
  


lag_m <- Arima(diff.xts(df$investment_current['2000-01/2014-12'],4, log = TRUE), order = c(2,1,2),seasonal = c(0,0,1), include.mean = TRUE)
lag_m %>% summary
forecast(lag_m, 12) %>% autoplot


# Вводим дополнительные переменные (LASSO)

# Неоклассическая модель
# надо как-то попробовать оценить остатки

df_full[,54:60] %>% as.zoo() %>% autoplot() + facet_free()


# у всех с 1:39 diff.xts
# 1 FC убрать
# 8 убрать
# 11 убрать
# 38 ВВП убрать
# 41, 42 ввп инвест в текущих ценах
# 43 deflator
# 44 - 46 beliefs (не надо пока что)
# 47 - 51,  технические показатели (убрать)
# 52 есть в издержках diff 4 (можно оставить)
# 53 moex  нормально diff 4
# 54 bankrate норм
# 55 net capital
# 56 oil оставить
# 57 q delete
# 58 qcap diff 4
# 59 cost diff 4
# 60 pyc delete
# 61 pyc lag delete
# 63 depr diff 4
# 63:67 лаги для неколассической модели



#todel <- c(1,8,11,38, 44:51, 57, 60, 61, 63,66:67)# for accelerator

todel <- c(1,8,11,38, 44:51, 57, 60, 61, 63:67)
df.mat_full <- df_full[,-todel] 


for(i in 1:ncol(df.mat_full)){
  df.mat_full[,i] %<>% diff.xts(4,log = TRUE)
}

# очистка от экзогенности
df.mat_full$gdp_net <- NA

df.mat_full$gdp_net['2000-09/2018-12'] <- lm(gdp_current~oil+deflator,df.mat_full['2000-09/2018-12'] )$fitted.values %>% as.numeric


# only for accelerator:-----
# df.mat_full$gdp_lag <- lag(df.mat_full$gdp_net)
# 
# df.mat_full$gdp_current <- df.mat_full$gdp_net
################

df.mat_full$gdp_net <- NULL 


df.mat_full%<>% na.omit()
X.mat_full <- model.matrix(investment_current~0+., data = df.mat_full)
X.train <- X.mat_full[1:56,]
X.test <- X.mat_full[57:72,]
y.train <- df.mat_full$investment_current["2001-01/2014-12"] %>% as.numeric
y.test <- df.mat_full$investment_current["2015-01/2018-12"]%>% as.numeric

# accelerator ----





acc_m <- lm(investment_current~gdp_net+gdp_lag+investment_lag,
            data = cbind(X.train, 
                         data.frame(investment_current= y.train)))

acc_df <- cbind(rbind(X.train, X.test),data.frame(investment_current= c(y.train, y.test)))
acc_m %>% summary
pred <- predict(acc_m, newdata = cbind(X.test, data.frame(investment_current= y.test)))

RMSE(pred, y.test)
acc_plot <- ggplot()+geom_line(aes(x = row.names(acc_df) %>% as.yearqtr, y = acc_df$investment_current))+
  # geom_line(aes(x = row.names(acc_df) %>% as.yearqtr,
  #                             y =predict(lasso_neoc_m, newdata = acc_df),
  #                             color = "lasso"))+
  geom_line(aes(x = row.names(acc_df) %>% as.yearqtr,
                y =predict(acc_m, newdata = acc_df),
                color = "Акселератор"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed") + 
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))


acc_plot



# lasso----
tc <- trainControl(method = "timeslice", initialWindow = 40,horizon = 10,fixedWindow = TRUE)
glm.out <- train(x=X.train,
                 y=y.train,
                 method = "glmnet",
                 metric = "RMSE",trControl = tc,
                 tuneGrid = expand.grid(.alpha = c(1),.lambda = seq(0.05,0.0001,length = 300)))


plot(glm.out)

best_lam <- glm.out$bestTune[1,2]

lasso_best <- glmnet(X.train,y.train, alpha = 1, lambda = c(best_lam,best_lam/2,best_lam/3, best_lam/4, best_lam/5))
pred <- predict(lasso_best,newx = X.test) #%>% melt
lasso_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  # geom_line(aes(x = row.names(acc_df) %>% as.yearqtr,
  #                             y =predict(lasso_neoc_m, newdata = acc_df),
  #                             color = "lasso"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(lasso_best, newx = X.mat_full)[,1],
                color = "LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

lasso_p

for (i in 1:ncol(pred)){
  RMSE(pred[,i], y.test %>% as.numeric) %>% print
}
predict(lasso_best, type="coef")

# adaptive lasso ----
# сначала ridge
train.ridge <- train(x=X.train,
                 y=y.train,
                 method = "glmnet",
                 metric = "RMSE",trControl = tc,
                 tuneGrid = expand.grid(.alpha = c(0),.lambda = seq(0.05,0.0001,length = 300)))
m_ridge <- glmnet(X.train, y.train, alpha = 0, lambda = train.ridge$bestTune[1,2])

ridge_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  # geom_line(aes(x = row.names(acc_df) %>% as.yearqtr,
  #                             y =predict(lasso_neoc_m, newdata = acc_df),
  #                             color = "lasso"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ridge, newx = X.mat_full)[,1],
                color = "Ridge"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
ridge_p

pred <- predict(m_ridge,newx = X.test)
for (i in 1:ncol(pred)){
  RMSE(pred[,i], y.test) %>% print
}

w3 <- 1/abs(as.numeric(coef(m_ridge))
              [1:(ncol(X.train))] )^0.5 ## Using gamma = 1
w3[which(w3 == Inf)] <- 999999999 ## Replacing values estimated as Infinite for 999999999
m_ada <- glmnet(X.train, y.train, alpha = 1, lambda = best_lam, penalty.factor = w3)

ada_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ada, newx = X.mat_full)[,1],
                color = "Adpative LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
ada_p

pred <- predict(m_ada,newx = X.test)# %>% melt
predict(m_ada, type="coef")
for (i in 1:ncol(pred)){
  RMSE(pred[,i], y.test) %>% print
}


# post-lasso
nzvars <- predict(lasso_best, type = "nonzero") %>% .[[1]]

train_post <- as.data.frame(X.train) %>%
  select(nzvars) %>%
  mutate(y = y.train)
test_post <-  as.data.frame(X.test) %>%
  select(nzvars) %>%
  mutate(y = 0)
m_post <- lm(y~., data = train_post)
m_post %>% summary

pred <- predict(m_post,newdata = test_post)# %>% melt
RMSE(pred, y.test) %>% print

post_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_post, newdata = rbind(train_post, test_post)),
                color = "Post-LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
post_p

# lasso+ neoclassical model-----

nznames <- colnames(X.mat_full[,nzvars])
lasso_neocl_df <- df_full[,c(nznames,colnames(df_full)[c(42,60:61,62)])]
for(i in 1:ncol(lasso_neocl_df)){
  lasso_neocl_df[,i] <- diff.xts(lasso_neocl_df[,i],4, log = TRUE)
}
lasso_neocl_df %<>% .['2001-01/2018-12']
lasso_neocl_df %<>% as.data.frame()
lasso_neoc_m <- lm(investment_current~., data = lasso_neocl_df[1:56,])
lasso_neoc_m %>% summary

neocl_m <- lm(investment_current~
     pyc+pyc_lag+
     depr , data = lasso_neocl_df[1:56,])
neocl_m %>% summary()
confint(neocl_m)
# coeftest(neocl_m,vcov. = vcovHC(neocl_m))


confint(lasso_neoc_m)
# coeftest(lasso_neoc_m,vcov. = vcovHC(lasso_neoc_m))
lasso_neoc_m %>% resid %>% qqPlot()
neocl_m%>% resid %>% qqPlot()


ggplot()+geom_line(aes(x = row.names(lasso_neocl_df) %>% as.yearqtr, y = lasso_neocl_df$investment_current))+
  geom_line(aes(x = row.names(lasso_neocl_df) %>% as.yearqtr,
                              y =predict(lasso_neoc_m, newdata = lasso_neocl_df),
                              color = "Неоклассическая +\nLASSO"))+
  geom_line(aes(x = row.names(lasso_neocl_df) %>% as.yearqtr,
                              y =predict(neocl_m, newdata = lasso_neocl_df),
                              color = "Неоклассическая"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
pred <- predict(lasso_neoc_m,newdata = lasso_neocl_df[57:72,])
pred <- predict(neocl_m,newdata = lasso_neocl_df[57:72,])

RMSE(pred, y.test) %>% print
R2_Score(pred,y.test) %>% print
# оценка Tobin q model при помощи LASSO -----

nznames <- colnames(X.mat_full[,nzvars])
lasso_tobin_df <- df_full[,c(nznames,colnames(df_full)[c(42,58,66:67)])]
for(i in 1:ncol(lasso_tobin_df)){
  lasso_tobin_df[,i] <- diff.xts(lasso_tobin_df[,i],4, log = TRUE)
}
lasso_tobin_df %<>% .['2001-01/2018-12']
lasso_tobin_df %<>% as.data.frame()
lasso_q_m <- lm(investment_current~., data = lasso_tobin_df[1:56,])
lasso_q_m %>% summary

tobin_m <- lm(investment_current~qcap+qcap_lag+qcap_lag2, data = lasso_tobin_df[1:56,])
tobin_m %>% summary()
confint(tobin_m)
# coeftest(tobin_m,vcov. = vcovHC(tobin_m))


confint(lasso_q_m)
# coeftest(lasso_q_m,vcov. = vcovHC(lasso_q_m))
lasso_q_m %>% resid %>% qqPlot()
tobin_m %>%  resid %>% qqPlot()

ggplot()+geom_line(aes(x = row.names(lasso_tobin_df) %>% as.yearqtr, y = lasso_tobin_df$investment_current))+
  geom_line(aes(x = row.names(lasso_tobin_df) %>% as.yearqtr,
                y =predict(lasso_q_m, newdata = lasso_tobin_df),
                color = "q Тобина+LASSO"))+
  geom_line(aes(x = row.names(lasso_tobin_df) %>% as.yearqtr,
                y =predict(tobin_m, newdata = lasso_tobin_df),
                color = "q Тобина"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
pred <- predict(lasso_q_m,newdata = lasso_tobin_df[57:72,])
pred <- predict(tobin_m,newdata = lasso_tobin_df[57:72,])

RMSE(pred, lasso_tobin_df[57:72,"investment_current"]) %>% print
R2_Score(pred,y.test) %>% print
# LASSO+AR(1)

lasso_ar_df <- df_full[,c(nznames,colnames(df_full)[c(42,65)])]
for(i in 1:ncol(lasso_ar_df)){
  lasso_ar_df[,i] <- diff.xts(lasso_ar_df[,i],4, log = TRUE)
}
lasso_ar_df %<>% .['2001-01/2018-12']
lasso_ar_df %<>% as.data.frame()

ar_m <- lm(investment_current~., data = lasso_ar_df) 
ar_1 <- lm(investment_current~investment_lag, data = lasso_ar_df) 
ar_m %>% summary
ar_1 %>% summary

ggplot()+geom_line(aes(x = row.names(lasso_ar_df) %>% as.yearqtr, y = lasso_ar_df$investment_current))+
  geom_line(aes(x = row.names(lasso_ar_df) %>% as.yearqtr,
                y =predict(ar_m, newdata = lasso_ar_df),
                color = "LASSO + AR(1)"))+
  geom_line(aes(x = row.names(lasso_ar_df) %>% as.yearqtr,
                y =predict(ar_1, newdata = lasso_ar_df),
                color = "AR(1)"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

pred <- predict(ar_m,newdata = lasso_ar_df[57:72,])
pred <- predict(ar_1,newdata = lasso_ar_df[57:72,])

RMSE(pred, lasso_ar_df[57:72,"investment_current"]) %>% print
# spike and slab----
m_ss <- spikeslab(x = X.train, y = y.train, n.iter2 = 10000)
m_ss %>% print
m_ss$summary
m_ss$model %>% melt
R2_Score(pred,y.test) %>% print

ss_prob <-  1:ncol(X.train) %>% map_dfr(function(i){
  prob <-sum(m_ss$model %>% melt %>%.$value==i)/10000
  data.frame(name = colnames(X.train)[i], num = i, prob=prob)
})
#predict(m_ss, newdata = X.test)$yhat.gnet
ss_prob %>% arrange(desc(prob)) %>% view
RMSE(predict(m_ss, newdata = X.test)$yhat.gnet,y.test) %>% print

ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ss, newdata = X.mat_full)$yhat.gnet,
                color = "Пик-плато"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))


# random forest ----

tc <- trainControl(method="timeslice", initialWindow = 40,fixedWindow = TRUE, horizon = 10)

tunegrid <- expand.grid(.mtry=seq(1,10))

rf.out <- train(x = X.train,
                y = y.train,
                method = "rf", 
                metric = "RMSE",
                trControl = tc,
                tuneGrid = tunegrid)

bestmtry <- rf.out$bestTune[1,1]
print(bestmtry)

m_rf <- randomForest(x = X.train, y = y.train, mtry = 6)
pred <- y.pred.ts <- predict(m_rf, newdata = X.test) %>%
  as.numeric
RMSE(pred,y.test) %>% print
R2_Score(pred,y.test) %>% print

ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_rf, newdata = X.mat_full),
                color = "Случайный лес"))+
  geom_vline(aes(xintercept  = as.yearqtr("2015-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))



# spike and slab----
ss.prob.df <- (0:16) %>% map_dfr(function(j){
  m_ss <- spikeslab(x = X.train[(1+j):(40+j),], y = y.train[(1+j):(40+j)], n.iter2 = 1000)
  ss_prob <-  (1:ncol(X.train)) %>% map_dfr(function(i){
    prob <-sum(m_ss$model %>% melt %>%.$value==i)/1000
    data.frame(name = colnames(X.train)[i], date = rownames(X.train)[40+j], prob=prob)
  })
})

ss.prob.df
m_ss$model %>% melt
R2_Score(pred,y.test) %>% print

ss.prob.df %>% group_by(name) %>% filter(mean(prob)>0.15) %>% ggplot(aes(x = as.yearqtr(date), y = prob, color = name))+geom_line()
