
df_lag <- create_lag(df, 4)
df_lag$y <- lag.xts(df_lag$investment, k = -1)
df_lag %<>% na.omit
# тренировка на расширенном массиве данных -----
# с 1995 c лагами (в итоге из-за взятия разностей и 4 лагов --- с 1997)
X.mat_full <- model.matrix(y~0+., data = df_lag)


X.train <- X.mat_full[1:68,]
X.test <- X.mat_full[69:88,]
y.train <- df_lag$y["1997-01/2013-12"] %>% as.numeric
y.test <- df_lag$y["2014-01/2018-12"]%>% as.numeric


# тренировка на малом массиве данных (с 1999)
X.train <- X.mat_full[9:60,]
X.test <- X.mat_full[61:88,]
X.mat_full <- X.mat_full[9:88,]
y.train <- df_lag$y["1999-01/2011-12"] %>% as.numeric
y.test <- df_lag$y["2012-01/2018-12"]%>% as.numeric



# lasso----
tc <- trainControl(method = "timeslice", initialWindow = 40,horizon = 10,fixedWindow = TRUE)
glm.out <- train(x=X.train,
                 y=y.train,
                 method = "glmnet",
                 metric = "RMSE",trControl = tc,
                 tuneGrid = expand.grid(.alpha = c(1),.lambda = seq(0.05,0.0001,length = 300)))


plot(glm.out)

best_lam <- glm.out$bestTune[1,2]

lasso_best <- glmnet(X.train,y.train, alpha = 1, lambda = c(best_lam))
pred_lasso <- predict(lasso_best,newx = X.test)
ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+

  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(lasso_best, newx = X.mat_full)[,1],
                color = "LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))



RMSE(pred_lasso[,i], y.test %>% as.numeric)

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

  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ridge, newx = X.mat_full)[,1],
                color = "Ridge"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
ridge_p

pred_ridge <- predict(m_ridge,newx = X.test)

RMSE(pred_ridge[,i], y.test)


w3 <- 1/abs(as.numeric(coef(m_ridge))
            [1:(ncol(X.train))] )^0.5 ## Using gamma = 1
w3[which(w3 == Inf)] <- 999999999 ## Replacing values estimated as Infinite for 999999999
m_ada <- glmnet(X.train, y.train, alpha = 1, lambda = best_lam, penalty.factor = w3)

ada_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ada, newx = X.mat_full)[,1],
                color = "Adpative LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
ada_p

pred_ada <- predict(m_ada,newx = X.test)# %>% melt
predict(m_ada, type="coef")
for (i in 1:ncol(pred_ada)){
  RMSE(pred_ada[,i], y.test) %>% print
}


# post-lasso ----
nzvars <- predict(lasso_best, type = "nonzero") %>% .[[1]]

nzvars
train_post <- as.data.frame(X.train) %>%
  select(nzvars) %>%
  mutate(y = y.train)
test_post <-  as.data.frame(X.test) %>%
  select(nzvars) %>%
  mutate(y = 0)
m_post <- lm(y~., data = train_post)
m_post %>% summary

pred_post <- predict(m_post,newdata = test_post)# %>% melt
RMSE(pred_post, y.test) %>% print

post_p <- ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_post, newdata = rbind(train_post, test_post)),
                color = "Post-LASSO"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
post_p


# random forest ----
tunegrid <- expand.grid(.mtry=seq(5,10))
tc_rf <- trainControl(method='repeatedcv', 
             number=10, 
             repeats=3, 
             search='grid')

rf.out <- train(x = X.train,
                y = y.train,
                method = "rf", 
                metric = "RMSE",
                trControl = tc_rf,
                tuneGrid = tunegrid)

bestmtry <- rf.out$bestTune[1,1]
print(bestmtry)

m_rf <- randomForest(x = X.train, y = y.train, mtry = 6)
pred_rf <- y.pred.ts <- predict(m_rf, newdata = X.test) %>%
  as.numeric
RMSE(pred_rf,y.test)

ggplot()+geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_rf, newdata = X.mat_full),
                color = "Случайный лес"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))



# spike and slab----
m_ss <- spikeslab(x = X.train, y = y.train, n.iter2 = 10000)
m_ss %>% print
m_ss$summary
m_ss$model %>% melt

pred_ss <- predict(m_ss, newdata = X.test)$yhat.gnet
RMSE(pred_ss,y.test)


ggplot()+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =predict(m_ss, newdata = X.mat_full)$yhat.gnet,
                color = "Пик-плато"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))



# arima ----
arima_m <- Arima(y.train,order = c(3, 1, 3), fixed = c(NA, NA, NA, NA, NA, NA))
ggtsdisplay(y.train)
arima_pred <- Arima(c(y.train, y.test), model=arima_m) %>% fitted() %>% as.numeric()

ggplot()+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr, y = c(y.train, y.test)))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr,
                y =arima_pred,
                color = "ARMA"))+
  geom_vline(aes(xintercept  = as.yearqtr("2012-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))


# Результаты ----

# Arima
RMSE(arima_pred[-(1:length(y.train))],y.test)

# LASSO
RMSE(pred_lasso[,i], y.test)

# Ridge
RMSE(pred_ridge[,i], y.test)

# Adaptive LASSO
RMSE(pred_ada[,i], y.test)

#Post-LASSO

RMSE(pred_post, y.test)

# Random Forest

RMSE(pred_rf,y.test)

# Spike-and-Slab
RMSE(pred_ss,y.test)



# plot ----

p <- ggplot()+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr%>% as.Date, y = c(y.train, y.test)))+
  geom_vline(aes(xintercept  = as.Date("2014-01-01")), color = "red",linetype="dashed")+
  labs(title = "",
       y = "Изменение инвестиций (log)",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))+
  
  geom_line(aes(x = row.names(X.mat_full)%>% as.yearqtr %>% as.Date,
                y =predict(lasso_best, newx = X.mat_full)[,1],
                color = "LASSO"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr  %>% as.Date,
                y =predict(m_ridge, newx = X.mat_full)[,1],
                color = "Ridge"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr %>% as.Date,
                y =predict(m_ada, newx = X.mat_full)[,1],
                color = "Adpative LASSO"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr %>% as.Date,
                y =predict(m_post, newdata = rbind(train_post, test_post)),
                color = "Post-LASSO"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr %>% as.Date,
                y =predict(m_rf, newdata = X.mat_full),
                color = "Случайный лес"))+
  geom_line(aes(x = row.names(X.mat_full) %>% as.yearqtr %>% as.Date,
                y =predict(m_ss, newdata = X.mat_full)$yhat.gnet,
                color = "Пик-плато"))

p2 <- p +scale_x_date(date_breaks = "1 year", 
                labels=date_format("%Y"),
                limits = as.Date(c("2014-01-01", '2019-01-31')))+
  ylim(c(-.15, .12))+
  ylab('')
ggarrange(p, p2,  common.legend = TRUE, legend="right")

# добавить прогнозирование на несколько кварталов
# 