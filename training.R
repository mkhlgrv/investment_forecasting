
# функция для вызовы
map_dfr(list(list1, list2, list3, list4, list5, list6),
        function(x){
          print(x)
          data.frame(model = x$model,
                     series = x$series,
                     lag = x$lag,
                     startdt=x$startdt,
                     enddt = x$enddt,
                     date = x$date,
                     h = x$h,
                     pred = x$pred)
})




# # Результаты ----
# 
# 
# # Arima
# write('Arima',file="data/rmse2.txt",append=TRUE)
# RMSE(arima_pred[-(1:length(y.train))],y.test) %>%
# write(file="data/rmse2.txt",append=TRUE)
# # LASSO
# 
# write('LASSO',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_lasso[,i], y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# # Ridge
# 
# write('Ridge',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_ridge[,i], y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# # Adaptive LASSO
# 
# write('Adaptive LASSO',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_ada[,i], y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# #Post-LASSO
# 
# write('Post-LASSO',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_post, y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# # Random Forest
# 
# write('Random Forest',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_rf,y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# # Spike-and-Slab
# write('Spike-and-Slab',file="data/rmse2.txt",append=TRUE)
# RMSE(pred_ss,y.test)%>%
#   write(file="data/rmse2.txt",append=TRUE)
# 
# 

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

p2 <- p +
  scale_x_date(date_breaks = "1 year", 
                labels=date_format("%Y"),
                limits = as.Date(c("2014-01-01", '2019-01-31')))+
  ylim(c(-.15, .12))+
  ylab('')
ggarrange(p, p2,  common.legend = TRUE, legend="right")

# добавить прогнозирование на несколько кварталов
# добавить переменные деловой активности, спреды (cds russia foreign spread)
# показать, что отношение инвестиций к ввп значимо (Сток, Уотсон) 

