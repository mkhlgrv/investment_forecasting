# простая акселераторная модель
# I(t) ~ Y(t) - Y(t-1)
acc_m <- lm(log(FC)~diff.xts(GDPEA_Q_DIRI,lag = 1, log = TRUE), data = df_long[30:100,])
acc_m %>% summary
acc_m %>% residuals %>% plot

acc_e <- lm(diff.xts(FC,log = TRUE, lag = 4)~0+diff.xts(GDPEA_Q_DIRI,log = TRUE, lag = 4)+
              lag(log(GDPEA_Q_DIRI), n = 4)+
              lag(log(FC), n = 4), data = df_long[30:101,])
acc_e %>% summary
acc_e %>% residuals %>% plot(type="l")
acc_e$fitted.values %>% plot
acc_e$model[,1]
ggplot()+geom_line(aes(x = row.names(acc_e$model) %>% as.yearqtr, y = acc_e$model[,1]))+
  geom_line(aes(x = row.names(acc_e$model) %>% as.yearqtr, y =acc_e$fitted.values, color = "fitted"))
# mu lambda = 2.01016
# delta mu lambda = 0.05375
# lambda = 0.05361

# mu = 37.49599
# delta = 0.02673917

df_long$rts %>% autoplot()
df_long$CONI_Q_CHI


# неоклассическая модель
# I(t) ~ sum(p) (P*Y / C)(t-p) + K(t-1)
# C = J(t-1) * (r + delta  - (J(t)-J(t-1)/J(t-1))) издержки владения капиталом
# r = i - pi реальная ставка
# надо не cpi а дефлятор
# J - индекс цен на стройматериалы и работы CONI_Q_CHI
df_long$realrate = (1+df_long$gov/100)/
  (df_long$CPI_Q_CHI/100*
     lag(df_long$CPI_Q_CHI)/100*
     lag(df_long$CPI_Q_CHI,2)/100*
     lag(df_long$CPI_Q_CHI, 3)/100)*100 -100
df_long$cost = lag(df_long$CONI_Q_CHI, 4) *
  (df_long$realrate/100 + 0.05 + diff.xts(df_long$CONI_Q_CHI,4, log = TRUE))
df_long$pyc <- (df_long$CPI_Q_CHI*df_long$GDPEA_Q_DIRI/df_long$cost)


neocl_m <- lm(diff.xts(FC, 4, log = TRUE)~0+
                 diff.xts(GDPEA_Q_DIRI,log = TRUE, lag = 4)+
                 diff.xts(CPI_Q_CHI,log = TRUE, lag = 4)+
                 diff.xts(cost, lag = 4)+
                 lag(diff.xts(cost, lag = 4)),
               data = df_long)

neocl_m %>% summary
neocl_lag_m <- lm(diff.xts(FC, 4, log = TRUE)~0+
     diff.xts(GDPEA_Q_DIRI,log = TRUE, lag = 4)+
       lag(diff.xts(GDPEA_Q_DIRI,log = TRUE, lag = 4))+
     diff.xts(CPI_Q_CHI,log = TRUE, lag = 4)+
     lag(diff.xts(CPI_Q_CHI,log = TRUE, lag = 4))+
     diff.xts(cost, lag = 4)+
       lag(diff.xts(cost, lag = 4))
     ,
   data = df_long)
    

neocl_lag_m %>% summary 
ggplot()#+
  #geom_line(aes(x = row.names(neocl_m$model) %>% as.yearqtr, y = neocl_m$model[,1]))+
  # geom_line(aes(x = row.names(neocl_m$model) %>% as.yearqtr, y =neocl_m$fitted.values, color = "nc"))+
  # geom_line(aes(x = row.names(acc_e$model) %>% as.yearqtr, y =acc_e$fitted.values, color = "accelerator"))+
  # geom_point(aes(x = row.names(neocl_lag_m$model) %>% as.yearqtr, y =neocl_lag_m$fitted.values -
  #                 neocl_lag_m$model[,1]
  #               , color = "neoclassical lag"))
ggplot()+geom_point(aes(x=lag(neocl_lag_m$residuals),y = neocl_lag_m$residuals))
