
source("code/importbarrolee.R")
data1 <- df %>% select(gdpsh480, grsh45) %>%
  mutate(G = log(gdpsh480),
         g = grsh45)
model1 <- lm(formula = g~G, data = data1)
p1 <- ggplot(data = data1, aes(x = G, y = g)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("GDP per capita, 1980 (log)") +
  ylab(TeX("Growth, $1980 \U2013 1984$")) +
  geom_point()
p1+geom_smooth(method = "lm")



summary(model1)

model1$fitted.values


stargazer(model1, model1, title="Regression Results",
          dep.var.labels=c("Overall Rating","High Rating"),
          covariate.labels=c("Handling of Complaints"),
          omit.stat=c("LL","ser","f"),
          ci=TRUE,
          ci.level=0.95,
          single.row=TRUE)
p2 <- p1+geom_smooth(method = "lm")

#data2
data2 <- BL_long %>%
  filter(Type != "con") %>%
  filter(Year == 1980) %>%
  as.data.table %>%
  dcast.data.table(WBCTRY~Indicator+Type,
                   value.var = "Value", fun.aggregate = max) %>% bind_cols(BL_long %>%
                                                                             filter(Type == "con") %>%
                                                                             as.data.table %>%
                                                                             dcast.data.table(WBCTRY~Indicator,
                                                                                              value.var = "Value",
                                                                                              fun.aggregate = max))
 
data2 <- na.omit(data2) 
#data2 <- data2[!is.na(data2$grsh_x),]
data2.x <- data2 %>% select(-c(grsh_x, WBCTRY, WBCTRY1)) %>% as.matrix()
for(i in 1:112){
  data2.x[,i] <- (data2.x[,i] - mean(data2.x[,i], na.rm = TRUE))/sd(data2.x[,i], na.rm = TRUE)
    
  }
data2.x <- data2.x[,-90]
data2.y <- data2 %>% pull(grsh_x) 

model2 <- cv.glmnet(x = data2.x, y= data2.y, standardize = FALSE)
model2
bestlam <- model2$lambda.min
lasso.coef  <- predict(model2, type = 'coefficients', s = bestlam)[1:30,]
lasso.coef
