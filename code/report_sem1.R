source("code/importbarrolee.R")
data1 <- BL %>% select(gdpsh5_2_80, grsh5_1_5) %>%
  mutate(G = log(gdpsh5_2_80),
         g = grsh5_1_5)
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





remove.sparsed.cols <- function(df, prop) {
  sparsed_cols <- c()
  for(i in 1:ncol(df)){
    if(sum(is.na(df[,i]))/nrow(df) >= prop){
      sparsed_cols <- c(sparsed_cols, i)
    }
  }
  if(is.null(sparsed_cols)){
    df
  } else{
    df[, - sparsed_cols]
  }
}

data2 <- BL %>% select(vars_BL %>%
                         filter(Type == 0 | (Period %in% c("80", "4"))) %>%
                         pull(Colname_wide)) %>%
  remove.sparsed.cols(0.3) %>%
  na.omit
rn <- rownames(data2)
#data2 <- data2[!is.na(data2$grsh_x),]
data2 %<>% mutate(G = gdpsh5_2_80,
                  Gl = log(gdpsh5_2_80),
                 g = grsh5_1_4) %>%
  select(-c(gdpsh5_2_80, grsh5_1_4,grsh4_1_4, gdpsh4_2_80, grwb_1_4, gdpwb_2_80))
data2.x <- data2 %>% select(-c(g)) %>% as.matrix()

data2.y <- data2 %>% pull(g) 

model2 <- cv.glmnet(x = data2.x, y= data2.y, standardize = TRUE)

bestlam <- model2$lambda.min
lasso.coef  <- predict(model2, type = 'coefficients', s = 0.001)
lasso.coef
