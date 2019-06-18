### Long ----
#### Random Forest ----

load("rawdata.RData")
source("fun.R")
source("lib.R")


df_tr <-  
  df_long %>%
  as.list() %>% 
  map(function(x){
    x[,1] <- diff.xts(log(x[,1]), 4)
    x
  }) %>% do.call.pipe(merge.xts)


out9 <- get.panel(df_tr, 40, 12, c(
  "boosting"), niter = 100, df_true = df_long)
save(out9, file = "out9.RData")


# acc ----\
out10 <- get.panel(df_tr, 40, 12, c(
  "acc"), niter = 100, df_true = df_long)
save(out10, file = "out10.RData")

out11 <- get.panel(df_tr, 40, 12, c(
  "VAR"), niter = 100, df_true = df_long)
save(out11, file = "out11.RData")