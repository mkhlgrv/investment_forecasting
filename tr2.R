### Long ----
#### Random Forest ----

load("rawdata.RData")
source("fun.R")
source("lib.R")


# for(i in 1:ncol(df_long)){
#   print(names(df_long[,i]))
#   df_long[,i] %>% tsdisplay() %>% print
#   readline()
# }

df_tr <-  
  df_long %>%
  as.list() %>% 
  map(function(x){
    x[,1] <- diff.xts(log(x[,1]), 4)
    x
  }) %>% do.call.pipe(merge.xts)


# regular ----
out1 <- get.panel(df_tr, 40, 12, c("ridge"), niter = 100, df_true = df_long)
save(out1, file = "out1.RData")
out2 <- get.panel(df_tr, 40, 12, c(
  "elnet"), niter = 100, df_true = df_long)
save(out2, file = "out2.RData")
out3 <- get.panel(df_tr, 40, 12, c("lasso"), niter = 100, df_true = df_long)
save(out3, file = "out3.RData")


# lasso modifications -----
out4 <- get.panel(df_tr, 40, 12, c(
  "lasso_pc"), niter = 100, df_true = df_long)
save(out4, file = "out4.RData")


out5 <- get.panel(df_tr, 40, 12, c(
  "post_lasso"), niter = 100, df_true = df_long)
save(out5, file = "out5.RData")

out6 <- get.panel(df_tr, 40, 12, c(
  "lasso_adaptive"), niter = 100, df_true = df_long)

save(out6, file = "out6.RData")
