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
out1 <- get.panel(df_tr, 40, 12, c("ridge"), niter = 100)
save(out1, file = "out1.RData")
out2 <- get.panel(df_tr, 40, 12, c(
  "elnet"), niter = 100)
save(out2, file = "out2.RData")
out3 <- get.panel(df_tr, 40, 12, c("lasso"), niter = 100)
save(out3, file = "out3.RData")


# lasso modifications -----
out4 <- get.panel(df_tr, 40, 12, c(
                                     "lasso_pc"), niter = 100)
save(out4, file = "out4.RData")


out5 <- get.panel(df_tr, 40, 12, c(
                                    "post_lasso"), niter = 100)
save(out5, file = "out5.RData")

out6 <- get.panel(df_tr, 40, 12, c(
                                    "lasso_adaptive"), niter = 100)

save(out6, file = "out6.RData")
### assamble -------
out7 <- get.panel(df_tr, 40, 12, c(
  "rf"), niter = 100)
save(out7, file = "out7.RData")
out8 <- get.panel(df_tr, 40, 12, c(
  "ss"), niter = 100)
save(out8, file = "out8.RData")
out9 <- get.panel(df_tr, 40, 12, c(
  "boosting"), niter = 100)
save(out9, file = "out9.RData")


# acc ----
out10 <- get.panel(df_tr, 40, 12, c(
  "acc"), niter = 100)
save(out10, file = "out10.RData")

out11 <- get.panel(df_tr, 40, 12, c(
  "VAR"), niter = 100)
save(out11, file = "out11.RData")


