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





### assamble -------
out7 <- get.panel(df_tr, 40, 12, c(
  "rf"), niter = 100, df_true = df_long)
save(out7, file = "out7.RData")
out8 <- get.panel(df_tr, 40, 12, c(
  "ss"), niter = 100, df_true = df_long)
save(out8, file = "out8.RData")