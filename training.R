source("lib.R")
source("fun.R", encoding = "utf8")
# Autoregression -----
# Random Walk and AR(p)

load("tfdata.RData")
arlist <- map(c("rw","arp"),
              function(modeli){get.ar(df = df_tf,
                                      window = 120,
                                      horizon = 12,
                                      nlead = c(1:18),
                                      model = modeli)})
save(arlist, file = "arlist.RData")

# Panel regressions ----
load("tfdata.RData")

system.time({
  reglist <- map(c("ridge","elnet",
                   "lasso",
                   "lasso_pc",
                   "post_lasso",
                   "lasso_lag",
                   "lasso_pc_lag",
                   "lasso_adaptive"),
                   function(modeli){get.panel.r(df_tf,
                                                120,
                                                12,
                                                nlead = c(1:18),
                                                model = modeli, niter = 200)})
               })




  ## Regression with regularisation ----
  ## Ridge, LASSO, Post-LASSO, and Elastic Network

save(reglist, file = "reglist.RData")

# Principal Components Analysis ----
rm(list = ls())
source("fun.R")
load("tfdata.RData")
X <- model.matrix(UNEMPL_M_SH~0+., data = df_tf) %>% as.matrix()
# выделяем главные компоненты
pc_list <- prcomp(X, center = TRUE, scale. = TRUE)
df_pc <- merge.xts(df_tf$UNEMPL_M_SH,xts(pc_list$x, order.by = time(df_tf %>% na.omit)))

# то же самое для лагов
X_lag <- model.matrix(UNEMPL_M_SH~0+., data = df_tf_lag) %>% as.matrix()
# выделяем главные компоненты
pc_list_lag <- prcomp(X_lag, center = TRUE, scale. = TRUE)
df_pc_lag <- merge.xts(df_tf_lag$UNEMPL_M_SH,xts(pc_list_lag$x, order.by = time(df_tf_lag %>% na.omit)))

# теперь посчитаем все регресионные модели для pc
regpclist <- c(map(c("ridge_pc","elnet_pc", "lasso_pc", "post_lasso_pc"),
                     function(modeli){get.panel.r(df_pc,
                                                    120,
                                                    12,
                                                    nlead = c(1:24),
                                                    model = modeli)}),
                 map(c("lasso_pc_lag", "post_lasso_pc_lag"),
                     function(modeli){get.panel.r(df_pc_lag,
                                                    120,
                                                    12,
                                                    nlead = c(1:24),
                                                    model = modeli)}))
save(regpclist,file= "regpclist.RData")
rm(list = ls())


#### Random Forest ----
load("tfdata.RData")
rflist <- list(get.panel.r(df_tf,
                                                  120,
                                                  12,
                                                  nlead = c(1:24),
                                                  model = "rf"))#
               # get.panel.r(df_tf_lag,
               #               120,
               #               12,
               #               nlead = c(1:24),
               #               model = "rf_lag"),
               # get.panel.r(df_pc,
               #               120,
               #               12,
               #               nlead = c(1:24),
               #               model = "rf_pc"),
               # get.panel.r(df_pc_lag,
               #               120,
               #               12,
               #               nlead = c(1:24),
               #               model = "rf_pc_lag"))
save(rflist,file= "rflist.RData")



# Spike-and-slab ----
sslist <- list(get.panel.r(df_tf,window = 120, horizon = 12, nlead = c(1:18), model = "ss", niter = 100),
               get.panel.r(df_pc,window = 120, horizon = 12, nlead = c(1:18), model = "ss_pc", niter = 100))


save(sslist,file= "sslist.RData")
