source("lib.R")
source("fun.R", encoding = "utf8")
rm(list = ls())
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

Sys.time()
rflist <- list(get.panel.r(df_tf,
                           120,
                           12,
                           nlead = c(1:18),
                           model = "rf"))

Sys.time()
bstlist <- list(get.panel.r(df_tf,
                            120,
                            12,
                            nlead = c(1:18),
                            model = "boost", niter = 100))

Sys.time()
  reglist <- c(map(c("ridge",
                     "elnet",
                   "lasso",
                   "lasso_pc",
                   "post_lasso",
                   "lasso_adaptive"),
                   function(modeli){get.panel.r(df_tf,
                                                120,
                                                12,
                                                nlead = c(1:18),
                                                model = modeli, niter = 200)}),
               map(c(
                     "lasso_lag",
                     "lasso_pc_lag"),
                   function(modeli){get.panel.r(df_tf_lag,
                                                120,
                                                12,
                                                nlead = c(1:18),
                                                model = modeli, niter = 200)}))
  
  Sys.time()
save.image(file = "curdata.RData")



  ## Regression with regularisation ----
  ## Ridge, LASSO, Post-LASSO, and Elastic Network

save(reglist, file = "reglist.RData")





#### Random Forest ----
load("tfdata.RData")

rflist <- list(get.panel.r(df_tf,
                           120,
                           12,
                           nlead = c(1:18),
                           model = "rf"))
save(rflist,file= "rflist.RData")

# Spike-and-slab ----
sslist <- list(get.panel.r(df_tf,window = 120, horizon = 12, nlead = c(1:18), model = "ss", niter = 100))

save(sslist,file= "sslist.RData")

# boosting ----

bstlist <- list(get.panel.r(df_tf,
                           120,
                           12,
                           nlead = c(1:18),
                           model = "boost"))
save(bstlist,file= "bstlist.RData")

# важно помнить что результаты 3 и 2 называются одинаково!!!!