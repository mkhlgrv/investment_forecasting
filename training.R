source("lib.R")
source("fun.R", encoding = "utf8")
# Autoregression -----
# Random Walk and AR(p)

load("tfdata.RData")
arlist <- map(c("rw","arp"),
              function(modeli){get.ar(df = df_tf,
                                      window = 120,
                                      horizon = 12,
                                      nlead = c(1:12),
                                      model = modeli)})
save(arlist, file = "arlist.RData")

# Panel regressions ----
load("tfdata.RData")

  ## Regression with regularisation ----
  ## Ridge, LASSO, Post-LASSO, and Elastic Network


system.time({
  regurarlist <- c(map(c("ridge","elnet", "lasso", "post_lasso"),
                       function(modeli){get.regular.r(df_tf,
                                                      120,
                                                      12,
                                                      nlead = c(1:12),
                                                      model = modeli)}),
                   map(c("lasso_lag", "post_lasso_lag"),
                       function(modeli){get.regular.r(df_tf_lag,
                                                      120,
                                                      12,
                                                      nlead = c(1:12),
                                                      model = modeli)}))
  
})
save(regurarlist, file = "regurarlist.RData")
