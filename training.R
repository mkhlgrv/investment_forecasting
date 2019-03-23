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

# Principal Components Analysis ----
rm(list = ls())
load("tfdata.RData")
X <- model.matrix(UNEMPL_M_SH~0+., data = df_tf) %>% as.matrix()
# выделяем главные компоненты
pc_list <- prcomp(X, center = TRUE, scale. = TRUE)
df_pc <- merge.xts(df_tf$UNEMPL_M_SH,xts(pc_list$x, order.by = time(df_tf %>% na.omit)))
# теперь посчитаем все регресионные модели для pc
pc.ridge.out <- get.regular.r(df=df_pc, window = 120, horizon = 10, nlead = c(1:12),model = "pc_ridge")

get.sc