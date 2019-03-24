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

# то же самое для лагов
X_lag <- model.matrix(UNEMPL_M_SH~0+., data = df_tf_lag) %>% as.matrix()
# выделяем главные компоненты
pc_list_lag <- prcomp(X_lag, center = TRUE, scale. = TRUE)
df_pc_lag <- merge.xts(df_tf_lag$UNEMPL_M_SH,xts(pc_list_lag$x, order.by = time(df_tf_lag %>% na.omit)))

# теперь посчитаем все регресионные модели для pc
regpclist <- c(map(c("ridge_pc","elnet_pc", "lasso_pc", "post_lasso_pc"),
                     function(modeli){get.regular.r(df_pc,
                                                    120,
                                                    12,
                                                    nlead = c(1:12),
                                                    model = modeli)}),
                 map(c("lasso_pc_lag", "post_lasso_pc_lag"),
                     function(modeli){get.regular.r(df_pc_lag,
                                                    120,
                                                    12,
                                                    nlead = c(1:12),
                                                    model = modeli)}))
