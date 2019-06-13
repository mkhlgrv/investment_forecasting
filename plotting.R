
rm(list = ls())
source("lib.R")
source("fun.R")


load("rawdata.RData")
load("out1.RData")
load("out2.RData")
load("out3.RData")
load("out4.RData")
load("out5.RData")
load("out6.RData")
load("out7.RData")
load("out8.RData")
load("out9.RData")
load("out10.RData")
load("out11.RData")
save.image(file = "out.RData")
load("out.RData")


# investment level
level_plot <-
  ggplot(df_long) +
  geom_line(aes(y = FC, x = time(df_long)))+
  labs(title = "",
       y = "Валовые инвестиции",
       x = "Дата") + theme_bw()
cairo_pdf("plot/level_plot.pdf", width = 10, height = 5)
print(level_plot)
dev.off()



des_plot <- na.omit(df_long)$FC %>% stl(4, robust = TRUE) %>% .$time.series %>% as.data.frame() %>%
  mutate(date = time(na.omit(df_long)),
         x = na.omit(df_long)$FC) %>%
  set_names(c("Сезонность", "Тренд", "Остаток", "Дата", "Исходный ряд")) %>%
  melt(id.vars = "Дата") %>%
  ggplot(aes(x = Дата, y = value))+
  geom_line()+
  facet_grid(rows = vars(variable), scales = "free_y")+
  labs(title = "", x = "")+
  theme_bw()
  

cairo_pdf("plot/des_plot.pdf", width = 8, height = 5)
print(des_plot)
dev.off()

##графики в уровнях
outall <- list(out1, out2,
     out3,
     out4, out5, out6,
     out7, out8, out9, out10,
     out11) %>% map(function(x){
       x %>% select(model,window,horizon,date,y.true,y.pred, y.true.ts, y.pred.ts) %>%
         unique
     }) %>% 
  do.call.pipe(rbind )

outall %<>% split(1:nrow(.)) %>%
  map(function(x){
    x$model <- as.character(x$model)
    x$model <- switch(x$model[1],
           acc = "Accelerator",
           lasso = "LASSO",
           "lasso_adaptive" = "Adaptive LASSO",
           "post_lasso" = "Post LASSO",
           lasso_pc = "LASSO+PC",
           elnet = "Elastic Net",
           ridge = "Ridge",
           rf = "Random Forest",
           boosting = "Boosting",
           "ss" = "Spike and Slab",
           VAR = "LASSO+VAR")
    x
  }) %>% do.call.pipe(rbind)


# bad_outall <- outall %>%
#   ggplot()+
#   geom_line(aes(x = date, y = y.true))+
#   geom_line(aes(x = date, y = y.pred, color = model))+
#   facet_wrap(vars(horizon), scales = "free")+
#   labs(title = "Прогнозы валовых инвестиций",
#        y = "Валовые инвестиции",
#        x = "Дата", subtitle = "Горизонт прогнозирования от 1 до 12 кварталов") +
#   theme_bw()+
#   guides(colour = guide_legend(title = "Модель"))
#   
# 
# cairo_pdf("plot/bad_outall.pdf", width = 10, height = 5)
# print(bad_outall)
# dev.off()

smooth1 <- outall %>%
  filter(model %in%c("Ridge", "Elastic Net", "LASSO", "Accelerator")) %>%
  group_by(model, window, horizon) %>%
  mutate(y.true = c(rep(NA,3), rollmean(y.true, 4)), 
         y.pred = c(rep(NA,3),rollmean(y.pred, 4))) %>%
  ggplot()+
  geom_line(aes(x = date, y = y.true))+
  geom_line(aes(x = date, y = y.pred, color = model))+
  facet_wrap(vars(horizon), scales = "free")+
  labs(title = "",
       y = "Валовые инвестиции",
       x = "Дата", subtitle = "Горизонт прогнозирования от 1 до 12 кварталов") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/smooth1.pdf", width = 10, height = 5)
print(smooth1)
dev.off()


smooth2 <- outall %>%
  filter(model %in%c("LASSO","LASSO+PC", "Post LASSO", "Adaptive LASSO", "Accelerator")) %>%
  group_by(model, window, horizon) %>%
  mutate(y.true = c(rep(NA,3), rollmean(y.true, 4)), 
         y.pred = c(rep(NA,3),rollmean(y.pred, 4))) %>%
  ggplot()+
  geom_line(aes(x = date, y = y.true))+
  geom_line(aes(x = date, y = y.pred, color = model))+
  facet_wrap(vars(horizon), scales = "free")+
  labs(title = "",
       y = "Валовые инвестиции",
       x = "Дата", subtitle = "Горизонт прогнозирования от 1 до 12 кварталов") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/smooth2.pdf", width = 10, height = 5)
print(smooth2)
dev.off()

smooth3 <- outall %>%
  filter(model %in%c("Random Forest","Spike and Slab", "Boosting", "Accelerator")) %>%
  group_by(model, window, horizon) %>%
  mutate(y.true = c(rep(NA,3), rollmean(y.true, 4)), 
         y.pred = c(rep(NA,3),rollmean(y.pred, 4))) %>%
  ggplot()+
  geom_line(aes(x = date, y = y.true))+
  geom_line(aes(x = date, y = y.pred, color = model))+
  facet_wrap(vars(horizon), scales = "free")+
  labs(title = "",
       y = "Валовые инвестиции",
       x = "Дата", subtitle = "Горизонт прогнозирования от 1 до 12 кварталов") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/smooth3.pdf", width = 10, height = 5)
print(smooth3)
dev.off()

smooth4 <- outall %>%
  filter(model %in%c("Accelerator", "LASSO+VAR")) %>%
  group_by(model, window, horizon) %>%
  mutate(y.true = c(rep(NA,3), rollmean(y.true, 4)), 
         y.pred = c(rep(NA,3),rollmean(y.pred, 4))) %>%
  ggplot()+
  geom_line(aes(x = date, y = y.true))+
  geom_line(aes(x = date, y = y.pred, color = model))+
  facet_wrap(vars(horizon), scales = "free")+
  labs(title = "",
       y = "Валовые инвестиции",
       x = "Дата", subtitle = "Горизонт прогнозирования от 1 до 12 кварталов") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/smooth4.pdf", width = 10, height = 5)
print(smooth4)
dev.off()

benchmark.score <- outall %>%
  filter(model == "Accelerator") %>%
  get.score()
  
rmse1 <- outall %>%
  filter(model %in% c("Ridge", "Elastic Net", "LASSO")) %>%
   get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$rmse <-x$rmse/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(rmse)
    x
  }) %>%
  ggplot()+geom_line(aes(x=horizon, y = rmse, color = model))+
  geom_point(aes(x=horizon, y = rmse, color = model)) +
  labs(title = "",
       y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
cairo_pdf("plot/rmse1.pdf", width = 10, height = 5)
print(rmse1)
dev.off()

rmse2 <- outall %>%
  filter(model %in% c("LASSO","LASSO+PC", "Post LASSO", "Adaptive LASSO")) %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$rmse <-x$rmse/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(rmse)
    x
  }) %>% ggplot()+geom_line(aes(x=horizon, y = rmse, color = model))+
  geom_point(aes(x=horizon, y = rmse, color = model))+
  labs(title = "",
       y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
cairo_pdf("plot/rmse2.pdf", width = 10, height = 5)
print(rmse2)
dev.off()

rmse3 <- outall %>%
  filter(model %in% c("Random Forest","Spike and Slab", "Boosting","LASSO+VAR")) %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$rmse <-x$rmse/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(rmse)
    x
  }) %>% ggplot()+geom_line(aes(x=horizon, y = rmse, color = model))+
  geom_point(aes(x=horizon, y = rmse, color = model))+
  labs(title = "",
       y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
cairo_pdf("plot/rmse3.pdf", width = 10, height = 5)
print(rmse3)
dev.off()


rmse4 <- outall %>%
  filter(model %in% c("Accelerator")) %>%
  get.score() %>%
  ggplot()+geom_line(aes(x=horizon, y = rmse, color = model))+
  geom_point(aes(x=horizon, y = rmse, color = model))+
  labs(title = "",
       y = "RMSE",
       x = "Горизонт прогноза") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))
cairo_pdf("plot/rmse4.pdf", width = 10, height = 5)
print(rmse4)
dev.off()




## nonzero coefs ----
# reglist[c(1,4,5)]

nonzerotime <- do.call(rbind, list(out2, out3, out4, out5, out6)) %>%
  split(1:nrow(.)) %>%
  map(function(x){
    x$model <- as.character(x$model)
    x$model <- switch(x$model[1],
                      acc = "Accelerator",
                      lasso = "LASSO",
                      "lasso_adaptive" = "Adaptive LASSO",
                      "post_lasso" = "Post LASSO",
                      lasso_pc = "LASSO+PC",
                      elnet = "Elastic Net",
                      ridge = "Ridge",
                      rf = "Random Forest",
                      boosting = "Boosting",
                      "ss" = "Spike and Slab",
                      VAR = "LASSO+VAR")
    x
  }) %>%
  
  do.call.pipe(rbind) %>%
  filter(horizon == 1) %>%
  group_by(model, horizon, date) %>%
  summarise(nonzero = mean(nonzero)) %>% 
  ungroup() %>%
  ggplot()+
  geom_line(aes(x = date, y = nonzero, colour = model), size = 0.8)+
  labs(title = "",
       y = "Количество ненулевых коэффициентов",
       x = "Дата") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/nonzerotime.pdf", width = 10, height = 5)
print(nonzerotime)
dev.off()


nonzeroerror <-
  do.call(rbind, list(out2, out3, out4, out5, out6)) %>%
  split(1:nrow(.)) %>%
  map(function(x){
    x$model <- as.character(x$model)
    x$model <- switch(x$model[1],
                      acc = "Accelerator",
                      lasso = "LASSO",
                      "lasso_adaptive" = "Adaptive LASSO",
                      "post_lasso" = "Post LASSO",
                      lasso_pc = "LASSO+PC",
                      elnet = "Elastic Net",
                      ridge = "Ridge",
                      rf = "Random Forest",
                      boosting = "Boosting",
                      "ss" = "Spike and Slab",
                      VAR = "LASSO+VAR")
    x
  }) %>%
  do.call.pipe(rbind) %>%
  mutate(error = (y.true - y.pred)^2) %>%
  group_by(model, horizon, date) %>%
  summarise(nonzero = mean(nonzero),
            error = mean(error)) %>%
    ungroup() %>%
    mutate(horizon = as.numeric(horizon)) %>%
  ggplot()+
  geom_point(aes(y = error, x = nonzero, colour = model), size = 1.1, alpha = 1)+
    geom_smooth(aes(y = error, x = nonzero,colour = model), se = FALSE, method = "lm")+
  facet_wrap(vars(horizon), scales = "free")+
  labs(title = "",x = "Количество ненулевых коэффициентов",
       y = "Квадрат ошибки (логарифмическая шкала)") +
  theme_bw()+
  scale_y_continuous(trans='log10')+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/nonzeroerror.pdf", width = 14, height = 7)
print(nonzeroerror)
dev.off()



nzmat1 <- do.call(rbind, list( #out3)) %>% #, out4, out5, 
                             out6)) %>%
  select(date, model, horizon, nzvars) %>%
  arrange(date) %>%
  unique

for(j in 2:ncol(df_long)){
  nzmat1[j+3] <- 0
  for(i in 1:nrow(nzmat1)){
    if(grepl(paste0(" ",j-1, " "),nzmat1$nzvars[i])){
      nzmat1[i,j+3] <- 1
    } 
  }
}

colnames(nzmat1)[-c(1:4)] <- colnames(df_long)[-1]
nzlollipop1 <- nzmat1 %>%
  group_by(model, horizon) %>%
  select(-nzvars) %>% melt(id.vars = c("model", "horizon", "date"), ) %>%
  group_by(model, horizon, variable) %>%
  summarise(nonzero = sum(value)) %>%
  ungroup %>%
  mutate(nonzero = nonzero/max(nonzero)) %>% 
  inner_join(series_info %>% select(tsname, fullname), by = c("variable" ="tsname")) %>%
  select(-model) %>%
  filter(horizon == 1) %>%
  arrange(nonzero) %>%
  mutate(fullname = factor(fullname, fullname)) %>%
  ggplot() +
  geom_segment(aes(x = 0, y = fullname, xend = nonzero, yend = fullname), color = "red")+
  labs(title = "",x = "Доля включений",
       y = "") +
  theme_bw()
  

cairo_pdf("plot/nzlollipop1.pdf", width = 12, height = 7)
print(nzlollipop1)
dev.off()

nzmat2 <- do.call(rbind, list( out3)) %>% #, out4, out5, 
  #out6)) %>%
  select(date, model, horizon, nzvars) %>%
  arrange(date) %>%
  unique

for(j in 2:ncol(df_long)){
  nzmat2[j+3] <- 0
  for(i in 1:nrow(nzmat2)){
    if(grepl(paste0(" ",j-1, " "),nzmat2$nzvars[i])){
      nzmat2[i,j+3] <- 1
    } 
  }
}

colnames(nzmat2)[-c(1:4)] <- colnames(df_long)[-1]
nzlollipop2 <- nzmat2 %>%
  group_by(model, horizon) %>%
  select(-nzvars) %>% melt(id.vars = c("model", "horizon", "date"), ) %>%
  group_by(model, horizon, variable) %>%
  summarise(nonzero = sum(value)) %>%
  ungroup %>%
  mutate(nonzero = nonzero/max(nonzero)) %>% 
  inner_join(series_info %>% select(tsname, fullname), by = c("variable" ="tsname")) %>%
  select(-model) %>%
  filter(horizon == 1) %>%
  arrange(nonzero) %>%
  mutate(fullname = factor(fullname, fullname)) %>%
  ggplot() +
  geom_segment(aes(x = 0, y = fullname, xend = nonzero, yend = fullname), color = "red")+
  labs(title = "",x = "Доля включений",
       y = "") +
  theme_bw()


cairo_pdf("plot/nzlollipop2.pdf", width = 12, height = 7)
print(nzlollipop2)
dev.off()


#### таблица -----

tab1 <- outall %>%
  filter(model!="Accelerator") %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$rmse <-x$rmse/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(rmse)
    x
  }) %>% 
  select(model, horizon, rmse) %>%
  rename(Модель = model ) %>%
  filter(horizon<=6) %>%
  dcast(Модель~horizon) %>%
  xtable%>%
    print(include.rownames = FALSE)

tab2 <- outall %>%
  filter(model!="Accelerator") %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$rmse <-x$rmse/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(rmse)
    x
  }) %>% 
  select(model, horizon, rmse) %>%
  rename(Модель = model ) %>%
  filter(horizon>6) %>%
  dcast(Модель~horizon) %>%
  xtable %>%
  print(include.rownames = FALSE)



error1 <- outall %>%
  filter(model!="Accelerator")%>%
  filter(model %in% c("LASSO", "Ridge", "Elastic Net")) %>%
  mutate(error = y.true - y.pred) %>%
  ggplot() +
  geom_density(aes(x = error, color = model)) +
  facet_wrap(vars(horizon)) + 
  labs(title = "",x = "Ошибка",
       y = "") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/error1.pdf", width = 12, height = 7)
print(error1)
dev.off()


error2 <- outall %>%
  filter(model!="Accelerator")%>%
  filter(model %in% c("LASSO","LASSO+PC", "Post LASSO", "Adaptive LASSO","LASSO+VAR")) %>%
  mutate(error = y.true - y.pred) %>%
  ggplot() +
  geom_density(aes(x = error, color = model)) +
  facet_wrap(vars(horizon)) + 
  labs(title = "",x = "Ошибка",
       y = "") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/error2.pdf", width = 12, height = 7)
print(error2)
dev.off()

error3 <- outall %>%
  filter(model!="Accelerator")%>%
  filter(model %in% c("Random Forest","Spike and Slab", "Boosting")) %>%
  mutate(error = y.true - y.pred) %>%
  ggplot() +
  geom_density(aes(x = error, color = model)) +
  facet_wrap(vars(horizon)) + 
  labs(title = "",x = "Ошибка",
       y = "") +
  theme_bw()+
  guides(colour = guide_legend(title = "Модель"))

cairo_pdf("plot/error3.pdf", width = 12, height = 7)
print(error3)
dev.off()

ertab1 <- check.normal(outall %>% filter(model != "Accelerator")) %>%
  select(model, horizon, pv) %>%
  rename(Модель = model ) %>%
  filter(horizon<=6) %>%
  dcast(Модель~horizon) %>%
  xtable%>%
  print(include.rownames = FALSE)

ertab2 <- check.normal(outall %>% filter(model != "Accelerator")) %>%
  select(model, horizon, pv) %>%
  rename(Модель = model ) %>%
  filter(horizon>=7) %>%
  dcast(Модель~horizon) %>%
  xtable%>%
  print(include.rownames = FALSE)


df_long %>%
  as.data.frame() %>%
  melt %>%
inner_join(series_info %>% select(tsname, fullname), by = c("variable" ="tsname")) %>%
  select(fullname) %>% unique %>%
  xtable%>%
  print(include.rownames = FALSE)
  



tab_app1 <- outall %>%
  filter(model!="Accelerator") %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$mae <-x$mae/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(mae)
    x
  }) %>%
  select(model, horizon, mae) %>%
  rename(Модель = model ) %>%
  filter(horizon<=6) %>%
  dcast(Модель~horizon) %>%
  xtable%>%
  print(include.rownames = FALSE)

tab_app2 <- outall %>%
  filter(model!="Accelerator") %>%
  get.score() %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x){
    x$mae <-x$mae/ benchmark.score %>% filter(horizon == x$horizon) %>% pull(mae)
    x
  }) %>%
  select(model, horizon, mae) %>%
  rename(Модель = model ) %>%
  filter(horizon>6) %>%
  dcast(Модель~horizon) %>%
  xtable%>%
  print(include.rownames = FALSE)



  