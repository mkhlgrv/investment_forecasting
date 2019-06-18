load("rawdata.RData")
source("fun.R")
source("lib.R")


out.rfss <- get.panel(df_long, 40, 12, c("rf", "ss"), niter = 100)

save(out.rfss, file = "out.rfss.RData")