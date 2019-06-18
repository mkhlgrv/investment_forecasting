load("rawdata.RData")
source("fun.R")
source("lib.R")

out <-  map(c("ss", "boost"),
            function(modeli){get.panel.r(df_long,
                                         40,
                                         2,
                                         nlead = c(1:12),
                                         model = modeli, niter = 200)})

save(out, file = "out3.RData")