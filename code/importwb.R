source("code/lib.R")
wbsources()[44,]
wbstats::wbsearch("GDP per capita")
c("NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD")
data1 <- wb(indicator = c("NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD"),
   startdate = 2012,
   enddate = 2017,
   freq = "Y")
data1.1 <- data1 %>%
  select("iso3c", "country", "indicatorID", "date", "value") %>%
  as.data.table() %>%
  dcast.data.table(iso3c + country + date~ indicatorID) %>%
  group_by(iso3c, country) %>%
  arrange(date) %>%
  summarise(g = prod(NY.GDP.PCAP.KD.ZG/100+1)^(1/8) - 1,
            G = log(NY.GDP.PCAP.KD[1]))
p1 <- ggplot(data = data1.1, aes(x = G, y = g)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("GDP per capita, 2012") +
  ylab(TeX("Growth, $2012 \U2013 2017$")) +
  geom_point()
model1 <- lm(formula = g~G, data = data1.1)
summary(model1)
model1$fitted.values


stargazer(model1, model1, title="Regression Results",
          dep.var.labels=c("Overall Rating","High Rating"),
          covariate.labels=c("Handling of Complaints"),
          omit.stat=c("LL","ser","f"),
          ci=TRUE,
          ci.level=0.95,
          single.row=TRUE)
p2 <- p1+geom_smooth(method = "lm")
p2
#data2 ----
ind <- c("NY.GDP.PCAP.KD.ZG",
         "NY.GDP.PCAP.KD",
         
  "NE.GDI.FPUB.ZS", #Gross public investment (% of GDP)
  "SL.TLF.1564.IN", #Labor force (15-24 years), total
  "SL.TLF.1524.IN",  # Labor force, total
  "SP.POP.TOTL", 
  "SP.DYN.TFRT.IN", # fertility
  "SH.DYN.MORT", # infant mortality
  "SP.DYN.CDRT.IN", # death rate
  "SP.DYN.CBRT.IN", # birth rate
  "SP.DYN.LE00.IN", # life expectancy at birth
  "GC.XPN.TOTL.GD.ZS", # total government expense (% of GDP)
  "GB.BAL.OVRL.GDP.ZS", # deficit or surplus
  "DT.DOD.DECT.CD", # external debt currenct us dollars
  "PV.EST", # political stability
  "VA.EST", # voice
  "NE.EXP.GNFS.ZS", #exp % of gdp
  "NE.IMP.GNFS.ZS", # imp
  "AG.SRF.TOTL.K2",
"FR.INR.RISK",
"PA.NUS.FCRF")



data2 <- wb(indicator = ind,
            startdate = 2012, enddate = 2017, freq = "Y") %>%
  dcast( iso3c+date ~ indicatorID, value.var = "value", fun.aggregate = max)

CM <- cor(data2.wide)
CM_old <- CM


data3 <-  data2 %>% 
  group_by(iso3c) %>%
  arrange(date) %>%
  summarise(g = prod(NY.GDP.PCAP.KD.ZG/100+1)^(1/6) - 1,
            G = log(NY.GDP.PCAP.KD[1]),
          Pop = log(SP.POP.TOTL[1]),        
          FX = PA.NUS.FCRF[1],
          Fert = SP.DYN.TFRT.IN[1],
          IMR = SH.DYN.MORT[1],
          DR = SP.DYN.CDRT.IN[1], # death rate
          BR = SP.DYN.CBRT.IN[1], # birth rate
          LE = SP.DYN.LE00.IN[1], # life expectancy at birth
          GExp = GC.XPN.TOTL.GD.ZS[1], # total government expense (% of GDP)
          Stab = PV.EST[1], # political stability
          Voice = VA.EST[1], # voice
          Ex = NE.EXP.GNFS.ZS[1], #exp % of gdp
          Im = NE.IMP.GNFS.ZS[1], # imp
          Area = AG.SRF.TOTL.K2[1]
          ) %>%
  na.omit()
for(i in 1:nrow(data3)) {
  for(j in 1:ncol(data3)){
    if(is.infinite(data3[i,j][[1]])){
      data3[i,j] <- NA
    }
  }
}
data3 <- na.omit(data3)
data3.x <- data3 %>% select(-c(g, iso3c)) %>% as.matrix()

data3.y <- data3 %>% pull(g) 

model2 <- cv.glmnet(x = data3.x, y= data3.y, standardize = F)

bestlam <- model2$lambda.min
lasso.coef  <- predict(model2, type = 'coefficients', s = 30)
lasso.coef
