wbstats::wbcountries() %>% head
wbstats::wbindicators() %>% head()
wbindicators() %>% head
wbsources()[44,]
wbstats::wbsearch("GDP per capita")
c("NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD")
data1 <- wb(indicator = c("NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD"),
   startdate = 2010,
   enddate = 2017,
   freq = "Y")
data1.1 <- data1 %>%
  select("iso3c", "country", "indicatorID", "date", "value") %>%
  as.data.table() %>%
  dcast.data.table(iso3c + country + date~ indicatorID) %>%
  group_by(iso3c, country) %>%
  arrange(date) %>%
  summarise(growth = prod(NY.GDP.PCAP.KD.ZG/100+1)^(1/8) - 1,
            gdp = NY.GDP.PCAP.KD[1])
qplot(gdp, growth, data = data1.1)
lm(formula = growth~gdp, data = data1.1)
