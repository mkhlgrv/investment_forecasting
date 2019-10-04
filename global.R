source('lib.R')
choises_q <- format(seq.Date(from = as.Date("2012-01-01"),
                             to = as.Date("2019-01-01"),
                             by = "quarter") %>%
                      as.yearqtr,
                    format = "%Y Q%q")

load('shinydata.RData')
