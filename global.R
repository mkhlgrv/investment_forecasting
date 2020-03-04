source('lib.R')
# out_hair <- 
#   fread('out_hair.csv', encoding = 'UTF-8', colClasses = c(date = 'Date',
#                                                                      startdt = 'Date',
#                                                                      enddt = 'Date',
#                                                                      forecastdate = 'Date'))
load('shinydata.RData')
print(out_hair)