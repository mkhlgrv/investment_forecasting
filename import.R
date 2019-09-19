# Исследование 
source("lib.R")
# 1. нужен набор данных для 1995 - 2019 и для 1999 - 2019

# доходность ртс, процентные ставки на межбанковском рынке, спреды по облигациям,
# темпы роста номинального эффективного курса, реального эффективного курса, прирост цен на нефть,
# 4 разность логарифма ВВП и ИПЦ, отношение номинальных инвестиций к номинальному ввп + лаги

# загрузка данных sophisthse ----
sophist_series <- c('UNEMPL_Q_SH',# безработица
                    'EMPLDEC_Q', # заявленная потребность в работниках
                    'CONSTR_Q_NAT', # индекс строительно-монтажных работ
                    'TRP_Q_PASS_DIRI', # индекс пассажирских перевозок
                    'WAG_Q', # зарплата
                    'CPI_Q_CHI',# ипц
                    'CONI_Q_CHI', # индекс цен на строительно-монтажные работы
                    'CTI_Q_CHI', # индекс тарифов на грузовые перевозки
                    'AGR_Q_DIRI', # индекс сельхоз производства
                    'CNSTR_Q_DIRI',# индекс работ в строительстве
                    'RTRD_Q_DIRI', # оборот розничной торговли
                    'HHI_Q_DIRI',# индекс реальных денежных доходов населения
                    'M0_Q', # M0
                    'M2_Q',# М2
                    'IR_Q',# прямые иностранные инвестиции
                    'ICR_Q',# валютные резервы ЦБР
                    'CBREV_Q',# доходы конс. бюджета 
                    'CBEX_Q',# расходы конс. бюджета
                    'FBREV_Q',# доходы фед. бюджета
                    'FBEX_Q',# расходы фед. бюджета
                    'RDEXRO_Q',# официальный курс доллара
                    'RDEXRM_Q',# курс доллара на ммвб
                    'RDEXRMA_Q',# средний за период курс доллара на ммвб
                    'LIAB_T_Q',# кредиторская задолженность в среднем за период
                    'LIAB_UNP_Q',# просроченная кредиторская задолженность в среднем за период
                    'LIAB_S_Q',# кредиторская задолженность поставщикам в среднем за период
                    'LIAB_B_Q',# кредиторская задолженность в бюджет в среднем за период
                    'DBT_T_Q',#дебиторская задолженность в среднем за период
                    'DBT_UNP_Q',#просроченная дебиторская задолженность в среднем за период
                    'DBT_P_Q',# дебиторская задолженность покупателей в среднем за период
                    'EX_T_Q',# экспорт
                    'EX_NON.CIS_Q',# экспорт в не-снг страны
                    'IM_T_Q',# импорт
                    'IM_NON.CIS_Q', # импорт не из стран снг
                    'INVFC_Q', # номинальные инвестиции
                    # методика расчета ввп поменялась в 2004, поэтому два разных ряда не соответствуют друг другу
                    'GDPEA_Q_DIRI',# индексы реального ввп (после 2004)
                    'GDP_Q_DIRI',# (до 2004)
                    'GDPEA_C_Q',# номинальный ввп (после 2004)
                    'GDP_Q_C',# (до 2004)
                    # методика расчета индекса цен производителей промтоваров тоже поменялась
                    'PPI_Q_CHI', # индекс цен производителей промтоваров (до 2004)
                    'PPI_EA_Q' # (после 2004-01)
)
sophistdata <- sophisthse(series.name = sophist_series,
                          output = 'zoo') %>% as.xts
sophistdata <- sophistdata[, which(colnames(sophistdata) %in% sophist_series)]
# заменяем значения в колонках для индекса реального ввп и для номинального ввп
sophistdata$GDPEA_Q_DIRI[which(is.na(sophistdata$GDPEA_Q_DIRI))] <-
  sophistdata$GDP_Q_DIRI[which(is.na(sophistdata$GDPEA_Q_DIRI))]/1.1515
sophistdata$GDPEA_C_Q[which(is.na(sophistdata$GDPEA_C_Q))] <-
  sophistdata$GDP_Q_C[which(is.na(sophistdata$GDPEA_C_Q))]
# удаляем лишние столбцы с ввп
sophistdata$GDP_Q_C <- sophistdata$GDP_Q_DIRI <- NULL

# заменяем значения для индекса цен производителей промтоваров
sophistdata$PPI_EA_Q[which(is.na(sophistdata$PPI_EA_Q))] <-
  sophistdata$PPI_Q_CHI[which(is.na(sophistdata$PPI_EA_Q))]
# удаляем лишний столбец (данные до 2004)
sophistdata$PPI_Q_CHI <- NULL


# загрузка индекса реальных инвестиций ----


# первая часть данных -- с 1995-01 по 2008-12
url <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab29arh.xls'

GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

inv1 <- read_excel(tf, 1L,skip=14, col_names = FALSE) %>%
  .[1,-1] %>%
  t %>%
  as.numeric %>%
  xts(x = .,
      order.by = seq(as.Date('1995-01-01'),
                     by = 'quarter',
                     length.out = length(.)) %>% as.yearqtr) %>%
  set_colnames('investment')

# вторая часть данных -- с 2003-01 по 2011-12
# внутри есть примечания вида 2)
# их надо удалить
url <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab29.xls'

GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

inv2 <- read_excel(tf, 1L,skip=13, col_names = FALSE) %>%
  .[1,-1] %>%
  t %>%
  gsub(pattern = "2)",
       replacement = "",
       x = .) %>%
  gsub(pattern = ",",
       replacement = ".",
       x = .) %>%
  as.numeric %>%
  xts(x = .,
      order.by = seq(as.Date('2003-01-01'),
                     by = 'quarter',
                     length.out = length(.)) %>% as.yearqtr) %>%
  set_colnames('investment')

# 3 часть данных -- с 2011-01 по 2016-12
url <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab29a.xls'

GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

inv3 <- read_excel(tf, 1L,skip=12, col_names = FALSE) %>%
  .[1,-1] %>%
  t %>%
  as.numeric %>%
  xts(x = .,
      order.by = seq(as.Date('2011-01-01'),
                     by = 'quarter',
                     length.out = length(.)) %>% as.yearqtr) %>%
  set_colnames('investment')

# 4 часть данных -- с 2014-01

url <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab29b.xls'

GET(url, write_disk(tf <- tempfile(fileext = ".xls")))

inv4 <- read_excel(tf, 1L,skip=12, col_names = FALSE) %>%
  .[1,-1] %>%
  t %>%
  as.numeric %>%
  xts(x = .,
      order.by = seq(as.Date('2014-01-01'),
                     by = 'quarter',
                     length.out = length(.)) %>% as.yearqtr) %>%
  set_colnames('investment')

# привести в соответствие со последующим индексом
inv3[,1] <- (inv3[,1])*(inv4['2014-01',1] %>% as.numeric)/(inv3['2014-01',1] %>% as.numeric)
inv2[,1] <- (inv2[,1])*(inv3['2011-01',1] %>% as.numeric)/(inv2['2011-01',1] %>% as.numeric)
inv1[,1] <- (inv1[,1])*(inv2['2003-01',1] %>% as.numeric)/(inv1['2003-01',1] %>% as.numeric)

inv <- rbind(inv1['1995-01/2002-12'], inv2['2003-01/2010-12'],inv3['2011-01/2013-12'],
             inv4['2014-01/2020-1'])

inv <- inv*100/(inv %>% first %>% as.numeric)

# загрузка квартального дефлятора ----
# росстат представляет данные в % по отношению к соответствующему кварталу прошлого года
# первая часть данных -- с 1996-01 по 2011-12
url1 <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab9.xls'

GET(url1, write_disk(tf1 <- tempfile(fileext = ".xls")))

def1 <- read_excel(tf1, 1L, skip = 5, col_names = FALSE) %>%
  .[1,] %>%
  t %>%
  as.numeric

# вторая часть данных -- с 2012-01 по последний день

url2 <- 'http://www.gks.ru/free_doc/new_site/vvp/kv/tab9a.xls'

GET(url2, write_disk(tf2 <- tempfile(fileext = ".xls")))

def2 <- read_excel(tf2, 1L, skip = 5, col_names = FALSE)%>%
  .[1,] %>%
  t %>%
  as.numeric

# склеиваем дефлятор
def <- xts(c(def1, def2),
    order.by = seq(as.Date('1996-01-01'),
                   by = 'quarter',
                   length.out = length(c(def1, def2))) %>% as.yearqtr) %>%
  set_colnames("deflator")

# Загрузка ставок мбр ----

# Показатели ставок межбанковского рынка (статистика ЦБ РФ)

# 03.01.1996 - 01.08.2000:

# http://www.cbr.ru/hd_base/mkr/mkr_base_old/

# уточнение: после загрузки в csv файлах ','->'.' and ';'->',' and '-'>''



# Фактические ставки по предоставленным кредитам 
# (MIACR - Moscow InterBank Actual Credit Rate)
# (в процентах годовых для рублевых кредитов)
# -Средневзвешанные фактические 

mkr1 <- import('data/mkr_old.csv') %>%
  set_colnames(c("date", '1d', '3d', '7d','14d', '21d', '30d', '60d', '90d')) %>%
  mutate(date=as.Date(date,format = '%d.%m.%Y')) %>%
  xts(x = .[,-1],order.by =  .[,1]) %>%
  # в данных за 1996--2000 есть повторы
  .[!duplicated(time(.))]

# без серьезных пропусков есть только данные:
# 1d, 7d, 14d, 30d

# c  01.08.2000

# https://www.cbr.ru/hd_base/mkr/mkr_base/

# Средневзвешенные фактические ставки по кредитам,
# предоставленным московскими банками (MIACR) с 01.08.2000

mkr2 <- import('data/mkr.csv') %>%
  set_colnames(c("date", '1d', '2-7d', '8-30d','31-90d', '91-180d', '181-365d')) %>%
  mutate(date=as.Date(date,format = '%d.%m.%Y')) %>%
  xts(x = .[,-1],order.by =  .[,1])


# без серьезных пропусков есть только данные:
# 1d, 2-7d,
# от 8 до 365 данные обновляются очень редко (или почти никогда)

# совмещать имеет смысл только по 1d -- 1d и 7d -- 2-7d
mkr <- merge(mkr1[,c('1d', '7d')],mkr2[,c('1d', '2-7d')]) %>%
  set_colnames(c('1d', '7d', '1d_old', '2-7_old'))
# склейка
mkr$`1d`[which(is.na(mkr$`1d`))] <- mkr$`1d_old`[which(is.na(mkr$`1d`))]
mkr$`7d`[which(is.na(mkr$`7d`))] <- mkr$`2-7_old`[which(is.na(mkr$`7d`))]
# удаление лишних колонок
mkr$`1d_old` <- mkr$`2-7_old` <- NULL

# необходимо перевести дневные данные в квартальные
# первый вариант - средние за квартал (реализован)
# второй вариант - на последнюю дату квартала

mkr %<>%
  as.data.frame %>%
  rownames_to_column('date') %>%
  group_by(date %>% as.Date %>% as.yearqtr) %>%
  summarise(mkr_1d = mean(`1d`, na.rm=TRUE),
            mkr_7d = mean(`7d`, na.rm=TRUE)) %>%
  as.data.frame %>%
  xts(x=.[,-1],order.by=.[,1])

# загрузка данных по гособлигациям ----
# данные не по всем облигациям есть с 1995
# зато для того времени есть данные по ГКО (месячные)

gko <- sophisthse('GKO_M', 'zoo')[,'GKO_M'] %>%
  as.xts %>%
  na.locf %>%
  set_colnames('GKO')

gko %<>% .[mod(month(time(.)), 3)==0,]

time(gko) <- as.yearqtr(time(gko))

# с 2004 года вместо ГКО в ряду используется доходность ОБР (можно удалить)
gko['2004-01/2019-12'] <- NA

# данные по доходности 6-месячных облигаций есть с 1995-03
# https://ru.investing.com/rates-bonds/russia-6-month-bond-yield-historical-data
# (скачать недельные (из-за дат). Порядок изменения файлов csv (через блокнот):
# 1. "," -> ;
# 2. , -> .
# 3. ; -> ,
# 4. " -> 
# 5. % -> 

gov_6m <- import('data/gov_6m.csv') %>%
  set_colnames(c("date", 'close', 'open', 'max','min', 'change')) %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  as.data.frame %>%
  group_by(as.yearqtr(date)) %>%
  filter(row_number() == min(row_number())) %>%
  as.data.frame %>%
  xts(x=.[,2],order.by=.[,1] %>% as.yearqtr) %>%
  set_colnames('gov_6m')


# данные по доходности годовых облигаций --- с 2001-08
# https://ru.investing.com/rates-bonds/russia-1-year-bond-yield-historical-data
# изменить файл аналогично предыдущей инструкции
# 1. "," -> ;
# 2. , -> .
# 3. ; -> ,
# 4. " -> 
# 5. % -> 
gov_1y <- import('data/gov_1y.csv') %>%
  set_colnames(c("date", 'close', 'open', 'max','min', 'change')) %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  as.data.frame %>%
  group_by(as.yearqtr(date)) %>%
  filter(row_number() == min(row_number())) %>%
  as.data.frame %>%
  xts(x=.[,2],order.by=.[,1] %>% as.yearqtr) %>%
  set_colnames('gov_1y')



# 3-летних --- с 2001-03
# https://ru.investing.com/rates-bonds/russia-3-year-bond-yield-historical-data
# изменить файл аналогично предыдущей инструкции
# 1. "," -> ;
# 2. , -> .
# 3. ; -> ,
# 4. " -> 
# 5. % -> 

gov_3y <- import('data/gov_3y.csv') %>%
  set_colnames(c("date", 'close', 'open', 'max','min', 'change')) %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  as.data.frame %>%
  group_by(as.yearqtr(date)) %>%
  filter(row_number() == min(row_number())) %>%
  as.data.frame %>%
  xts(x=.[,2],order.by=.[,1] %>% as.yearqtr) %>%
  set_colnames('gov_3y')
gov <- merge.xts(gov_6m, gov_1y, gov_3y)


# загрузка данных из bloomberg (цена нефти, эффективный обменный курс и индекс RTS) ----
# Квартальные данные
# файл bloomberg.csv 

# Russia Real Effective Exchange Rate Broad (BISBUR)
# Russia Nominal Effecive Exchange Rate (BISBRUN)
# CO1 Comdty (Oil Brent) last price
# RTS last price

# сохранить excel как csv
# действия:
# 1. , -> .
# 2. ; -> ,
bloomberg <- import('data/bloomberg.csv') %>%
  set_colnames(c("date", 'reer', 'neer', 'oil','rts')) %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y") %>% as.yearqtr) %>%
  xts(x=.[,-1],order.by=.[,1])


# конечная склейка данных ----

rawdata <- merge.xts(inv,sophistdata, def, mkr, gov,gko, bloomberg)
save(rawdata, file = "data/raw.RData")
rio::export(rawdata, 'data/raw.csv', 'csv', row.names = TRUE)
rm(list=ls())
