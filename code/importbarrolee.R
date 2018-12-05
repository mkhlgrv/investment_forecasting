 rm(list =ls())
suppressMessages(source("code/lib.R"))
readme_vars <- readLines("data/barrolee/readme.txt")[-c(1:716)]
contain_prn <- which(grepl(".prn",readme_vars
                        ))
countries <- readLines("data/barrolee/readme.txt")[c(565:702)] %>%
  gsub(pattern = " {2,}", replacement = "  ", x = .) %>%
  gsub(pattern = "^ ", replacement = "", x = .) %>%
  strsplit(split = "  ") %>%
  do.call(what = rbind, args = .) %>%
  as.data.frame() %>%
  set_names(c("SHCODE", "COUNTRY  NAME", "WBCTRY", "SMPL98", "SMPL97"))
get.vars.BL <- function(x, i) {
  if(!is.na(contain_prn[i+1])) {
    data.frame(File = word(readme_vars
                          [x]),
               Colname_BL = (c(readme_vars
                          [x:(contain_prn[i+1]-1)]) %>%
      paste(collapse = " ") %>%
       gsub(pattern = ".{1,}: {1,}", replacement = "", x = .) %>%
       gsub(pattern = " {2,}", replacement = " ", x = .) %>%
       gsub(pattern = "^ | $", replacement = "", x = .) %>%
      strsplit(split = " ") %>%
        unlist
      )
    )
  } else {
    data.frame(File = word(readme_vars
                          [x]),
               Colname_BL = (c(readme_vars
                          [x:length(readme_vars
                                                )]) %>%
                             paste(collapse = " ") %>%
                             gsub(pattern = ".{1,}: {1,}", replacement = "", x = .) %>%
                             gsub(pattern = " {2,}", replacement = " ", x = .) %>%
                             gsub(pattern = "^ | $", replacement = "", x = .) %>%
                             strsplit(split = " ") %>%
                             unlist
               )
    )
  }
}
vars_BL <- imap_dfr(contain_prn, get.vars.BL) %>%
  mutate(Colname_correct = Colname_BL)
vars_BL$Colname_correct[which(vars_BL$Colname_BL=="grsh56a")] <- "grsh57"
vars_BL$Colname_correct[which(vars_BL$Colname_BL=="bmp2ml")] <- "bmp2l"
vars_BL$Colname_correct[grep("bmp[0-9]{1}l", vars_BL$Colname_correct)] <- paste0("bmpl", c(1:6))





#type x
vars1 <- vars_BL$Colname_correct %>%
  grep(pattern ="[a-z][0-9]{1}$|grsh5|invsh5|invsh4|
                    grsh4|govsh4|govsh5|gvxdxe4|gvxdxe5", x = ., value = TRUE) %>% .[-grep(pattern = "durs1", x = .)]

# type xx
vars2 <- vars_BL$Colname_correct  %>%
  grep(pattern = "[a-z][0-9]{2}$|gdpsh5|gdpsh4|pop15|pop65|lifee0|pysh5|pcsh5|pish5|pgsh5", x = ., value = TRUE) %>%
.[-grep("grsh5|invsh5|invsh4|grsh4|govsh4|govsh5|gvxdxe4|gvxdxe5|smpl97", x = .)]
vars0 <- vars_BL$Colname_correct %>% .[-which(vars_BL$Colname_correct %in% c(vars1, vars2))] 

vars_BL <- vars_BL %>% mutate(Type = ifelse(Colname_correct %in% vars0,
                                 0,
                                 ifelse(Colname_correct %in% vars1,
                                        1,
                                        ifelse(Colname_correct %in% vars2,
                                               2,
                                               NA)))) %>%
  mutate(Colname_unique = ifelse(Type == 0,
                         Colname_correct,
                         ifelse(Type == 1,
                                substr(Colname_correct, 1, nchar(Colname_correct) - 1),
                         ifelse(Type == 2,
                                substr(Colname_correct, 1, nchar(Colname_correct) - 2),NA))),
         Period = ifelse(Type == 0,
                         "",
                         ifelse(Type == 1,
                                substring(Colname_correct, nchar(Colname_correct)),
                         ifelse(Type == 2,
                                substring(Colname_correct, nchar(Colname_correct) - 1),
                                NA))),
         Colname_wide = paste(Colname_unique, Type, Period, sep = "_"))



read.set.names.BL <- function(x){
  read.table(paste0("data/barrolee/",x$File[1])) %>%
    purrr::set_names(x$Colname_wide)
}

BL <- vars_BL %>%
  split(.$File) %>%
  map_dfc(read.set.names.BL) %>% 
  set_rownames(countries$WBCTRY)
colnames(BL)
# Пример
vars_65 <- vars_BL %>% filter(Type == 0 | (Type == 1&Period == 1) | (Type == 2 & Period == 65)) %>% pull(Colname_wide)
X <- BL %>% select(vars_65,-gdpsh4_2_65)
Y <- BL %>% pull(gdpsh4_2_65)
X <- X[!is.na(Y),]
Y <- Y[!is.na(Y)]

for(i in 1:ncol(X)){
  X[,i][is.na(X[,i])] <- mean(X[,i], na.rm = TRUE)
}
X <- as.matrix(X)
model1 <- glmnet(X,Y)
summary(model1)

