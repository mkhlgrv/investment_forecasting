rm(list =ls())
source("code/lib.R")
readme <- readLines("data/barrolee/readme.txt")[-c(1:716)]
firstlines <- which(grepl(".prn",readme))
countries <- readLines("data/barrolee/readme.txt")[c(565:702)] %>%
  gsub(pattern = " {2,}", replacement = "  ", x = .) %>%
  gsub(pattern = "^ ", replacement = "", x = .) %>%
  strsplit(split = "  ") %>%
  do.call(what =rbind, args = .) %>%
  as.data.frame() %>%
  set_names(c("SHCODE", "COUNTRY  NAME", "WBCTRY", "SMPL98", "SMPL97"))
get.names.BL <- function(x, i) {
  if(!is.na(firstlines[i+1])) {
    data.frame(File = word(readme[x]),
               Colname = (c(readme[x:(firstlines[i+1]-1)]) %>%
      paste(collapse = " ") %>%
       gsub(pattern = ".{1,}: {1,}", replacement = "", x = .) %>%
       gsub(pattern = " {2,}", replacement = " ", x = .) %>%
       gsub(pattern = "^ | $", replacement = "", x = .) %>%
      strsplit(split = " ") %>%
        unlist
      )
    )
  } else {
    data.frame(File = word(readme[x]),
               Colname = (c(readme[x:length(readme)]) %>%
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
set.seed(1)
file_colname <- imap_dfr(firstlines, get.names.BL) 

set.names.BL <- function(x){
    read.table(paste0("data/barrolee/",x$File[1])) %>%
      purrr::set_names(x$Colname)
}

df <- file_colname %>%
  split(.$File) %>%
  map_dfc(set.names.BL) %>% 
  mutate(WBCTRY = as.character(countries$WBCTRY)) #%>%
# t()
df_list <- list()
df_list[[1]] <- df %>%
  select(matches("[a-z][0-9]{1}$|grsh5|invsh5|invsh4|
                    grsh4|govsh4|govsh5|gvxdxe4|gvxdxe5"),
        -durs1)
df_list[[2]] <- df %>%
  select(matches("[a-z][0-9]{2}$|gdpsh5|gdpsh4|pop15|pop65|lifee0|pysh5|pcsh5|pish5|pgsh5"),
         -c(matches("grsh5|invsh5|invsh4|
                    grsh4|govsh4|govsh5|gvxdxe4|gvxdxe5|smpl97")
      ))
df_list[[3]] <- df %>%
  select(matches("bmp[0-9]l|bmp2ml"))
df_list[[4]] <- df %>%
  select(names(df)[which(!names(df) %in% c(names(dfy), names(dfyy), names(dfyl)))])
# с одной цифрой или просто плохие
# INVWBx GRWBx INVSH5x GRSH5x 
# INVSH4x GRSH4x 
# POP15xx
# GOVSH4x GOVSH5x GDEx GEERECx GEETOTx
# INVPUBx GGCFDx GVXDXE4x GVXDXE5x PINSTABx ASSASSPx
# COUPx REVOLx PINSTABx POLRIGHTx CIVLIBx EXx
# IMx BMPx TOTx LLYx

# SMPL97 DURS1

# GDPSH4xx GDPSH5xx POP65xx

# Type xx for date indicators, x for averagee period indicators, con for constant

cut.colname.BL <- function(df, i) {
  if(i == 3) {
    data.frame(Colname = colnames(df)) %>%
      inner_join(file_colname, by = "Colname") %>%
      mutate(Colname_cut = "bmpl",
             Type = "x") %>%
      return()
  } else {if(i == 4) {
    i <- 0
  }
  data.frame(Colname = colnames(df)) %>%
    inner_join(file_colname, by = "Colname") %>%
    mutate(Colname_cut = substr(Colname, 1,nchar(Colname)-i),
           Type = ifelse(i == 2, "xx",
                          ifelse(i == 1,
                                  "x",
                                  "con"))) %>%
    unique() %>%
    return()
  }
}
cut_colname_type <- imap_dfr(df_list, cut.colname.BL)  
map(cut_colname_type %>% pull(Colname_cut) %>% unique, melt.BL, df)
melt.BL <- function(cn, df) {
  df %>%
    select(matches(paste0("^", cn, "[0-9]{2}$")), WBCTRY) %>%
    as.data.table() %>%
    melt.data.table(id.vars = "WBCTRY",
                    variable.name = "Indicator",
                    value.name = "Value") %T>%
    mutate(Year = paste0("19",
                         substring(Indicator,
                                   nchar(Indicator)-1)),
           Indicator = substr(Indicator, 1, nchar(Indicator)-2))
}
# train <- df_colnames %>% sample_n(3)
# 
  
    
read.table(paste0("data/barrolee/","codes.prn"))
