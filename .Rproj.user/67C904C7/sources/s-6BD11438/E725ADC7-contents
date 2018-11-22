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
               Colnames = (c(readme[x:(firstlines[i+1]-1)]) %>%
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
               Colnames = (c(readme[x:length(readme)]) %>%
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
df <- imap_dfr(firstlines, get.names.BL) %>%
  split(.$File) %>%
  map_dfc(set.names.BL) %>% 
  mutate(WBCTRY = as.character(countries$WBCTRY)) #%>%
  # t()
set.names.BL <- function(x){
    read.table(paste0("data/barrolee/",x$File[1])) %>%
      purrr::set_names(x$Colnames)
}

dfyy <- df %>%
  select(matches("50$|55$|60$|65$|70$|75$|80$|85$|90$"))
unique_cn_yy <- colnames(dfyy) %>%
  substr(1,nchar(.)-2) %>% 
  unique()
map(unique_cn_yy, melt.BL, df)
melt.BL <- function(cn, df) {
  cn = "h"
  df %>%
    select(matches(paste0("^", cn, "[0-9]{2}$")), WBCTRY) %>%
    as.data.table() %>%
    melt.data.table(id.vars = "WBCTRY")
}
# train <- df_colnames %>% sample_n(3)
# 
  
    
read.table(paste0("data/barrolee/","codes.prn"))
