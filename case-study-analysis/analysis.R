library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

read_aklides <- function(x) {
  
  all_sheets <- excel_sheets(x)
  do.call(rbind, lapply(all_sheets, function(ith_sheet) {
    dat <- read_xlsx(x, sheet = ith_sheet)
    
    marker_id <- !grepl("...", x = colnames(dat), fixed = TRUE)
    marker_names <- c("", paste0(colnames(dat)[marker_id], "_"))
    
    chr_dat <- dat[-c(1, 2), ]
    final_dat <- do.call(cbind, lapply(chr_dat, as.numeric))

    colnames(final_dat) <- gsub("[^a-z_]*", "", 
         x = paste0(marker_names[cumsum(marker_id) + 1], 
                                  dat[1, ], "_", dat[2, ]),
         ignore.case = TRUE)
    
    data.frame(table = ith_sheet, final_dat, stringsAsFactors = FALSE)
  }))
}

akl1 <- read_aklides("./case-study-data/RAW Data AKLIDES NUK.xlsx")

library(ggplot2)

overview_data <- select(akl1, table, ImageNr_n, ObjectNr_n, 
       MarkerFITC_FociOK_n, MarkerAPC_FociOK_n) %>% 
  mutate(ImageNr_n = factor(ImageNr_n), 
         ObjectNr_n = factor(ObjectNr_n)) %>% 
  pivot_longer(cols = c(MarkerFITC_FociOK_n, MarkerAPC_FociOK_n)) %>% 
  mutate(marker = gsub("Marker", "", sapply(strsplit(name, "_"), first))) %>% 
  select(-name)
  

ggplot(overview_data, aes(x = ObjectNr_n, y = value, fill = marker)) +
  geom_col(position = "dodge") +
  facet_grid(table ~ ImageNr_n)

sort(unique(overview_data[["ObjectNr_n"]]))
