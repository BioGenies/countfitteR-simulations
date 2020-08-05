library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(countfitteR)
library(latex2exp)
# read_aklides <- function(x) {
#   
#   all_sheets <- excel_sheets(x)
#   do.call(rbind, lapply(all_sheets, function(ith_sheet) {
#     dat <- read_xlsx(x, sheet = ith_sheet)
#     
#     marker_id <- !grepl("...", x = colnames(dat), fixed = TRUE)
#     marker_names <- c("", paste0(colnames(dat)[marker_id], "_"))
#     
#     chr_dat <- dat[-c(1, 2), ]
#     final_dat <- do.call(cbind, lapply(chr_dat, as.numeric))
# 
#     colnames(final_dat) <- gsub("[^a-z_]*", "", 
#          x = paste0(marker_names[cumsum(marker_id) + 1], 
#                                   dat[1, ], "_", dat[2, ]),
#          ignore.case = TRUE)
#     
#     data.frame(table = ith_sheet, final_dat, stringsAsFactors = FALSE)
#   }))
# }
# 
# akl1 <- read_aklides("./case-study-data/RAW Data AKLIDES NUK.xlsx")
# 
# library(ggplot2)
# 
# overview_data <- select(akl1, table, ImageNr_n, ObjectNr_n, 
#        MarkerFITC_FociOK_n, MarkerAPC_FociOK_n) %>% 
#   mutate(ImageNr_n = factor(ImageNr_n), 
#          ObjectNr_n = factor(ObjectNr_n)) %>% 
#   pivot_longer(cols = c(MarkerFITC_FociOK_n, MarkerAPC_FociOK_n)) %>% 
#   mutate(marker = gsub("Marker", "", sapply(strsplit(name, "_"), first))) %>% 
#   select(-name)
#   
# 
# ggplot(overview_data, aes(x = ObjectNr_n, y = value, fill = marker)) +
#   geom_col(position = "dodge") +
#   facet_grid(table ~ ImageNr_n)
# 
# sort(unique(overview_data[["ObjectNr_n"]]))

dat_nucdetect <- read.csv2("./case-study-data/results.csv") %>% 
  select(-Nucleus) %>% 
  setNames(c("md5", "Red.Foci", "Green.Foci")) %>% 
  mutate(Software = "NucDetect")

dat_cellprofiler <- inner_join(read.csv("./case-study-data/assignment.csv"),
                               read.csv("./case-study-data/counts.csv")) %>% 
  select(-Number) %>% 
  setNames(c("md5", "Red.Foci", "Green.Foci")) %>% 
  mutate(Software = "CellProfiler")

dat <- rbind(dat_cellprofiler, dat_nucdetect)

selected_model <- do.call(rbind, 
                          lapply(unique(dat[["Software"]]), 
                                 function(ith_software) {
                                   software_dat <- filter(dat, Software == ith_software)
                                   do.call(rbind, 
                                           lapply(c("Red.Foci", "Green.Foci"), 
                                                  function(ith_channel) {
                                                    counts <- split(software_dat[[ith_channel]], 
                                                                    software_dat[["md5"]])
                                                    # if the image is empty, CellProfiler give an
                                                    # empty vector
                                                    counts[lengths(counts) == 0] <- 0
                                                    all_fits <- fit_counts(counts, model = "all")
                                                    cbind(software = ith_software,
                                                          channel = ith_channel,
                                                          select_model(all_fits))
                                                  }))
                                 }))

p <- mutate(selected_model, 
            chosen_model = factor(chosen_model, levels = c("Poisson",
                                                           "ZIP",
                                                           "NB",
                                                           "ZINB")),
            channel = factor(channel, labels = c(TeX("$\\gamma H2AX$"), 
                                                 TeX("$53BP1$")))) %>% 
  ggplot(aes(x = chosen_model)) +
  geom_bar() +
  geom_label(aes(y = ..count.., label = ..count..), stat = "count") +
  facet_grid(software ~ channel, labeller = "label_parsed") +
  scale_x_discrete("Selected model") +
  scale_y_continuous("Number of images") +
  theme_bw()

ggsave("./files/fig_case_study.eps", plot = p, width = 20, 
       height = 15, units = "cm")

group_by(selected_model, software, channel, chosen_model) %>% 
  summarise(total = length(chosen_model)) %>% 
  mutate(frac = total/sum(total))

group_by(selected_model, software, channel) %>% 
  summarise(is_poisson = mean(chosen_model == "Poisson")*100) 
