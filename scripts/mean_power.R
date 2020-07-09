library(dplyr)
library(tidyr)
library(xtable)
iter <- 100

load("./data/bic_sim_zip.rda")
load("./data/bic_sim_zinb.rda")
load("./data/bic_sim_pois.rda")
load("./data/bic_sim_nb.rda")
load("./data/twostep_sim_zip.rda")
load("./data/twostep_sim_zinb.rda")
load("./data/twostep_sim_pois.rda")
load("./data/twostep_sim_nb.rda")

head(bic_sim_zip)


# Mean empirical power of countfitteR and two-step test for ZIP and ZINB distributions

data.frame("Poisson" = mean(bic_sim_pois[["pow_mean"]]),
           "ZIP" = mean(bic_sim_zip[["pow_mean"]]),
           "NB" = mean(bic_sim_nb[["pow_mean"]]),
           "ZINB" = mean(bic_sim_zinb[["pow_mean"]])) %>%
  bind_rows(data.frame("Poisson" = mean(twostep_sim_pois[["pow_mean"]], na.rm = TRUE),
                       "ZIP" =   mean(twostep_sim_zip[["pow_mean"]], na.rm = TRUE),
                       "NB" =   mean(twostep_sim_nb[["pow_mean"]], na.rm = TRUE),
                       "ZINB" =  mean(twostep_sim_zinb[["pow_mean"]], na.rm = TRUE))) %>%
  mutate("Method" = c("CountfitteR", "Two-step")) %>%
  select(Method, ZIP, ZINB) %>%
  xtable()
  






