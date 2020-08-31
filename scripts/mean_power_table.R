library(dplyr)
library(tidyr)
library(xtable)
library(gt)
iter <- 1000

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

means <- data.frame("Poisson_mean" = mean(bic_sim_pois[["pow_mean"]]),
           "ZIP_mean" = mean(bic_sim_zip[["pow_mean"]]),
           "NB_mean" = mean(bic_sim_nb[["pow_mean"]]),
           "ZINB_mean" = mean(bic_sim_zinb[["pow_mean"]])) %>%
  bind_rows(data.frame("Poisson_mean" = mean(twostep_sim_pois[["pow_mean"]], na.rm = TRUE),
                       "ZIP_mean" =   mean(twostep_sim_zip[["pow_mean"]], na.rm = TRUE),
                       "NB_mean" =   mean(twostep_sim_nb[["pow_mean"]], na.rm = TRUE),
                       "ZINB_mean" =  mean(twostep_sim_zinb[["pow_mean"]], na.rm = TRUE))) %>%
  mutate("Method" = c("CountfitteR", "Two-step"))  %>%
   pivot_longer(c("Poisson_mean", "ZINB_mean", "NB_mean", "ZIP_mean"), names_to = "Distribution", values_to = "value") %>%
  mutate(value = round(value, 2))



solved <- data.frame("Poisson_solved" = sum(iter - bic_sim_pois[["uncomputable"]]),
           "ZIP_solved" = sum(iter - bic_sim_zip[["uncomputable"]]),
           "NB_solved" = sum(iter - bic_sim_nb[["uncomputable"]]),
           "ZINB_solved" = sum(iter - bic_sim_zinb[["uncomputable"]])) %>%
  bind_rows(data.frame("Poisson_solved" = sum(iter - twostep_sim_pois[["uncomputable"]], na.rm = TRUE),
                       "ZIP_solved" =   sum(iter - twostep_sim_zip[["uncomputable"]], na.rm = TRUE),
                       "NB_solved" =   sum(iter - twostep_sim_nb[["uncomputable"]], na.rm = TRUE),
                       "ZINB_solved" =  sum(iter - twostep_sim_zinb[["uncomputable"]], na.rm = TRUE))) %>%
  mutate("Method" = c("CountfitteR", "Two-step"),
         "Poisson_solved" = paste0(round(100 * Poisson_solved / max(Poisson_solved),2), "%"),
         "ZIP_solved" = paste0(round(100 * ZIP_solved / max(ZIP_solved),2), "%"),
         "NB_solved" = paste0(round(100 * NB_solved / max(NB_solved),2), "%"),
         "ZINB_solved" = paste0(round(100 * ZINB_solved / max(ZINB_solved),2), "%")) %>%
  pivot_longer(c("Poisson_solved", "ZINB_solved", "NB_solved", "ZIP_solved"), names_to = "Distribution", values_to = "value")

rbind(means, solved) %>%
  pivot_wider(names_from = "Method", values_from = value) %>%
  arrange(Distribution) %>%
  separate(Distribution, c("Distribution", "x"), sep = "_") %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()
  






