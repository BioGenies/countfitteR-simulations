library(tidyr)
library(countfitteR)
library(dplyr)
library(pbapply)
library(ggplot2)

set.seed(210)

reps <- 100
ns <- c(20, 50, 100)
lambdas = seq(from = 0.1, to = 10, length.out = 20)
rs = seq(from = 0.01, to = 1, length.out = 10)


ZIP_data <- pblapply(ns, function(ith_n){
  lapply(rs, function(ith_r){
    lapply(lambdas, function(ith_lambda) {
      lapply(1:reps, function(ith_rep) {
        sim <- rZIP(n = ith_n, lambda = ith_lambda, r = ith_r)
        data.frame(distribution = "pois",
                   n = ith_n,
                   lambda = ith_lambda,
                   variance = ith_lambda,
                   r = ith_r,
                   rep = ith_rep,
                   moment = c("sim_mean", "sim_var"),
                   value = c(mean(sim), var(sim)))
      }) %>%
        bind_rows() 
    }) %>%
    bind_rows()
  }) %>%
    bind_rows()
}) %>%
  bind_rows()


save(ZIP_data, file = "./data/pois_simulations.Rda")

