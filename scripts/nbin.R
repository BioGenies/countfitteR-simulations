library(tidyr)
library(countfitteR)
library(dplyr)
library(pbapply)
library(ggplot2)

set.seed(210)

reps <- 100
ns <- c(20, 50, 100)
lambdas <- seq(from = 0.1, to = 10, length.out = 20)
rs <- seq(from = 0.01, to = 1, length.out = 10)
sizes <- seq(from = 1, to = 10, length.out = 10)

ZINB_data <- pblapply(ns, function(ith_n){
  lapply(rs, function(ith_r){
    lapply(sizes, function(ith_size) {
      lapply(lambdas, function(ith_lambda) {
        lapply(1:reps, function(ith_rep) {
          sim <- rZINB(n = ith_n, size = ith_size, mu = ith_lambda, r = ith_r)
          data.frame(distribution = "nbin",
                     n = ith_n,
                     size = ith_size,
                     lambda = ith_lambda,
                     variance = ith_lambda + (ith_lambda^2)/(ith_r),
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
}) %>%
  bind_rows() 




save(ZINB_data, file = "./data/nbin_simulations.Rda")

