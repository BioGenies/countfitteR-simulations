library(tidyr)
library(countfitteR)
library(dplyr)
library(pbapply)
library(ggplot2)

n <- c(50, 100, 200)
lambda <- c(2, 5, 10)
r = seq(from = 0.1, to = 0.9, length.out = 9)
size_scale <- c(0.5, 1, 2)
iter <- 100

######## ZIP

set.seed(210)


bic_sim_zip <- pblapply(r, function(ith_r){
  lapply(lambda, function(ith_lambda){
    lapply(n, function(ith_n){
      correct <- lapply(1:iter, function(x){
        df <- data.frame(sim_ZIP = rZIP(n = ith_n, lambda = ith_lambda, r = ith_r))
        fitlist <- fit_counts(df, model = "all")
        models <- c("pois", "nb", "zip", "zinb")
        best_model <- models[which.min(c(fitlist[["sim_ZIP_pois"]]$BIC,
                                         fitlist[["sim_ZIP_nb"]]$BIC,
                                         fitlist[["sim_ZIP_zip"]]$BIC,
                                         fitlist[["sim_ZIP_zinb"]]$BIC))]
        best_model == "zip"
      }) %>%
        unlist() 
      data.frame(model = "zip",
                 method = "bic",
                 n = ith_n, 
                 lambda = ith_lambda,
                 r = ith_r,
                 pow_mean = mean(correct, na.rm = TRUE),
                 pow_sd = sd(correct, na.rm = TRUE),
                 na = 0)
      
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

save(bic_sim_zip, file = "./data/bic_sim_zip.rda")

ggplot(bic_sim_zip, aes(x = r, y = pow_mean)) + 
  geom_path() +
  facet_grid(lambda~n)

########################## ZINB

set.seed(210)

bic_sim_zinb <- pblapply(r, function(ith_r){
  lapply(lambda, function(ith_lambda){
    lapply(size_scale, function(ith_size){
      lapply(n, function(ith_n){
        correct <- lapply(1:iter, function(x){
          df <- data.frame(sim_ZINB = rZINB(n = ith_n, mu = ith_lambda, size = ith_lambda * ith_size, r = ith_r))
          fitlist <- fit_counts(df, model = "all")
          models <- c("pois", "nb", "zip", "zinb")
          best_model <- models[which.min(c(fitlist[["sim_ZINB_pois"]]$BIC,
                                           fitlist[["sim_ZINB_nb"]]$BIC,
                                           fitlist[["sim_ZINB_zip"]]$BIC,
                                           fitlist[["sim_ZINB_zinb"]]$BIC))]
          print(best_model)
          best_model == "zinb"
        }) %>%
          unlist() 
        
        data.frame(model = "zinb",
                   method = "bic",
                   n = ith_n, 
                   lambda = ith_lambda,
                   size = ith_lambda * ith_size,
                   r = ith_r,
                   pow_mean = mean(correct, na.rm = TRUE),
                   pow_sd = sd(correct, na.rm = TRUE),
                   na = 0)
      }) %>%
        bind_rows()  
    }) %>%
      bind_rows()  
  }) %>%
    bind_rows()
}) %>%
  bind_rows()


save(bic_sim_zinb, file = "./data/bic_sim_zinb.rda")

ggplot(bic_sim_zinb, aes(x = r, y = pow_mean, color = factor(size))) + 
  geom_point() +
  geom_line() +
  facet_grid(lambda~n)


############### POIS

set.seed(210)

bic_sim_pois <- pblapply(lambda, function(ith_lambda){
    lapply(n, function(ith_n){
      correct <- lapply(1:iter, function(x){
        df <- data.frame(sim_col = rpois(n = ith_n, lambda = ith_lambda))
        fitlist <- fit_counts(df, model = "all")
        models <- c("pois", "nb", "zip", "zinb")
        best_model <- models[which.min(c(fitlist[["sim_col_pois"]]$BIC,
                                         fitlist[["sim_col_nb"]]$BIC,
                                         fitlist[["sim_col_zip"]]$BIC,
                                         fitlist[["sim_col_zinb"]]$BIC))]
        best_model == "pois"
      }) %>%
        unlist() 
      data.frame(model = "pois",
                 method = "bic",
                 n = ith_n, 
                 lambda = ith_lambda,
                 pow_mean = mean(correct, na.rm = TRUE),
                 pow_sd = sd(correct, na.rm = TRUE),
                 na = 0)

  }) %>%
    bind_rows()
}) %>%
  bind_rows()

save(bic_sim_pois, file = "./data/bic_sim_pois.rda")

ggplot(bic_sim_pois, aes(x = 1, y = pow_mean)) + 
  geom_point() +
  facet_grid(lambda~n)



############### NB

set.seed(210)

bic_sim_nb <- pblapply(lambda, function(ith_lambda){
    lapply(size_scale, function(ith_size){
      lapply(n, function(ith_n){
        correct <- lapply(1:iter, function(x){
          df <- data.frame(sim_col = rnbinom(n = ith_n, mu = ith_lambda, size = ith_lambda * ith_size))
          fitlist <- fit_counts(df, model = "all")
          models <- c("pois", "nb", "zip", "zinb")
          best_model <- models[which.min(c(fitlist[["sim_col_pois"]]$BIC,
                                           fitlist[["sim_col_nb"]]$BIC,
                                           fitlist[["sim_col_zip"]]$BIC,
                                           fitlist[["sim_col_zinb"]]$BIC))]
          best_model == "nb"
        }) %>%
          unlist() 
        data.frame(model = "nb",
                   method = "bic",
                   n = ith_n, 
                   lambda = ith_lambda,
                   size = ith_lambda * ith_size,
                   pow_mean = mean(correct, na.rm = TRUE),
                   pow_sd = sd(correct, na.rm = TRUE),
                   na = 0)
 
    }) %>%
      bind_rows()  
  }) %>%
    bind_rows()
}) %>%
  bind_rows()


save(bic_sim_nb, file = "./data/bic_sim_nb.rda")

ggplot(bic_sim_nb, aes(x = 1, y = pow_mean, color = size, group = size)) + 
  geom_point() +
  facet_grid(lambda~n)
