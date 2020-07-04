library(tidyr)
library(countfitteR)
library(dplyr)
library(pbapply)
library(ggplot2)

reps <- 100
n <- 1000
lambdas = seq(from = 0.1, to = 10, length.out = 20)
rs = seq(from = 0.01, to = 1, length.out = 10)
nbin_size <- 1

ZINB_data <- pblapply(rs, function(ith_r){
  lapply(lambdas, function(ith_lambda) {
    lapply(1:reps, function(ith_rep) {
      sim <- rZINB(n = n, size = nbin_size, mu = ith_lambda, r = ith_r)
      data.frame(distribution = "nbin",
                 lambda = ith_lambda,
                 r = ith_r,
                 rep = ith_rep,
                 moment = c("mean", "sd"),
                 value = c(mean(sim), sd(sim)))
    }) %>%
      bind_rows() 
  }) %>%
    bind_rows()
}) %>%
  bind_rows() %>% 
  mutate(r = factor(round(r, 3)),
         lambda = round(lambda, 3))


# plots
filter(ZINB_data, moment == "mean") %>% 
  ggplot(aes(x = lambda, y = value, color = r)) +
  geom_point() +
  coord_equal()

filter(ZINB_data, moment == "mean") %>% 
  ggplot(aes(x = factor(lambda), y = value)) +
  geom_boxplot() +
  geom_point(aes(y = lambda), color = "red", size = 2) +
  facet_wrap(~ r)

ZINB_data  %>%
  pivot_wider(names_from = moment, values_from = value) %>%
  ggplot(aes(x = mean, y = sd, color = r)) +
  geom_point() + 
  geom_smooth()
