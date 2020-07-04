library(ggplot2)

load("./data/pois_simulations.Rda")
load("./data/nbin_simulations.Rda")


# ZIP

filter(ZIP_data, moment == "sim_mean") %>% 
  ggplot(aes(x = lambda, y = value, color = r)) +
  geom_point() +
  coord_equal() +
  facet_wrap(~factor(n))

filter(ZIP_data, moment == "sim_mean") %>% 
  ggplot(aes(x = factor(lambda), y = value)) +
  geom_boxplot() +
  geom_point(aes(y = lambda), color = "red", size = 2) +
  facet_wrap(~ r) +
  facet_wrap(~factor(n))

ZIP_data  %>%
  pivot_wider(names_from = moment, values_from = value) %>%
  ggplot(aes(x = sim_mean, y = sim_var, color = r)) +
  geom_point() + 
  geom_smooth() +
  facet_wrap(~factor(n))



# ZINB

filter(ZINB_data, moment == "sim_mean") %>% 
  ggplot(aes(x = lambda, y = value, color = r)) +
  geom_point() +
  coord_equal() +
  facet_wrap(~factor(n))

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

