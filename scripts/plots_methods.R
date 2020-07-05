library(ggplot2)
library(dplyr)

load("./data/bic_sim_zip.rda")
load("./data/bic_sim_zinb.rda")
load("./data/twostep_sim_zip.rda")
load("./data/twostep_sim_zinb.rda")


sim_zinb <- rbind(bic_sim_zinb, twostep_sim_zinb)

ggplot(sim_zinb, aes(x = r, y = pow, col = method)) +
  geom_path() +
  facet_grid(lambda~size)
