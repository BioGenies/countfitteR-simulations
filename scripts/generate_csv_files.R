library(readr)

load("./data/bic_sim_zip.rda")
load("./data/bic_sim_zinb.rda")
load("./data/bic_sim_pois.rda")
load("./data/bic_sim_nb.rda")
load("./data/twostep_sim_zip.rda")
load("./data/twostep_sim_zinb.rda")
load("./data/twostep_sim_pois.rda")
load("./data/twostep_sim_nb.rda")


sim_pois <- rbind(bic_sim_pois, twostep_sim_pois)
sim_nb <- rbind(bic_sim_nb, twostep_sim_nb)
sim_zip <- rbind(bic_sim_zip, twostep_sim_zip)
sim_zinb <- rbind(bic_sim_zinb, twostep_sim_zinb)

write_csv(sim_pois, path = "./files/S1.csv")
write_csv(sim_nb, path = "./files/S2.csv")
write_csv(sim_zip, path = "./files/S3.csv")
write_csv(sim_zinb, path = "./files/S4.csv")
