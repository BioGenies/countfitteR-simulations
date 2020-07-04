library(tidyr)
library(countfitteR)
library(dplyr)
library(pbapply)

set.seed(210)

reps <- 100
n <- 100
lambda <- 2
r = 0.8
iter <- 500


corrects <- pblapply(1:iter, function(x){
  df <- data.frame(sim_NZIP = rZINB(n = n, mu = lambda, size = lambda/2, r = r))
  
  # first step
  model_nb <- MASS::glm.nb(sim_NZIP ~ 1,data = df)
  model_pois <- stats::glm(sim_NZIP ~ 1, family = poisson(link = "log"), data = df)
  
  lrt <- lmtest::lrtest(model_pois, model_nb)
  chosen_model <- ifelse(lrt$`Pr(>Chisq)`[2] < 0.05, "nbin", "pois")
  
  # second step
  if(chosen_model == "nbin"){
    model_zinb <- pscl::zeroinfl(sim_NZIP ~ 1, data = df, dist = "negbin")
    vuong_test <- capture.output(vuong(model_nb, model_zinb))
    vuong_test <- unlist(strsplit(vuong_test[6], " "))
    vuong_test <- vuong_test[vuong_test != ""]
    if(vuong_test[6] < 0.05   ) {
      chosen_model <- ifelse(vuong_test[3] == "model1", "nbin", "zinb")     
    } else {
      chosen_model <- "nbin"
    }
  } else {
    model_zinp <- pscl::zeroinfl(sim_NZIP ~ 1, data = df)
    vuong_test <- capture.output(vuong(model_pois, model_zinp))
    vuong_test <- unlist(strsplit(vuong_test[6], " "))
    vuong_test <- vuong_test[vuong_test != ""]
    if(vuong_test[6] < 0.05) {
      chosen_model <- ifelse(vuong_test[3] == "model1", "pois", "zip")     
    } else {
      chosen_model <- "pois"
    }
  }
  if(chosen_model == "zinb") 1  
}) %>%
  unlist() %>%
  sum() 

cat("countfitteR emp. power:", corrects / iter, ", n:", n, ", lambda:", lambda, ", r:", r)






