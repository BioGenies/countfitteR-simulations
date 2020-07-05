library(tidyr)
library(dplyr)
library(pbapply)
library(pscl)
library(MASS)
library(countfitteR)
library(ggplot2)


first_step <- function(model_pois, model_nb){
  lrt <- lmtest::lrtest(model_pois, model_nb)
  chosen_model <- ifelse(lrt$`Pr(>Chisq)`[2] < 0.05, "nb", "pois")
  chosen_model
}

second_step <- function(df, model_pois, model_nb, chosen_model){
  if(chosen_model == "nb"){
    model_zinb <- zeroinfl(sim_col ~ 1, data = df, dist = "negbin")
    vuong_test <- capture.output(vuong(model_nb, model_zinb))
    vuong_test <- unlist(strsplit(vuong_test[6], " "))
    vuong_test <- vuong_test[vuong_test != ""]
    if(vuong_test[6] < 0.05   ) {
      chosen_model2 <- ifelse(vuong_test[3] == "model1", "nb", "zinb")     
    } else {
      chosen_model2 <- "nb"
    }
  } else {
    model_zinp <- zeroinfl(sim_col ~ 1, data = df)
    vuong_test <- capture.output(vuong(model_pois, model_zinp))
    vuong_test <- unlist(strsplit(vuong_test[6], " "))
    vuong_test <- vuong_test[vuong_test != ""]
    if(vuong_test[6] < 0.05) {
      chosen_model2 <- ifelse(vuong_test[3] == "model1", "pois", "zip")     
    } else {
      chosen_model2 <- "pois"
    }
  }
  chosen_model2
}


n <- c(50, 100, 200)
lambda <- c(2, 5, 10)
r = seq(from = 0.1, to = 0.9, length.out = 9)
size_scale <- c(0.5, 1, 2)
iter <- 100

############# ZIP

set.seed(210)

twostep_sim_zip <- pblapply(r, function(ith_r){
  lapply(lambda, function(ith_lambda){
    lapply(n, function(ith_n){
        correct <- lapply(1:iter, function(x){
          tryCatch({  
            df <- data.frame(sim_col = rZIP(n = ith_n, lambda = ith_lambda, r = ith_r))
  
            model_pois <- stats::glm(sim_col ~ 1, family = poisson(link = "log"), data = df)
            model_nb <-  glm.nb(sim_col ~ 1,data = df)
            
            chosen_model <- first_step(model_pois, model_nb)
            chosen_model2 <- second_step(df, model_pois, model_nb, chosen_model)
            
            chosen_model2 == "zip" },
          error = function(e) NA)
        }) %>%
          unlist()
        data.frame(model = "zip",
                   method = "twostep",
                   n = ith_n, 
                   lambda = ith_lambda,
                   r = ith_r,
                   pow_mean = mean(correct, na.rm = TRUE),
                   pow_sd = sd(correct, na.rm = TRUE),
                   na = sum(is.na(correct)))

    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
}) %>%
  bind_rows()        

save(twostep_sim_zip, file = "./data/twostep_sim_zip.rda")


ggplot(twostep_sim_zip, aes(x = r, y = pow_mean)) + 
  geom_path() +
  facet_grid(lambda~n)


############## ZINB

set.seed(210)


twostep_sim_zinb <- pblapply(r, function(ith_r){
  lapply(lambda, function(ith_lambda){
    lapply(size_scale, function(ith_size){
      lapply(n, function(ith_n){
        correct <- lapply(1:iter, function(x){
          tryCatch({
              df <- data.frame(sim_col = rZINB(n = ith_n, mu = ith_lambda, size = ith_lambda * ith_size,r = ith_r))
              # first step
              model_pois <- stats::glm(sim_col ~ 1, family = poisson(link = "log"), data = df)
              model_nb <-  tryCatch(glm.nb(sim_col ~ 1,data = df),
                                    error = function(e)NULL)
              chosen_model <- first_step(model_pois, model_nb)
              chosen_model2 <- second_step(df, model_pois, model_nb, chosen_model)
              
              chosen_model2 == "zinb"},
            error = function(e) NA)
           
        }) %>%
          unlist()
        data.frame(model = "zip",
                   method = "twostep",
                   n = ith_n, 
                   lambda = ith_lambda,
                   size = ith_lambda * ith_size,
                   r = ith_r,
                   pow_mean = mean(correct, na.rm = TRUE),
                   pow_sd = sd(correct, na.rm = TRUE),
                   na = sum(is.na(correct)))
      }) %>%
        bind_rows()  
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
}) %>%
  bind_rows()        

save(twostep_sim_zinb, file = "./data/twostep_sim_zinb.rda")


ggplot(twostep_sim_zinb, aes(x = r, y = pow_mean, color = size, group = size)) + 
  geom_point() +
  geom_line() +
  facet_grid(lambda~n)



########### POIS


set.seed(210)

twostep_sim_pois <- pblapply(lambda, function(ith_lambda){
    lapply(n, function(ith_n){
      correct <- lapply(1:iter, function(x){
        tryCatch({  
          df <- data.frame(sim_col = rpois(n = ith_n, lambda = ith_lambda))
          
          model_pois <- stats::glm(sim_col ~ 1, family = poisson(link = "log"), data = df)
          model_nb <-  tryCatch(glm.nb(sim_col ~ 1,data = df),
                                error = function(e) NULL)
          
          chosen_model <- first_step(model_pois, model_nb)
          chosen_model2 <- second_step(df, model_pois, model_nb, chosen_model)
          chosen_model2 == "pois" },
          error = function(e) NA)
      }) %>%
        unlist()
      data.frame(model = "pois",
                 method = "twostep",
                 n = ith_n, 
                 lambda = ith_lambda,
                 pow_mean = mean(correct, na.rm = TRUE),
                 pow_sd = sd(correct, na.rm = TRUE),
                 na = sum(is.na(correct)))
      
  }) %>%
    bind_rows()
}) %>%
  bind_rows()        

save(twostep_sim_pois, file = "./data/twostep_sim_pois.rda")


ggplot(twostep_sim_pois, aes(x = 1, y = pow_mean)) + 
  geom_point() +
  facet_grid(lambda~n)


#### NB

set.seed(210)


twostep_sim_nb <- pblapply(lambda, function(ith_lambda){
    lapply(size_scale, function(ith_size){
      lapply(n, function(ith_n){
        correct <- lapply(1:iter, function(x){
          tryCatch({
            df <- data.frame(sim_col = rnbinom(n = ith_n, mu = ith_lambda, size = ith_lambda * ith_size,))
            # first step
            model_pois <- stats::glm(sim_col ~ 1, family = poisson(link = "log"), data = df)
            model_nb <-  tryCatch(glm.nb(sim_col ~ 1,data = df),
                                  error = function(e)NULL)
            chosen_model <- first_step(model_pois, model_nb)
            chosen_model2 <- second_step(df, model_pois, model_nb, chosen_model)
            
            chosen_model2 == "nb"},
            error = function(e) NA)
          
        }) %>%
          unlist()
        data.frame(model = "nb",
                   method = "twostep",
                   n = ith_n, 
                   lambda = ith_lambda,
                   size = ith_lambda * ith_size,
                   pow_mean = mean(correct, na.rm = TRUE),
                   pow_sd = sd(correct, na.rm = TRUE),
                   na = sum(is.na(correct)))
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
}) %>%
  bind_rows()        

save(twostep_sim_nb, file = "./data/twostep_sim_nb.rda")


ggplot(twostep_sim_nb, aes(x = 1, y = pow_mean, color = size, group = size)) + 
  geom_point() +
  geom_line() +
  facet_grid(lambda~n)


