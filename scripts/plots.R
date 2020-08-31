#devtools::install_github("teunbrand/ggh4x")
library(ggh4x)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
library(patchwork)
iter <- 1000

load("./data/bic_sim_zip.rda")
load("./data/bic_sim_zinb.rda")
load("./data/bic_sim_pois.rda")
load("./data/bic_sim_nb.rda")
load("./data/twostep_sim_zip.rda")
load("./data/twostep_sim_zinb.rda")
load("./data/twostep_sim_pois.rda")
load("./data/twostep_sim_nb.rda")



# Zero-inflated Poisson

sim_zip <- rbind(bic_sim_zip, twostep_sim_zip)
sim_zip[["selected"]] <- iter - sim_zip[["uncomputable"]]
sim_zip[["correct distribution"]] <- sim_zip[["selected"]] * sim_zip[["pow_mean"]]
sim_zip[["wrong distribution"]] <- sim_zip[["selected"]] * (1-sim_zip[["pow_mean"]])

p <- sim_zip %>%
  pivot_longer(cols = c("uncomputable", "correct distribution", "wrong distribution"), names_to = "result", values_to = "counts") %>%
  mutate(result = factor(result, levels = c("uncomputable", "wrong distribution", "correct distribution")),
         lambda = factor(lambda,
                         levels = sort(unique(lambda)),
                         labels = TeX(paste0("$\\lambda$", ": ", sort(unique(lambda)))))) %>%
  ggplot(aes(x = r, y = counts/iter, fill = result)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_nested(n~lambda+method, labeller = labeller(lambda = label_parsed, 
                                                    method = label_value, 
                                                    n = label_both)) +
  scale_fill_manual(values = c('#000000', '#ef8a62','#67a9cf')) +
  theme_bw() +
  ylab("Empirical Power") +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9))

p
ggsave("./files/fig_zip.eps", plot = p, width = 20, height = 10, units = "cm")




# Zero-inflated Negative Binomial

sim_zinb <- rbind(bic_sim_zinb, twostep_sim_zinb)
sim_zinb[["selected"]] <- iter - sim_zinb[["uncomputable"]]
sim_zinb[["correct distribution"]] <- sim_zinb[["selected"]] * sim_zinb[["pow_mean"]]
sim_zinb[["wrong distribution"]] <- sim_zinb[["selected"]] * (1-sim_zinb[["pow_mean"]])

sim_zinb[["size_scale"]] <- sim_zinb[["lambda"]] / sim_zinb[["size"]]
scales <- sim_zinb[["size_scale"]] %>% unique()

plot_list <- list()
for(ith_scale in scales){
  
  leg_pos <- ifelse(ith_scale != 0.5, "none", "bottom")
  
  p <- sim_zinb %>%
    filter(size_scale == ith_scale) %>%
    pivot_longer(cols = c("uncomputable", "correct distribution", "wrong distribution"), names_to = "result", values_to = "counts") %>%
    mutate(result = factor(result, levels = c("uncomputable", "wrong distribution", "correct distribution")), lambda_num = lambda, 
           lambda = factor(lambda,
                           levels = sort(unique(lambda)),
                           labels = TeX(paste0("$\\lambda$", ": ", sort(unique(lambda)))))) %>%
    ggplot(aes(x = r, y = counts/iter, fill = result)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_nested(n~lambda+method, labeller = labeller(lambda = label_parsed, 
                                                      method = label_value, 
                                                      n = label_both)) +
    scale_fill_manual(values = c('#000000', '#ef8a62','#67a9cf')) +
    theme_bw() + 
    ggtitle(TeX(paste("$\\theta$ =",  "$\\lambda \\times$", ith_scale))) +
    ylab("Empirical Power") +
    theme(legend.position = leg_pos, legend.title = element_blank()) + 
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9))
  
      
  
  plot_list[[length(plot_list)+1]] <- p
}

ppp <- plot_list[[1]] / plot_list[[2]] / plot_list[[3]]
ppp
ggsave("./files/fig_zinb.eps", plot = ppp, width = 20, height = 30, units = "cm")






# Poisson (supplementary) S1.eps

sim_pois <- rbind(bic_sim_pois, twostep_sim_pois)
sim_pois[["selected"]] <- iter - sim_pois[["uncomputable"]]
sim_pois[["correct distribution"]] <- sim_pois[["selected"]] * sim_pois[["pow_mean"]]
sim_pois[["wrong distribution"]] <- sim_pois[["selected"]] * (1-sim_pois[["pow_mean"]])

p <- sim_pois %>%
  pivot_longer(cols = c("uncomputable", "correct distribution", "wrong distribution"), names_to = "result", values_to = "counts") %>%
  mutate(result = factor(result, levels = c("uncomputable", "wrong distribution", "correct distribution")),
         lambda = factor(lambda,
                         levels = sort(unique(lambda)),
                         labels = TeX(paste0("$\\lambda$", ": ", sort(unique(lambda)))))) %>%
  ggplot(aes(x = "", y = counts/iter, fill = result)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_nested(n~lambda+method, labeller = labeller(lambda = label_parsed, 
                                                    method = label_value, 
                                                    n = label_both)) +
  scale_fill_manual(values = c('#000000', '#ef8a62','#67a9cf')) +
  theme_bw() +
  ylab("Empirical Power") +
  xlab("") +
  theme(legend.position = "bottom", legend.title = element_blank())

p
ggsave("./files/S1.eps", plot = p, width = 20, height = 10, units = "cm")






# Negative Binomial (supplementary) S2.eps

sim_nb <- rbind(bic_sim_nb, twostep_sim_nb)
sim_nb[["selected"]] <- iter - sim_nb[["uncomputable"]]
sim_nb[["correct distribution"]] <- sim_nb[["selected"]] * sim_nb[["pow_mean"]]
sim_nb[["wrong distribution"]] <- sim_nb[["selected"]] * (1-sim_nb[["pow_mean"]])

sim_nb[["size_scale"]] <- sim_nb[["lambda"]] / sim_nb[["size"]]
scales <- sim_nb[["size_scale"]] %>% unique()

plot_list <- list()
for(ith_scale in scales){
  
  leg_pos <- ifelse(ith_scale != 0.5, "none", "bottom")
  
  p <- sim_nb %>%
    filter(size_scale == ith_scale) %>%
    pivot_longer(cols = c("uncomputable", "correct distribution", "wrong distribution"), names_to = "result", values_to = "counts") %>%
    mutate(result = factor(result, levels = c("uncomputable", "wrong distribution", "correct distribution")), lambda_num = lambda, 
           lambda = factor(lambda,
                           levels = sort(unique(lambda)),
                           labels = TeX(paste0("$\\lambda$", ": ", sort(unique(lambda)))))) %>%
    ggplot(aes(x = "", y = counts/iter, fill = result)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_nested(n~lambda+method, labeller = labeller(lambda = label_parsed, 
                                                      method = label_value, 
                                                      n = label_both)) +
    scale_fill_manual(values = c('#000000', '#ef8a62','#67a9cf')) +
    theme_bw() + 
    ggtitle(TeX(paste("$\\theta$ =",  "$\\lambda \\times$", ith_scale))) +
    ylab("Empirical Power") +
    xlab("")
    theme(legend.position = leg_pos, legend.title = element_blank()) 
  
  
  
  plot_list[[length(plot_list)+1]] <- p
}

ppp <- plot_list[[1]] / plot_list[[2]] / plot_list[[3]]
ppp
ggsave("./files/S2.eps", plot = ppp, width = 20, height = 30, units = "cm")










