library(posterior)
library(tidyr)
library(readr)
library(dplyr)
library(here)
library(lubridate)
library(rstan)
library(tidybayes)
library(latex2exp)

theme_set(theme_classic(base_size = 12) + theme(legend.position = "bottom"))

sg_dat <- read_csv(here("final_project", "Data", "sg_dat.csv"))
sg_dat_cutoff <- read_csv(here("final_project", "Data", "sg_dat_cutoff.csv"))

gumbel_model <- read_rds(here("final_project", "Models", "gumbel_model.rds"))
gumbel_m <- gumbel_model$draws() %>% as_draws_df
rm(gumbel_model)

posterior_mu_estimates <- gumbel_m %>% spread_draws(mu[n]) %>%
    group_by(n) %>%
    summarise(lower = quantile(mu, 0.025),
              median = quantile(mu,0.5),
              upper = quantile(mu,0.975))

posterior_beta_estimates <- gumbel_m %>% spread_draws(beta[n]) %>%
    group_by(n) %>%
    summarise(lower = quantile(beta, 0.025),
              median = quantile(beta,0.5),
              upper = quantile(beta,0.975))

mirror_gumbel_pdf <- function(y, mu,beta) {
    z = (mu-y)/beta
    1/beta*exp(-z-exp(-z))
}

mirror_gumbel_cdf <- function(y, mu, beta) {
    z = (mu-y)/beta
    exp(-exp(-z))
}

dens <- function(fitted_player, show_fit = T) {
    index <- sg_dat %>% filter(player == fitted_player) %>% pull(group) %>% unique()
    player_density <- tibble(t=seq(-4,4,0.01), 
                             density=mirror_gumbel_pdf(t, 
                                                       posterior_mu_estimates$median[index],
                                                       posterior_beta_estimates$median[index]))
    p <- sg_dat %>% filter(player == fitted_player) %>%
        ggplot() + coord_cartesian(xlim=c(-4,4)) +
        geom_density(aes(x=sg))

    if(show_fit) p <- p+geom_line(data=player_density, aes(x=t,y=density), color="red")
    p
}

dens("Dustin Johnson")
ggsave(here("final_project", "Figures", "posterior_density_johnson.pdf"), width=5, height=3, device="pdf")

dens("Dustin Johnson", F)
ggsave(here("final_project", "Figures", "empirical_density_johnson.pdf"), width=5, height=3, device="pdf")

dens("Jordan Spieth")

dens("Justin Thomas")
ggsave(here("final_project", "Figures", "posterior_density_thomas.pdf"), width=5, height=3, device="pdf")

dens("Justin Thomas", F)
ggsave(here("final_project", "Figures", "empirical_density_thomas.pdf"), width=5, height=3, device="pdf")

dens("Bryson DeChambeau")
ggsave(here("final_project", "Figures", "posterior_density_dechambeau.pdf"), width=5, height=3, device="pdf")

dens("Bryson DeChambeau", F)
ggsave(here("final_project", "Figures", "empirical_density_dechambeau.pdf"), width=5, height=3, device="pdf")

dens("Bo Van Pelt")
dens("Omar Uresti")
dens("Robert Allenby")


#dens("Collin Morikawa")
#ggsave("morikawa_density.pdf", width=5, height=3, device="pdf")

dens("Phil Mickelson")
dens("Tiger Woods")
dens("Jon Rahm")

quantile_data <- sg_dat %>% mutate(mu = posterior_mu_estimates$median[group],
                  beta = posterior_beta_estimates$median[group],
                  quantile = mirror_gumbel_cdf(sg, mu, beta))

bins <- 100

quantile_data %>% ggplot() + geom_histogram(aes(x=quantile), bins=bins) +
    geom_hline(yintercept = nrow(quantile_data)/bins, color='red') +
    scale_y_continuous(expand=c(0,0))

ggsave(here("final_project", "Figures", "quantile_plot.pdf"), width=5, height=4, device="pdf")

quantile_data %>% filter(player == "Dustin Johnson") %>% 
    ggplot() + geom_histogram(aes(x=quantile), bins=10)

quantile_data %>% filter(player == "Justin Thomas") %>% 
    ggplot() + geom_histogram(aes(x=quantile), bins=10)

quantile_data %>% filter(player == "Bryson DeChambeau") %>% 
    ggplot() + geom_histogram(aes(x=quantile), bins=10)

model_summary <- function(m, dat) {
    posterior_mu_estimates <- m %>% spread_draws(mu[n]) %>%
        group_by(n) %>%
        summarise(lower_95 = quantile(mu,0.025),
                  median = quantile(mu,0.5),
                  upper_95 = quantile(mu,0.975)) %>%
        mutate(variable = paste0("mu_",n)) %>%
        select(variable,lower_95,median,upper_95)
    
    posterior_beta_estimates <- m %>% spread_draws(beta[n]) %>%
        group_by(n) %>%
        summarise(lower_95 = quantile(beta,0.025),
                  median = quantile(beta,0.5),
                  upper_95 = quantile(beta,0.975)) %>%
        mutate(variable = paste0("beta_",n)) %>%
        select(variable,lower_95,median,upper_95)
    
    D_theta_hat <- dat %>% mutate(mu = posterior_mu_estimates$median[group],
                      beta = posterior_beta_estimates$median[group]) %>%
        mutate(log_density = log(mirror_gumbel_pdf(sg, mu, beta))) %>%
        summarise(D = -2*sum(log_density)) %>% pull(D)
    
    mu_draws <- m %>% select(matches("mu")) %>% as.matrix
    beta_draws <- m %>% select(matches("beta")) %>% as.matrix
    
    D_theta <- c()
    
    for(i in 1:nrow(m)) {
        if(i %% 100 == 0) print(i)
        D_theta[i] <- dat %>% mutate(mu = mu_draws[i,group],
                                              beta = beta_draws[i,group]) %>%
            mutate(log_density = log(mirror_gumbel_pdf(sg, mu, beta))) %>%
            summarise(D = -2*sum(log_density)) %>% pull(D)
    }

    D_theta_avg <- mean(D_theta)
    p_D <- D_theta_avg - D_theta_hat
    DIC <- D_theta_hat + 2*p_D
    
    list(posterior_w_estimates = posterior_mu_estimates,
         posterior_sigma_estimate = posterior_beta_estimates,
         D_theta_hat = D_theta_hat,
         D_theta_avg = D_theta_avg,
         p_D = p_D,
         DIC = DIC)
}

model_summary(gumbel_m, sg_dat)

progress_trend <- function(fitted_player) {
    sg_dat %>% filter(player == fitted_player) %>% 
        ggplot(aes(x=date, y=sg)) + 
            geom_line() +
            geom_smooth(stat='smooth',
                        method='loess',
                        formula='y ~ x',
                        color='steelblue',alpha=0.3,size=1) +
        xlab("Date") +
        ylab("Standardized shots gained") +
        labs(subtitle=paste("Progress trend for", fitted_player))
}

progress_trend("Jordan Spieth")
ggsave(here("final_project", "Figures", "progress_trend_spieth.pdf"),p, width=5, height=3, device="pdf")
progress_trend("Bryson DeChambeau")
ggsave(here("final_project", "Figures", "progress_trend_dechambeau.pdf"),p, width=5, height=3, device="pdf")

progress_trend("Dustin Johnson")
progress_trend("Patrick Reed")

posterior_mu_estimates %>% ggplot() + geom_histogram(aes(x=median), bins=30) +
    xlab(TeX("$\\mu$")) +
    ylab(TeX("Count")) +
    scale_y_continuous(expand=c(0,0))
ggsave(here("final_project", "Figures", "posterior_mu_medians.pdf"), width=5,height=3, device="pdf")

posterior_beta_estimates %>% ggplot() + geom_histogram(aes(x=median), bins=30) +
    xlab(TeX("$\\beta$")) +
    ylab(TeX("Count")) +
    scale_y_continuous(expand=c(0,0))
ggsave(here("final_project", "Figures", "posterior_beta_medians.pdf"), width=5,height=3, device="pdf")


player_estimates <- posterior_mu_estimates %>% rename(mu = median) %>%
    select(mu, n) %>%
    left_join(select(rename(posterior_beta_estimates, beta = median), beta, n)) %>%
    rename(group = n) %>%
    left_join(sg_dat, by="group") %>%
    group_by(player) %>%
    summarise(mu = max(mu),
              beta = max(beta))


