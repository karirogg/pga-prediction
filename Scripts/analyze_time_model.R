library(posterior)
library(tidyr)
library(readr)
library(dplyr)
library(here)
library(lubridate)
library(rstan)
library(tidybayes)
library(latex2exp)

source(here("final_project", "Scripts", "simulate_tournament_gumbel.R"))
theme_set(theme_classic(base_size = 12) + theme(legend.position = "bottom"))

gumbel_time_model <- read_rds(here("final_project", "Models", "gumbel_time_model.rds"))
gumbel_time_m <- gumbel_time_model$draws() %>% as_draws_df
rm(gumbel_time_model)

posterior_alpha_estimates <- gumbel_time_m %>% spread_draws(alpha[Q,i]) %>%
    group_by(Q,i) %>%
    summarise(lower_95 = quantile(alpha, 0.025),
              median = quantile(alpha,0.5),
              upper_95 = quantile(alpha,0.975)) %>%
    select(Q,i,median) %>%
    pivot_wider(names_from = i, 
                names_prefix = "alpha_",
                values_from = median) %>%
    rename(group = Q) %>%
    ungroup

posterior_beta_estimates <- gumbel_time_m %>% spread_draws(log_beta[group]) %>%
    mutate(beta = exp(log_beta)) %>%
    group_by(group) %>%
    summarise(beta = quantile(beta, 0.5))%>%
    ungroup

hyperparameter_estimates <- gumbel_time_m %>% select(matches("mu_alpha"), matches("sigma2_alpha"), mu_beta, sigma2_beta) %>%
    rename(sigma_alpha_1 = "sigma2_alpha[1]",
           sigma_alpha_2 = "sigma2_alpha[2]",
           sigma_alpha_3 = "sigma2_alpha[3]",
           sigma_alpha_4 = "sigma2_alpha[4]",
           mu_alpha_1 = "mu_alpha[1]",
           mu_alpha_2 = "mu_alpha[2]",
           mu_alpha_3 = "mu_alpha[3]",
           mu_alpha_4 = "mu_alpha[4]",
           sigma_beta = sigma2_beta) %>%
    as_tibble() %>%
    mutate(sigma_alpha_1 = sqrt(sigma_alpha_1),
           sigma_alpha_2 = sqrt(sigma_alpha_2),
           sigma_alpha_3 = sqrt(sigma_alpha_3),
           sigma_alpha_4 = sqrt(sigma_alpha_4),
           sigma_beta = sqrt(sigma_beta)) %>%
    mutate(draw = 1:n()) %>%
    pivot_longer(c(-draw), names_to="parameter") %>%
    group_by(parameter) %>%
    summarise(lower_95 = quantile(value, 0.025),
              median = quantile(value,0.5),
              upper_95 = quantile(value,0.975)) %>%
    slice(c(1,6,2,7,3,8,4,9,5,10)) %>%
    mutate(parameter = paste0("$\\",parameter, "}$"),
           parameter = gsub("alpha", "{\\\\alpha", parameter),
           parameter = gsub("beta", "{\\\\beta", parameter)) %>%
    rename(Parameter = parameter,
           "Lower 95\\%" = lower_95,
           "Posterior median" = median, 
           "Upper 95\\%" = upper_95)

kable(hyperparameter_estimates)

alpha_draws <- gumbel_time_m %>% spread_draws(alpha[group,i]) %>%
    pivot_wider(names_from = i, 
                names_prefix = "alpha_",
                values_from = alpha) %>%
    ungroup %>%
    select(c(-.chain, -.iteration))
beta_draws <- gumbel_time_m %>% spread_draws(log_beta[group]) %>%
    mutate(beta = exp(log_beta)) %>%
    select(c(-.chain,-.iteration,-log_beta))

parameter_draws <- left_join(alpha_draws, beta_draws, by=c("group",".draw"))
    
sg_dat <- read_csv(here("final_project", "Data", "sg_dat.csv"))
sg_dat_cutoff <- read_csv(here("final_project", "Data", "sg_dat_cutoff.csv"))

mirror_gumbel_pdf <- function(y, mu,beta) {
    z = (mu-y)/beta
    1/beta*exp(-z-exp(-z))
}

mirror_gumbel_cdf <- function(y, mu, beta) {
    z = (mu-y)/beta
    exp(-exp(-z))
}

inverse_mirror_gumbel <- function(p, mu, beta) {
    beta*log(-log(p))+mu
}

player_predictions <- sg_dat_cutoff %>%
    left_join(posterior_alpha_estimates, by=c("group")) %>%
    left_join(posterior_beta_estimates, by=c("group")) %>%
    mutate(mu = alpha_1 + alpha_2*t + alpha_3*t^2 + alpha_4*t^3,
           predicted = mu+beta*log(log(2)),
           quantile = mirror_gumbel_cdf(sg, mu, beta),
           residuals = sg-predicted)

bins <- 50
p <- player_predictions %>% ggplot() + 
    geom_histogram(aes(x=quantile), bins=bins) +
    geom_hline(yintercept = nrow(player_predictions)/bins, color='red') +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    theme(plot.margin=unit(c(0.2,0.5,0.2,0.2),"cm"))
ggsave(here("final_project", "Report", "Figures", "quantile_plot.pdf"),p, width=5, height=3, device="pdf")

dens <- function(fitted_player, show_fit = T) {
    index <- sg_dat %>% filter(player == fitted_player) %>% pull(group) %>% unique()
    player_density <- tibble(t=seq(-4,4,0.01), 
                             density=mirror_gumbel_pdf(t, 
                                                       -log(log(2)),
                                                       posterior_beta_estimates$beta[index]))
    p <- player_predictions %>% filter(player == fitted_player) %>%
        ggplot() + coord_cartesian(xlim=c(-4,4)) +
        geom_density(aes(x=residuals)) +
        xlab("Residuals") +
        ylab("Density")
    
    if(show_fit) p <- p+geom_line(data=player_density, aes(x=t,y=density), color="red")
    p
}

dens("Justin Thomas")
ggsave(here("final_project", "Report", "Figures", "posterior_predictive_thomas.pdf"), width=5,height=3, device="pdf")
dens("Jon Rahm")
ggsave(here("final_project", "Report", "Figures", "posterior_predictive_rahm.pdf"), width=5,height=3, device="pdf")

dens("Jon Rahm", show_fit = F)
ggsave(here("final_project", "Report", "Figures", "rahm_empirical_density.pdf"), width=5, height=3, device="pdf")

dens("Jordan Spieth", show_fit = F)
ggsave(here("final_project", "Report", "Figures", "empirical_density_spieth.pdf"), width=5, height=3, device="pdf")

dens("Dustin Johnson")
dens("Jordan Spieth")
ggsave(here("final_project", "Report", "Figures", "posterior_predictive_spieth.pdf"), width=5, height=3, device="pdf")

dens("Bryson DeChambeau")
ggsave(here("final_project", "Report", "Figures", "posterior_predictive_dechambeau.pdf"), width=5,height=3, device="pdf")
dens("Sandy Lyle")

player_predictions %>% filter(player == "Justin Thomas") %>% 
    ggplot(aes(x=residuals)) + geom_density() +
    geom_line()

p <- player_predictions %>% filter(player == "Jordan Spieth") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) + 
    geom_line(aes(x=date,y=predicted), color='red')
ggsave(here("final_project", "Report", "Figures", "spieth_fitted_form.pdf"), p, width=5, height=3, device="pdf")

p <- player_predictions %>% filter(player == "Bryson DeChambeau") %>%
    ggplot() + geom_line(aes(x=date,y=sg))# + 
    #geom_line(aes(x=date,y=predicted), color='red')
ggsave(here("final_project", "Report", "Figures", "dechambeau_sg.pdf"), p, width=5, height=3, device="pdf")

ggsave(here("final_project", "Report", "Figures", "dechambeau_fitted_form.pdf"), p, width=5, height=3, device="pdf")

player_predictions %>% filter(player == "Justin Thomas") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) + 
    geom_line(aes(x=date,y=predicted), color='red') #+ 
    #geom_smooth(aes(x=date,y=sg), stat='smooth', method='loess', formula='y~x')

p <- player_predictions %>% filter(player == "Jon Rahm") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) + 
    geom_line(aes(x=date,y=predicted), color='red')
#ggsave(here("final_project", "Report", "Figures", "rahm_sg.pdf"), p, width=5, height=3, device="pdf")

ggsave(here("final_project", "Report", "Figures", "rahm_fitted_form.pdf"), p, width=5, height=3, device="pdf")

player_predictions %>% filter(player == "Patrick Cantlay") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) + 
    geom_line(aes(x=date,y=predicted), color='red')
    #geom_smooth(aes(x=date,y=sg), stat='smooth', method='loess', formula='y~x')

player_predictions %>% filter(player == "Sandy Lyle") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) + 
    geom_line(aes(x=date,y=predicted), color='red') #+ 
    #geom_smooth(aes(x=date,y=sg), stat='smooth', method='loess', formula='y~x')

mirror_gumbel_pdf <- function(y, mu,beta) {
    z = (mu-y)/beta
    1/beta*exp(-z-exp(-z))
}

mirror_gumbel_cdf <- function(y, mu, beta) {
    z = (mu-y)/beta
    exp(-exp(-z))
}

model_summary <- function(m, dat) {
    posterior_alpha_estimates <- m %>% spread_draws(alpha[Q,i]) %>%
        group_by(Q,i) %>%
        summarise(lower_95 = quantile(alpha, 0.025),
                  median = quantile(alpha,0.5),
                  upper_95 = quantile(alpha,0.975)) %>%
        select(Q,i,median) %>%
        pivot_wider(names_from = i, 
                    names_prefix = "alpha_",
                    values_from = median) %>%
        rename(group = Q) %>%
        ungroup
    
    posterior_beta_estimates <- m %>% spread_draws(beta[n]) %>%
        group_by(n) %>%
        summarise(beta = quantile(beta,0.5)) %>%
        rename(group = n)
    
    D_theta_hat <- dat %>% left_join(posterior_alpha_estimates, by="group") %>%
        left_join(posterior_beta_estimates, by="group") %>%
        mutate(log_density = log(mirror_gumbel_pdf(sg, alpha_1 + t*alpha_2 + t^2*alpha_3 + t^3*alpha_4, beta))) %>%
        summarise(D = -2*sum(log_density)) %>% pull(D)
    
    alpha_draws <- m %>% spread_draws(alpha[Q,i]) %>%
        pivot_wider(names_from = i, 
                    names_prefix = "alpha_",
                    values_from = alpha) %>%
        ungroup
    beta_draws <- m %>% select(matches("beta")) %>% as.matrix
    
    D_theta <- c()
    
    for(i in 1:nrow(m)) {
        if(i %% 100 == 0) print(i)
        current_alpha_draw <- alpha_draws %>% filter(.draw == i)
        D_theta[i] <- dat %>% mutate(alpha_1 = current_alpha_draw$alpha_1[group],
                                     alpha_2 = current_alpha_draw$alpha_2[group],
                                     alpha_3 = current_alpha_draw$alpha_3[group],
                                     alpha_4 = current_alpha_draw$alpha_4[group],
                                     beta = beta_draws[i,group]) %>%
            mutate(log_density = log(mirror_gumbel_pdf(sg, alpha_1 + t*alpha_2 + t^2*alpha_3 + t^3*alpha_4, beta))) %>%
            summarise(D = -2*sum(log_density)) %>% pull(D)
    }
    
    D_theta_avg <- mean(D_theta)
    p_D <- D_theta_avg - D_theta_hat
    DIC <- D_theta_hat + 2*p_D
    
    list(posterior_alpha_estimates = posterior_alpha_estimates,
         posterior_beta_estimate = posterior_beta_estimates,
         D_theta_hat = D_theta_hat,
         D_theta_avg = D_theta_avg,
         p_D = p_D,
         DIC = DIC)
}

model_summary(gumbel_time_m, sg_dat_cutoff)

cutoff_date <- max(sg_dat_cutoff$date)

tournaments_left <- unique(filter(sg_dat, date > cutoff_date)$tournament)

tournament_simulations <- tibble()

for(tournament in tournaments_left) {
    tournament_simulations <- tournament_simulations %>% bind_rows(simulate_tournament(tournament, 2021))
}

tournament_simulations %>% write_csv(here("final_project", "Data", "tournament_simulations.csv"))

tournament_simulations <- read_csv(here("final_project", "Data", "tournament_simulations.csv"))

model_performance <- tournament_simulations %>% group_by(tournament) %>%
    summarise(correct_winner = sum(pos == 1 & predicted_pos == 1, na.rm=T),
              correct_top_10 = sum(pos <= 10 & predicted_pos <= 10, na.rm=T)/10,
              prop_top_10 = 10/n(),
              correct_top_25 = sum(pos <= 25 & predicted_pos <= 25, na.rm=T)/25,
              prop_25 = 25/n(),
              correct_made_cut = sum(!is.na(pos) & !is.na(predicted_pos))/max(pos, na.rm=T),
              prop_cut = max(pos,na.rm=T)/n())


## Chi squared test

thomas_predictions <- player_predictions %>% filter(player == "Jordan Spieth")

thomas_predictions %>% ggplot(aes(x=quantile)) + geom_histogram(bins=10)

bins <- 10
expected <- nrow(thomas_predictions)/bins

x2 <- 0
for(i in 1:bins) {
    amount <- thomas_predictions %>% filter(quantile >= (i-1)/bins, quantile < i/bins) %>% nrow()
    print(amount)
    x2 <- x2 + (amount-expected)^2/expected
}

if(x2 > qchisq(0.975, bins-1)) print(paste("Failed, chisq=",x2, "qchisq=",qchisq(0.975,bins)))

anderson_darling <- function(y, mu, beta) {
    w = c(mirror_gumbel_cdf(y,mu,beta))
    sort(w)
    -length(y) - mean((2*(1:length(y))-1)*(log(w) + log(1-rev(w))))
}
L <- 2000

discrepancy <- tibble()

for(i in 1:L) {
    if(i %% 100 == 0) print(i)
    alpha_draw <- alpha_draws %>% filter(.draw == i) %>% select(group, matches("alpha"))
    beta_draw <- beta_draws %>% filter(.draw == i) %>% ungroup %>% select(group, beta)
    
    draw_predictions <- sg_dat_cutoff %>% left_join(alpha_draw, by="group") %>%
        left_join(beta_draw, by="group") %>%
        mutate(mu = alpha_1 + alpha_2*t + alpha_3*t^2 + alpha_4*t^4,
               y_rep = inverse_mirror_gumbel(runif(n()), mu, beta)) %>%
        group_by(player) %>%
        summarise(discrepancy = anderson_darling(y_rep, mu, beta) >= anderson_darling(sg, mu, beta))
    
    discrepancy <- discrepancy %>% bind_rows(draw_predictions)
}

p_values <- discrepancy %>% group_by(player) %>% summarise(p_value = mean(discrepancy))

p_values %>% ggplot() + geom_histogram(aes(x=p_value), bins=30) +
    xlab("p-value") +
    ylab("Count") +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0), limits=c(0,1)) + 
    theme(plot.margin=unit(c(0.2,0.5,0.2,0.2),"cm"))

ggsave(here("final_project", "Report", "Figures", "p_values.pdf"), width=5, height=3, device="pdf")

                                   