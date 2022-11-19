library(posterior)
library(tidyr)
library(readr)
library(ggplot2)

theme_set(theme_classic(base_size = 12) + theme(legend.position = "bottom"))

sg_dat <- read_csv(here("final_project", "Data", "sg_dat.csv"))
sg_dat_cutoff <- read_csv(here("final_project", "Data", "sg_dat_cutoff.csv"))
#golf_dat <- read_csv(here("final_project", "Data", "pga_raw_data.csv"))

player_model <- read_rds(here("final_project", "Models", "player_specific_model.rds"))
player_m <- player_model$draws() %>% as_draws_df
rm(player_model)

full_model <- read_rds(here("final_project", "Models", "player_specific_variance_model.rds"))
full_m <- full_model$draws() %>% as_draws_df
rm(full_model)

baseline_model <- read_rds(here("final_project", "Models", "baseline_model.rds"))
baseline_m <- baseline_model$draws() %>% as_draws_df
rm(baseline_model)

posterior_w_player_estimates <- player_m %>% spread_draws(w[n,k]) %>%
    group_by(n,k) %>%
    summarise(lower_95 = quantile(w,0.025),
              median = quantile(w,0.5),
              upper_95 = quantile(w,0.975)) %>%
    select(n,k,lower_95,median,upper_95) %>%
    ungroup()

posterior_sigma_player_estimates <- player_m %>% spread_draws(sigma2[n]) %>%
    mutate(sigma = sqrt(sigma2)) %>%
    group_by(n) %>%
    summarise(lower_95 = quantile(sigma,0.025),
              median = quantile(sigma,0.5),
              upper_95 = quantile(sigma,0.975)) %>%
    mutate(variable = paste0("sigma_",n)) %>%
    select(variable,lower_95,median,upper_95)

posterior_w_estimates <- full_m %>% spread_draws(w[n]) %>%
    group_by(n) %>%
    summarise(lower_95 = quantile(w,0.025),
              median = quantile(w,0.5),
              upper_95 = quantile(w,0.975)) %>%
    mutate(variable = paste0("w_",n)) %>%
    select(variable,lower_95,median,upper_95)

posterior_sigma_estimates <- full_m %>% spread_draws(sigma2[n]) %>%
    mutate(sigma = sqrt(sigma2)) %>%
    group_by(n) %>%
    summarise(lower_95 = quantile(sigma,0.025),
              median = quantile(sigma,0.5),
              upper_95 = quantile(sigma,0.975)) %>%
    mutate(variable = paste0("sigma_",n)) %>%
    select(variable,lower_95,median,upper_95)


# This matrix will be used to simulate tournaments and draw plots
sg_matrix <- full_m %>% select(matches("w\\[")) %>% as.matrix

sg_X <- sg_dat %>% select(paste0("sg_",1:K)) %>% as.matrix
sg_Y <- sg_dat$sg

predictions <- sg_dat %>%
    mutate(i = 1:n(),
           prediction = sg_X[i,] %*% posterior_w_estimates$median,
           standardized_residuals = (sg-prediction)/posterior_sigma_estimates$median[group])

predictions %>% filter(player == "Jordan Spieth") %>%
    ggplot(aes(x=date, y=sg)) + 
    geom_smooth(stat='smooth',
                method='loess',
                formula='y ~ x',
                color='steelblue',alpha=0.3,size=1) +
    geom_line() +
    geom_line(aes(x=date,y=prediction), color="red")

predictions %>% filter(player == "Tiger Woods") %>% 
    ggplot(aes(x=sg_2, y=sg)) + geom_point() +
    stat_summary()

normalqqplot <- function(x, nu){
    alpha <- 0.05
    n <- length(x)
    pplot <- (1:n)/(n + 1)
    plow <- qbeta(alpha/2,(1:n),n-(1:n)+1)
    pupp <- qbeta(1-alpha/2,(1:n),n-(1:n)+1)
    qqnorm <- qnorm(pplot)
    qx_low <- qnorm(plow)
    qx_upp <- qnorm(pupp)
    e_sort <- sort(x)
    index_e <- order(x)
    p <- ggplot(data=NULL,aes(x=qqnorm, y=e_sort)) +
        geom_point() +
        geom_line(y = qx_low, lty=2) +
        geom_line(y = qx_upp, lty=2) +
        geom_line(y = qqnorm) +
        xlab("Standard normal quantiles") +
        ylab("Standardized residuals") +
        coord_cartesian(xlim=c(-4,4), ylim=c(-7,7))
    p
}

normalqqplot(predictions$standardized_residuals)
ggplot() + geom_histogram(aes(x=predictions$standardized_residuals), bins=100) +
    scale_y_continuous(expand=c(0,0))

#### Calculating DIC

model_summary <- function(m, dat, constant_sigma=F) {
    posterior_w_estimates <- m %>% spread_draws(w[n]) %>%
        group_by(n) %>%
        summarise(lower_95 = quantile(w,0.025),
                  median = quantile(w,0.5),
                  upper_95 = quantile(w,0.975)) %>%
        mutate(variable = paste0("w_",n)) %>%
        select(variable,lower_95,median,upper_95)
    
    posterior_sigma_estimates <- tibble()
    if(constant_sigma) {
        posterior_sigma_estimates <- tibble(lower_95 = rep(quantile(m$sigma,0.025), length(unique(sg_dat$group))),
                                            median = rep(quantile(m$sigma,0.5), length(unique(sg_dat$group))),
                                            upper_95 = rep(quantile(m$sigma,0.975), length(unique(sg_dat$group))))
    } else {
        posterior_sigma_estimates <- m %>% spread_draws(sigma2[n]) %>%
            mutate(sigma = sqrt(sigma2)) %>%
            group_by(n) %>%
            summarise(lower_95 = quantile(sigma,0.025),
                      median = quantile(sigma,0.5),
                      upper_95 = quantile(sigma,0.975)) %>%
            mutate(variable = paste0("sigma_",n)) %>%
            select(variable,lower_95,median,upper_95)
    }
    D <- function(y, w, sigma) {
        sg_X <- y %>% select(paste0("sg_",1:K)) %>% as.matrix
        
        y %>% mutate(sigma_hat = sigma[group],
                     prediction = sg_X %*% w,
                     log_density = -log(sqrt(2*pi)*sigma[group])-(sg-prediction)^2/(2*sigma[group]^2)) %>%
              summarise(x=-2*sum(log_density)) %>% pull(x)
    }
    
    D_theta_hat <- D(dat, posterior_w_estimates$median, posterior_sigma_estimates$median)
    
    weights <- m %>% select(matches("w\\[")) %>% as.matrix
    sigmas <- c() 
    D_theta <- rep(0,nrow(m))
    if(constant_sigma) {
        for(i in 1:nrow(m)) {
            sigmas <- m$sigma 
        }
        
        for(i in 1:nrow(m)) {
            D_theta[i] <- D(dat, weights[i,], rep(sigmas[i],length(unique(dat$group))))
        }
    } else {
        sigmas <- m %>% select(matches("sigma2\\[")) %>% as.matrix %>% sqrt
        for(i in 1:nrow(m)) {
            D_theta[i] <- D(dat, weights[i,], sigmas[i,])
        }
    }
    
    D_theta_avg <- mean(D_theta)
    p_D <- D_theta_avg - D_theta_hat
    DIC <- D_theta_hat + 2*p_D
    
    list(posterior_w_estimates = posterior_w_estimates,
         posterior_sigma_estimate = posterior_sigma_estimates,
         D_theta_hat = D_theta_hat,
         D_theta_avg = D_theta_avg,
         p_D = p_D,
         DIC = DIC)
}

baseline_model_summary <- model_summary(baseline_m, sg_dat_cutoff, constant_sigma = T)
full_model_summary <- model_summary(full_m, sg_dat_cutoff, constant_sigma = F)

sim_US_open <- simulate_tournament("U.S. Open", 2021)
sim_3M_open <- simulate_tournament("3M Open", 2021)
