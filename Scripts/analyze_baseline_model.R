
model <- read_rds(here("final_project", "Models", "baseline_gumbel.rds"))
m <- model$draws() %>% as_draws_df
rm(model)

mu_median <- median(m$mu)
beta_median <- exp(median(m$beta))

D_theta_hat <- sg_dat_cutoff %>% mutate(log_likelihood = 
                                            log(mirror_gumbel_pdf(sg, mu_median, beta_median))) %>%
    summarise(D_theta = -2*sum(log_likelihood)) %>% pull(D_theta)

D_theta <- c()

for(i in 1:nrow(m)) {
    D_theta[i] <- sg_dat_cutoff %>% mutate(log_likelihood = 
                                               log(mirror_gumbel_pdf(sg, m$mu[i], exp(m$beta[i])))) %>%
        summarise(D_theta = -2*sum(log_likelihood)) %>% pull(D_theta) %>% as.numeric
}

D_theta_avg <- mean(D_theta)
p_D <- D_theta_avg-D_theta_hat

DIC <- D_theta_hat + 2*p_D

sg_dat_cutoff %>% ggplot(aes(x=sg)) + geom_density() +
    geom_line(data=tibble(t=seq(-6,3,0.1), 
                          density = mirror_gumbel_pdf(t, mu_median,beta_median)), 
              aes(x=t,y=density), color='red')
