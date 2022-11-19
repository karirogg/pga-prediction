theme_set(theme_classic(base_size = 12) + theme(legend.position = "bottom"))

lognormal_model <- read_rds(here("final_project", "Models", "lognormal_model.rds"))
lognormal_m <- lognormal_model$draws() %>% as_draws_df
rm(lognormal_model)

posterior_mu_estimates <- lognormal_m %>% spread_draws(translated_mu[n]) %>%
    mutate(mu = -exp(translated_mu)+5) %>%
    group_by(n) %>%
    summarise(lower = quantile(mu, 0.025),
              median = quantile(mu,0.5),
              upper = quantile(mu,0.975))

posterior_sigma_estimates <- lognormal_m %>% spread_draws(sigma[n]) %>%
    group_by(n) %>%
    summarise(lower = quantile(sigma, 0.025),
              median = quantile(sigma,0.5),
              upper = quantile(sigma,0.975))


translated_lognormal <- function(y, mu,sigma) {
    log(dlnorm(y, meanlog=mu, sdlog=sigma))
}

dj_density <- tibble(t=seq(-4,4,0.01), 
                     density=translated_lognormal(t, 
                                           posterior_mu_estimates$median[112],
                                           posterior_sigma_estimates$median[112]))

sg_dat %>% filter(player == "Justin Thomas") %>%
    ggplot() + geom_density(aes(x=sg)) +
    geom_line(data=dj_density, aes(x=t,y=density), color="red")
