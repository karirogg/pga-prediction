inv_gumbel <- function(x, mu,beta) {
    mu + beta*log(-log(x))
}

simulate_tournament <- function(tournament_name, year) {
    tournament_dat <- read_csv(here("final_project", "Data", "sg_dat.csv")) %>%
        filter(tournament == tournament_name, year(date) == year)
    
    mod <- read_rds(here("final_project", "Models", "gumbel_time_model.rds"))
    m <- mod$draws() %>% as_draws_df
    rm(mod)
    
    alpha_draws <- m %>% spread_draws(alpha[Q,i]) %>%
        pivot_wider(names_from = i, 
                    names_prefix = "alpha_",
                    values_from = alpha) %>%
        ungroup %>%
        rename(group = Q) %>%
        select(group, matches("alpha"), .draw)
    beta_draws <- m %>% spread_draws(log_beta[n]) %>%
        mutate(beta = exp(log_beta)) %>%
        rename(group = n) %>%
        select(group,beta, .draw)
    
    parameter_draws <- left_join(alpha_draws, beta_draws, by=c("group", ".draw"))
    
    cut <- length(tournament_dat$pos[!is.na(tournament_dat$pos)])
    
    tournament_dat %>%
        left_join(parameter_draws, by="group") %>%
        mutate(rng = runif(n()),
               mu = alpha_1 + t*alpha_2 + t^2*alpha_3 + t^3*alpha_4,
               predicted = inv_gumbel(rng, mu, beta),
               uniform_dist = (sg-inv_gumbel(0.025,mu,beta))/(inv_gumbel(0.975, mu,beta)-inv_gumbel(0.025,mu,beta))) %>%
        group_by(.draw) %>%
        arrange(desc(predicted)) %>%
        mutate(predicted_pos = 1:n()) %>%
        ungroup() %>%
        group_by(player) %>%
        summarise(win_prob = mean(predicted_pos == 1),
                  top_10_prob = mean(predicted_pos <= 10),
                  top_25_prob = mean(predicted_pos <= 25),
                  made_cut_prob = mean(predicted_pos <= cut),
                  predicted_sg = mean(mu)) %>%
        left_join(tournament_dat, by="player") %>%
        ungroup() %>%
        arrange(desc(win_prob)) %>%
        mutate(predicted_pos = c(1:cut, rep(NA, n()-cut))) %>%
        arrange(predicted_pos) %>% 
        select(player, win_prob, top_10_prob, top_25_prob, made_cut_prob, tournament, pos, predicted_pos, sg, predicted_sg)
}

simulate_uniform <- function(tournament_name, year) {
    tournament_dat <- read_csv(here("final_project", "Data", "sg_dat.csv")) %>%
        filter(tournament == tournament_name, year(date) == year)
    
    total_winners <- 0
    total_top_10 <- 0
    total_top_25 <- 0
    total_made_cut <- 0
    
    cut <- length(tournament_dat$pos[!is.na(tournament_dat$pos)])
    
    L <- 2000
    for(i in 1:L) {
        simulation <- tournament_dat %>% slice(sample(n())) %>% mutate(predicted_pos = 1:n())
        total_winners <- total_winners + nrow(filter(simulation, pos == 1, predicted_pos == 1))
        total_top_10 <- total_top_10 + nrow(filter(simulation, pos <= 10, predicted_pos <= 10))
        total_top_25 <- total_top_25 + nrow(filter(simulation, pos <= 25, predicted_pos <= 25))
        total_made_cut <- total_made_cut + nrow(filter(simulation, pos <= cut, predicted_pos <= cut))
    }
    
    tibble(prop_winners = total_winners/L,
         prop_top_10 = total_top_10/(10*L),
         prop_top_25 = total_top_25/(25*L),
         prop_made_cut = total_made_cut/(cut*L))
}


