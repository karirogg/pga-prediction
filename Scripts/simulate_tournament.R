library(tidyr)

simulate_tournament <- function(tournament_name, year, K=5) {
    tournament_dat <- read_csv(here("final_project", "Data", "sg_dat.csv")) %>%
        filter(tournament == tournament_name, year(date) == year)
    
    mod <- read_rds(here("final_project", "Models", "player_specific_variance_model.rds"))
    m <- mod$draws() %>% as_draws_df
    rm(mod)
    
    weight_draws <- m %>% 
        select(matches("w\\[")) %>% 
        as.matrix

    sigma_draws <- m %>% 
        spread_draws(sigma2[group]) %>%
        mutate(sigma = sqrt(sigma2))
        
    sg_X <- tournament_dat %>% select(paste0("sg_",1:K)) %>% as.matrix
    rownames(sg_X) = tournament_dat$player
    
    cut <- max(tournament_dat$pos, na.rm=T)
    
    player_fits <- weight_draws %*% t(sg_X) %>% as_tibble %>%
        mutate(.draw = 1:n()) %>%
        pivot_longer(c(-.draw), names_to="player", values_to="sg_hat") %>%
        left_join(select(tournament_dat, player, group), by="player") %>%
        left_join(select(sigma_draws, group, sigma, .draw), by=c("group", ".draw")) %>%
        left_join(select(tournament_dat, player, pos, sg), by="player") %>%
        mutate(lower_95 = sg_hat - qnorm(0.975)*sigma,
               upper_95 = sg_hat + qnorm(0.975)*sigma,
               uniform_dist = ifelse(sg < lower_95, 0, ifelse(sg > upper_95, 1, (sg-lower_95)/(upper_95-lower_95))))
    
    player_fits %>%
        mutate(predicted_sg = rnorm(n(), mean=sg_hat, sd = sigma)) %>%
        group_by(.draw) %>%
        arrange(desc(predicted_sg)) %>%
        mutate(predicted_pos = 1:n()) %>%
        ungroup %>%
        arrange(.draw, predicted_pos) %>%
        group_by(player) %>%
        summarise(win_prob = mean(predicted_pos == 1),
                  top_10_prob = mean(predicted_pos <= 10),
                  made_cut_prob = mean(predicted_pos <= cut),
                  predicted_sg = mean(predicted_sg)) %>%
        ungroup %>%
        arrange(desc(win_prob)) %>%
        mutate(predicted_pos = c(1:cut, rep(NA, n()-cut))) %>%
        left_join(select(tournament_dat, player, pos, sg)) %>%
        arrange(pos)
}
