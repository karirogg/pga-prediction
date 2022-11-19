run_lognormal_model <- function(warmup = 500, iters = 500, chains = 4) {
    set_cmdstan_path("~/software/cmdstan")
    sg_dat <- read_csv(here("final_project", "Data", "sg_dat_cutoff.csv")) %>%
        mutate(negative_sg = -sg)
    
    g <- sg_dat %>% pull(group)
    Q <- length(unique(sg_dat$group))
    
    stan_data <- list(
        N = nrow(sg_dat),
        Q = Q,
        y = -sg_dat$sg+5,
        g = g
    )
    
    mod <- cmdstan_model(here("final_project", "Models", paste0("lognormal_model",".stan")))
    
    fit <- mod$sample(
        data = stan_data, 
        show_messages = FALSE, 
        chains = chains, 
        parallel_chains = chains,
        iter_sampling = iters,
        iter_warmup = warmup,
        max_treedepth = 15,
        init = 0,
        refresh = 10
    )
    
    fit$save_object(file = here("final_project", "Models", paste0("lognormal_model", ".rds")))
}

run_lognormal_model()
#run_golf_model("player_specific_variance_model", K = 5)

