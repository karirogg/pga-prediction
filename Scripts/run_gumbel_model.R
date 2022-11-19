library(cmdstanr)
library(dplyr)
library(here)
library(readr)

run_gumbel_model <- function(warmup = 500, iters = 500, chains = 4) {
    set_cmdstan_path("~/software/cmdstan")
    sg_dat <- read_csv(here("final_project", "Data", "sg_dat_cutoff.csv"))

    Q <- length(unique(sg_dat$group))
    
    stan_data <- list(
        N = nrow(sg_dat),
        #Q = Q,
        y = sg_dat$sg#,
        #g = sg_dat$group,
        #t= sg_dat$t
    )
    
    mod <- cmdstan_model(here("final_project", "Models", paste0("baseline_gumbel",".stan")))
    
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
    
    fit$save_object(file = here("final_project", "Models", paste0("baseline_gumbel", ".rds")))
}

run_gumbel_model()
#run_golf_model("player_specific_variance_model", K = 5)

