library(readr)
library(dplyr)
library(here)
library(tidyr)
library(lubridate)

# It is quite unnatural that the made cut rate is above 80%
golf_dat <- read_csv(here("final_project", "Data" , "pga_raw_data.csv")) %>%
    group_by(`tournament name`, date) %>% filter(mean(made_cut) <= 0.8)

K <- 10

# Make sure at least 10 data points per player 
sg_dat <- golf_dat %>%
    group_by(player) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    rename(tournament = "tournament name") %>%
    group_by(tournament, date) %>%
    mutate(average_strokes = strokes/n_rounds,
           mean_strokes = mean(average_strokes),
           sg = (mean(average_strokes)-average_strokes)/sd(average_strokes)) %>% 
    filter(!is.na(sg)) %>%
    ungroup %>% 
    arrange(date)

#for(i in 1:K) sg_dat <- sg_dat %>% mutate(!!paste0("sg_",i) := c(rep(NA,i), sg[1:(n()-i)]))

cutoff_date <- sg_dat %>% ungroup %>% 
    filter(tournament == "U.S. Open", year(date) == 2021) %>%
    slice(1) %>% pull(date)

cutoff_players <- filter(sg_dat, date < cutoff_date) %>% pull(player) %>% unique

sg_dat <- sg_dat %>% group_by(player) %>%
    #filter(!is.na(.data[[paste0("sg_",K)]])) %>%
    select(player, tournament, date, pos, sg) %>% #, paste0("sg_",1:K)) %>% 
    filter(player %in% cutoff_players) %>%
    mutate(group = group_indices()) %>%
    mutate(t_star = as.numeric(date-min(date)),
           t = 2*(t_star-(max(t_star)-min(t_star))/2)/(max(t_star)-min(t_star))) %>%
    select(c(-t_star)) %>%
    ungroup

sg_dat_cutoff <- sg_dat %>% filter(date < cutoff_date)
    
sg_dat_cutoff %>% write_csv(here("final_project", "Data", "sg_dat_cutoff.csv"))
sg_dat %>% write_csv(here("final_project", "Data", "sg_dat.csv"))


sg_dat %>% group_by(tournament, date) %>% summarise(x=sd(sg)) %>% ungroup() %>%
    summarise(lower_95 = quantile(x,0.025),
              median = quantile(x,0.5), 
              upper_95 = quantile(x,0.975))# %>% 
    #ggplot() + geom_histogram(aes(x=x), bins=100)

tmp <- sg_dat %>% filter(tournament == "3M Open", year(date) == 2021)
ggplot(tmp) + geom_histogram(aes(x=sg), bins=20) + geom_vline(xintercept=mean(tmp$sg), color='red')

sg_dat %>% filter(tournament == "3M Open", year(date) == 2021) %>%
    summarise(x=mean(sg))

running_average <- function(x, dates) {
    out = c()
    for(i in 1:length(x)) {
        out[i] = mean(x[(i-length(dates[dates >= dates[i] %m-% months(12) & dates < dates[i]])):i])
    }
    out
}

time_sg_dat <- sg_dat %>%
    mutate(t_star = as.numeric(date-min(date)),
           t = 2*(t_star-(max(t_star)-min(t_star))/2)/(max(t_star)-min(t_star)))

running_average_dat <- sg_dat %>% group_by(player) %>% 
    mutate(running_average = running_average(sg, date))

time_sg_dat %>% write_csv(here("final_project", "Data", "time_sg_dat.csv"))

running_average_dat %>% filter(player == "Jordan Spieth") %>%
    ggplot() + geom_line(aes(x=date,y=sg)) +
    geom_line(aes(x=date,y=running_average), color="red")
