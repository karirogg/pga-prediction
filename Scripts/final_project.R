library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(httr)

theme_set(theme_classic(base_size = 12) + theme(legend.position = "bottom"))

golf_dat <- read_csv(here("final_project", "pga_raw_data.csv"))
championship_dat <- read_csv(here("final_project", "championship_data.csv"))

golf_dat %>% group_by(course) %>% summarise(amount = n())

colnames(golf_dat)

call_id <- "7da97eb19601456eb913c0ee8a8dceee"
#call_url <- "https://api.sportsdata.io/golf/v2/json/Leaderboard/1"

call_url <- "https://api.sportsdata.io/golf/v2/json/Tournaments"
token_request <- GET(call_url, add_headers("Ocp-Apim-Subscription-Key" = call_id))
tournament_data <- content(token_request, as="parsed")

championship_data <- tibble()

for(i in 1:length(tournament_data)) {
    new_row <- list()
    for(name in names(tournament_data[[i]])) {
        if(name == "Rounds") break
        new_row[name] = tournament_data[[i]][name]
    }
    championship_data <- championship_data %>% bind_rows(new_row)
}

championship_data %>% write_csv("championship_data.csv")

golf_dat %>% group_by(player) %>% summarise(x=mean(pos, na.rm=T)) %>% arrange(x)

golfers_form <- function(name) {
    golf_dat %>% filter(player == name, !is.na(pos)) %>% 
        ggplot(aes(x=ymd(date), y = strokes-hole_par)) + 
        geom_line() + 
        geom_smooth(stat='smooth',
                    method='loess',
                    formula='y ~ x',
                    color='steelblue',alpha=0.3,size=1) +
        scale_x_date(expand=c(0,0)) +
        ylab("Strokes relative to par") +
        xlab("Date") +
        geom_hline(yintercept=0, lty = 2) +
        labs(title=element_blank(), subtitle=paste("Form of", name))
}

p <- golfers_form("Jordan Spieth")
ggsave(here("final_project", "Figures", "spieth_form.png"), p, device = "png", width=5, height=3)

p <- golfers_form("Tiger Woods")
ggsave(here("final_project", "Figures", "woods_form.png"), p, device = "png", width=5, height=3)

golfers_form("Phil Mickelson")

p <- golfers_form("Patrick Cantlay")
ggsave(here("final_project", "Figures", "cantlay_form.png"), p, device = "png", width=5, height=3)

golfers_form("Bryson DeChambeau")
golfers_form("Dustin Johnson")

p <- golfers_form("Patrick Reed")
ggsave(here("final_project", "Figures", "reed_form.png"), p, device = "png", width=5, height=3)

# datagolf_api_token <- "2c2fdc079c26000b862a1bae6aa5"
# 
# tournament_data_http <- "https://feeds.datagolf.com/historical-raw-data/event-list?file_format=csv&key=2c2fdc079c26000b862a1bae6aa5"
# token_request <- GET(tournament_data_http)
# event_data_raw <- content(token_request, as="parsed")
# 
# event_data <- tibble()
# 
# for(i in 1:length(event_data_raw)) {
#     event_data <- event_data %>% bind_rows(data.frame(event_data_raw[[i]]))
# }
# 
# pga_event_data <- event_data %>% filter(tour == "pga")
# 
# get_event_data <- function(tour, event_id, year) {
#     fetch_link <- paste0("https://feeds.datagolf.com/historical-raw-data/rounds?tour=", tour ,"&event_id=", event_id, "&year=", year,"&file_format=csv&key=2c2fdc079c26000b862a1bae6aa5")
#     rounds_token_request <- GET(fetch_link)
#     rounds_data <- content(rounds_token_request, as="parsed")
#     rounds_data
# }
