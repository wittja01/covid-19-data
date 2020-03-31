
# libraries ---------------------------------------------------------------

pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(rvest)
pacman::p_load(stringdist)
pacman::p_load(gghighlight)
pacman::p_load(ggrepel)
pacman::p_load(wbstats, glue)

# Checks that data is current
system("git pull")

conflict_prefer("filter", "dplyr")
# Load data ---------------------------------------------------------------

county_df <- read.csv(here::here("us-counties.csv"))
state_df <- read.csv(here::here("us-states.csv")) %>% 
  mutate(date = as.Date(date))


# Plots -------------------------------------------------------------------
date <- as.Date(Sys.Date())
lab_notes <- glue(paste0(
  "Data as provided by NYTimes github: https://github.com/nytimes/covid-19-data\n",
  "Updated as of {date}"
))


gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    #plot.title.position = "plot", 
    #plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
  ),
  labs(caption = lab_notes),
  gghighlight(TRUE,  label_key = state, use_direct_label = TRUE,
              max_highlight = 10,
              label_params = list(segment.color = NA, nudge_x = 1))
)

# Filter state data frame and adjust to compare states once they have, say 10 confirmed cases
low_case_df <- state_df %>% 
  group_by(state) %>% 
  filter(cases >= 10) %>% 
  mutate(days = date - min(date))

low_case_df %>% 
  ggplot(aes(x = days, y = deaths, colour = state)) +
  geom_line() +
  gg_my_blob +
  facet_wrap(~state)

low_case_df %>% 
  ggplot(aes(x = days, y = cases, colour = state)) +
  geom_line() +
  gg_my_blob +
  facet_wrap(~state)
