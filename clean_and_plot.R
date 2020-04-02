
# libraries ---------------------------------------------------------------

pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(rvest)
pacman::p_load(stringdist)
pacman::p_load(gghighlight)
pacman::p_load(ggrepel)
pacman::p_load(wbstats, glue)

# Checks that data is current
system("git config --global user.name 'Jake'")
system("git pull https://github.com/nytimes/covid-19-data")

conflict_prefer("filter", "dplyr")
# Load data ---------------------------------------------------------------

county_df <- read.csv(here::here("us-counties.csv"))
state_df <- read.csv(here::here("us-states.csv")) %>% 
  mutate(date = as.Date(date))
state_df$state_abbr <- state.abb[match(state_df$state, state.name)]
state_df <- state_df %>% 
  mutate(state_abbr = case_when(is.na(state_abbr) ~ as.character(state),
                                TRUE ~ as.character(state_abbr)),
         state_abbr = case_when(state_abbr == "District of Columbia" ~ "DC",
                                TRUE ~ state_abbr)
  )

curve_df <- state_df %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  filter(cases >= 10) %>% 
  mutate(days = date - min(date))

# Create data frames for label layer. Days_elapsed and cases/deaths give (x,y) for
# labels
max_cases <- curve_df %>% 
  group_by(state) %>% 
  filter(cases == max(cases)) %>% 
  ungroup() %>% 
  mutate(days_elapsed = 1,
         cases = max(curve_df$cases))

max_deaths <- curve_df %>% 
  group_by(state) %>% 
  filter(deaths == max(deaths)) %>% 
  ungroup() %>% 
  mutate(days_elapsed = 1,
         deaths = max(curve_df$deaths))

# Get background curves
curve_bkgrd <- curve_df %>% 
  ungroup() %>% 
  select(-state)


# Plots -------------------------------------------------------------------
date <- as.Date(Sys.Date())
lab_notes <- glue(paste0(
  "Data as provided by NYTimes github: https://github.com/nytimes/covid-19-data\n",
  "Code based on code from Kieran Healy @kjhealy"
))


# State cases plot
state_cases_plot_log <- curve_df %>% 
  ggplot(mapping = aes(x = days, y = cases)) +
  geom_line(data = curve_bkgrd, aes(group = state_abbr),
            size = 0.15, colour = "grey80") +
  geom_line(color = "firebrick",
            lineend = "round") +
  geom_text(data = max_cases,
            mapping = aes(label = state, x = days_elapsed),
            vjust = "inward",
            hjust = "inward",
            fontface = "bold",
            color = "firebrick",
            size = 2.1) +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  facet_wrap(~state, ncol = 6) +
  labs(x = "Days since 10th confirmed case",
       y = "Cumulative number of cases (log10 scale)",
       title = "Cumulative Number of Reported Cases of COVID-19: US States and Territories",
       subtitle = paste("Data as of", format(max(state_df$date), "%A, %B %e, %Y")),
       caption = lab_notes) +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(0.7)),
        plot.caption = element_text(size = rel(0.7)),
        # turn off the strip label and tighten the panel spacing
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.spacing.y = unit(0.3, "lines"),
        axis.text.y = element_text(size = rel(0.6)),
        axis.title.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(0.6)),
        legend.text = element_text(size = rel(1)))
# Get on non-log scale
state_cases_plot <- state_cases_plot_log +
  scale_y_continuous(trans = "identity") +
  labs(y = "Cumulative number of cases")


# State deaths plot
state_deaths_plot_log <- curve_df %>% 
  ggplot(mapping = aes(x = days, y = deaths)) +
  geom_line(data = curve_bkgrd, aes(group = state_abbr),
            size = 0.15, colour = "grey80") +
  geom_line(color = "firebrick",
            lineend = "round") +
  geom_text(data = max_deaths,
            mapping = aes(label = state, x = days_elapsed),
            vjust = "inward",
            hjust = "inward",
            fontface = "bold",
            color = "firebrick",
            size = 2.1) +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  facet_wrap(~state, ncol = 6) +
  labs(x = "Days since 10th confirmed case",
       y = "Cumulative number of deaths (log10 scale)",
       title = "Cumulative Number of Reported Deaths of COVID-19: US States and Territories",
       subtitle = paste("Data as of", format(max(state_df$date), "%A, %B %e, %Y")),
       caption = lab_notes) +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(0.7)),
        plot.caption = element_text(size = rel(0.7)),
        # turn off the strip label and tighten the panel spacing
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.spacing.y = unit(0.3, "lines"),
        axis.text.y = element_text(size = rel(0.6)),
        axis.title.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(0.6)),
        legend.text = element_text(size = rel(1)))

state_deaths_plot <- state_deaths_plot_log +
  scale_y_continuous(trans = "identity") +
  labs(y = "Cumulative number of deaths")

# Save output
ggsave(plot = state_cases_plot_log,
       filename = here::here("state_cases_log_scale.png"),
       units = c("in"),
       height = 8,
       width = 12)

ggsave(plot = state_cases_plot,
       filename = here::here("state_cases_id_scale.png"),
       units = c("in"),
       height = 8,
       width = 12)

ggsave(plot = state_deaths_plot_log,
       filename = here::here("state_deaths_log_scale.png"),
       units = c("in"),
       height = 8,
       width = 12)

ggsave(plot = state_deaths_plot,
       filename = here::here("state_deaths_id_scale.png"),
       units = c("in"),
       height = 8,
       width = 12)
