## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(lubridate)
library(scales)
library(viridis)
library(gganimate)
library(transformr)
library(gifski)

# Next, set the plotting theme
theme_clean <- theme_bw(base_family = "Gill Sans Nova") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean)

## Drawing the data
# We draw the data straight from the online file
# First, set some parameters
ukhsa_deaths_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByPublishDate&metric=newDeaths28DaysByDeathDate&format=csv"
ukhsa_caption <- "Source: UK Health Security Agency: Coronavirus (Covid-19) in the UK dashboard - Data download."
ukhsa_last_date <- as_date("2022-01-16")

# Next, download the file
temp <- tempfile()
temp <- curl_download(url = ukhsa_deaths_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

# Calculate extra columns for the week end (Sunday) date and weekday
ukhsa_england_df <- read_csv(temp) %>%
  dplyr::filter(date <= ukhsa_last_date,
                date >= as_date("2020-03-09")) %>%
  dplyr::mutate(week_end_date = ceiling_date(date, unit = "week",
                                             change_on_boundary = FALSE),
                day_of_week = wday(date, label = TRUE, abbr = FALSE))

## Making the graphs
# First, we make the static graphs
ukhsa_date_breaks <- seq(as_date("2020-04-05"), as_date("2022-01-02"),
                                 "13 weeks")

ukhsa_england_gg <- ukhsa_england_df %>%
  ggplot(aes(x = week_end_date, y = newDeaths28DaysByPublishDate,
             group = day_of_week)) +
  geom_line(aes(colour = day_of_week), size = 1.5) +
  scale_colour_viridis(name = "Week day",
                       discrete = TRUE) +
  scale_x_date(breaks = ukhsa_date_breaks,
               date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0)) +
  labs(title = "There is a clear reporting cycle in Covid-19 surveillance deaths in England.",
       subtitle = "Deaths within 28 day of a positive test result, by reporting date in England.",
       x = "Reporting week: week end date (Sunday)",
       y = "Deaths within 28 days of a positive test",
       caption = ukhsa_caption)

# The second graph shows deaths by date of occurrence
ukhsa_date_death_gg <- ukhsa_england_df %>%
  ggplot(aes(x = date, y = newDeaths28DaysByDeathDate)) +
  geom_line(colour = "#008080", size = 1.5) +
  scale_x_date(date_breaks = "13 weeks",
               date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0)) +
  labs(title = "Deaths by date of occurrence reflect a more accurate picture than reporting dates.",
       subtitle = str_wrap("Deaths within 28 days of a positive test in England, by date of death. These statistics are subject to revision, when more deaths are reported."),
       x = "Date of death",
       y = "Covid-19 surveillance deaths",
       caption = ukhsa_caption)

# Next, we make the animated graph
ukhsa_england_agg <- ukhsa_england_df %>%
  ggplot(aes(x = week_end_date, y = newDeaths28DaysByPublishDate,
             group = day_of_week)) +
  geom_line(aes(colour = day_of_week), size = 1.5) +
  scale_colour_viridis(discrete = TRUE, guide = "none") +
  scale_x_date(breaks = ukhsa_date_breaks,
               date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0)) +
  geom_text(x = as_date("2020-04-01"), y = 1600,
            aes(label = day_of_week, colour = day_of_week),
            size = 9, fontface = "bold", hjust = 0) +
  labs(title = "There is a clear reporting cycle in Covid-19 surveillance deaths in England.",
       subtitle = "Deaths within 28 days of a positive test result, by reporting date in England.",
       x = "Reporting week: week end date (Sunday)",
       y = "Deaths within 28 days of a positive test",
       caption = ukhsa_caption) +
  transition_states(states = day_of_week)

# The animate function renders the GIF
ukhsa_england_gif <- animate(ukhsa_england_agg,
        renderer = gifski_renderer(),
        height = 600, width = 1200)