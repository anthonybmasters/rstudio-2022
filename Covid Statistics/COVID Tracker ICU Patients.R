## Packages and Themes
# Install the packages we need
library(tidyverse)
library(lubridate)
library(scales)
library(sysfonts)
library(showtext)

# Next, set the plotting theme
font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
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

## Drawing and tidying the data
# We draw the ICU figures from the online CSV
# Replicating the work by Tristan Mahr
# https://www.tjmahr.com/morgan-stanley-cursed-covid-plot/
covid_tracker_url <- "https://covidtracking.com/data/download/all-states-history.csv"

covid_tracker_title <- "In the latest dataset, there are some states with missing values in early May 2020."
covid_tracker_subtitle <- "The number of people currently hospitalized in the Intensive Care Unit with Covid-19, by US state. Definitions differ between states and the states have different population sizes. The time series are between 28 April and 11 May 2020. It is sometimes unclear if the measure includes hospitalised children."
covid_tracker_caption <- "Source: COVID Tracking Project (United States)."

covid_nys_title <- "For New York, later versions filled out earlier missing values for ICU patients with Covid-19."
covid_nys_subtitle <- "The number of people currently hospitalized in the Intensive Care Unit with Covid-19, in the state of New York. The time series are between 28 April and 11 May 2020. The COVID Tracking Project file was later revised."

covid_tracker_tbl <- readr::read_csv(
  file = covid_tracker_url,
  col_types = cols(
    date = col_date(),
    state = col_character(),
    inIcuCurrently = col_number(),
    .default = col_skip()),
  progress = FALSE)

covid_tracker_df <- covid_tracker_tbl %>%
  dplyr::filter(date >= as_date("2020-04-28"),
                date <= as_date("2020-05-11")) %>%
  tidyr::drop_na()

# We can also draw older versions of the CSV file
covid_backup_url <- "https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/5ec9962d5f5f6505bb0593df150ab62867af98f7/data/states_daily_4pm_et.csv"

covid_backup_tbl <- readr::read_csv(
  file = covid_backup_url,
  col_types = cols(
    date = col_date("%Y%m%d"),
    state = col_character(),
    inIcuCurrently = col_number(),
    .default = col_skip()),
  progress = FALSE) %>%
  dplyr::rename(inIcuCurrently_2020_05_12 = inIcuCurrently)

covid_tidy_df <- covid_tracker_df %>%
  dplyr::left_join(covid_backup_tbl,
                   by = c("date", "state")) %>%
  pivot_longer(cols = 3:4,
               names_to = "measure",
               values_to = "count")

## Making the graphs
covid_tracker_gg <- covid_tracker_df %>%
  ggplot(aes(x = date, y = inIcuCurrently)) +
  geom_line(size = 1.5, na.rm = TRUE) +
  facet_wrap(~state) +
  scale_x_date(expand = c(0,0),
               date_labels = "%d %b\n%Y",
               date_breaks = "1 week") +
  scale_y_continuous(limits = c(0, NA),
                     labels = comma_format()) +
  labs(title = covid_tracker_title,
       subtitle = str_wrap(covid_tracker_subtitle, width = 160),
       x = "Date",
       y = "Currently in ICU with Covid-19",
       caption = covid_tracker_caption)

covid_nys_ann <- tribble(
  ~date, ~count, ~measure, ~annotation_label,
  as_date("2020-05-01"), 3800, "inIcuCurrently", "Latest version",
  as_date("2020-05-08"), 3300, "inIcuCurrently_2020_05_12", "Version on\n12 May 2020")

covid_nys_gg <- covid_tidy_df %>%
  dplyr::filter(state == "NY") %>%
  ggplot(aes(x = date, y = count, group = measure)) +
  geom_line(aes(colour = measure),
            size = 1.5, na.rm = TRUE) +
  geom_text(data = covid_nys_ann,
            aes(colour = measure, label = annotation_label),
            family = "Spline Sans", fontface = "bold",
            size = 7, hjust = 0) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#fe8c11")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%d %b\n%Y",
               date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, NA),
                     labels = comma_format()) +
  annotate("text", x = as_date("2020-05-01"), y = 2500,
           label = "These values were missing\nfrom the version on 12 May.",
           family = "Spline Sans", fontface = "plain",
           size = 6, hjust = 0 ) +
  annotate("curve", x = as_date("2020-05-02"), xend = as_date("2020-05-03"),
           y = 2750, yend = 3300, curvature = -0.2,
           arrow = arrow(length = unit(2, "mm")), colour = "#008080") +
  labs(title = covid_nys_title,
       subtitle = str_wrap(covid_nys_subtitle, width = 120),
       x = "Date",
       y = "Currently in ICU with Covid-19",
       caption = covid_tracker_caption)