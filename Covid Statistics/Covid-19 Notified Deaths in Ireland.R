## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(janitor)
library(scales)
library(viridis)
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
# Draw statistics from the API
hpsc_api_url <- "https://services1.arcgis.com/eNO7HHeQ3rUcBllm/arcgis/rest/services/CovidStatisticsProfileHPSCIrelandOpenData/FeatureServer/0/query?where=1%3D1&outFields=Date,StatisticsProfileDate,DeathsToday_DOD,DeathsCumulative_DOD,ConfirmedCovidCases,TotalConfirmedCovidCases,ConfirmedCovidDeaths,TotalCovidDeaths&outSR=4326&f=json"
hpsc_api_output <- jsonlite::fromJSON(hpsc_api_url)
hpsc_api_tbl <- hpsc_api_output$features$attributes %>%
  janitor::clean_names()

# Set labels for the graphs
hpsc_deaths_title <- "Notified Covid-19 deaths in Ireland have shown two large peaks."
hpsc_deaths_subtitle <- "Notified deaths involving Covid-19 in Ireland, by reporting date and date of death. These figures include deaths in confirmed, probable and possible Covid-19 cases. Deaths by date of occurrence in recent days may be incomplete, and are subject to revision."

hpsc_report_title <- "Before daily publication ended, Covid-19 surveillance deaths in Ireland had a reporting cycle."
hpsc_report_subtitle <- "Notified deaths involving Covid-19 in Ireland, by reporting date, allocated to each week. These figures include deaths in confirmed, probable and possible Covid-19 cases. There are delays between death and notification, and some notified deaths may be later removed."
hpsc_source <- "Source: Health Protection Surveillance Centre (Ireland): COVID-19 HPSC Detailed Statistics Profile."

# Transform the date columns
hpsc_datetime_origin <- "1970-01-01 00:00:00"
hpsc_covid_df <- hpsc_api_tbl %>%
  dplyr::mutate(
    date = as_datetime(date/1000,
                       origin = hpsc_datetime_origin),
    statistics_profile_date = as_datetime(statistics_profile_date/1000,
                                          origin = hpsc_datetime_origin),
    date = as_date(date),
    statistics_profile_date = as_date(statistics_profile_date),
    week_end_date = ceiling_date(date, unit = "week",
                                 change_on_boundary = FALSE),
    day_of_week = wday(date, label = TRUE, abbr = FALSE))

# Tidy the data, and set a prevailing date column
hpsc_tidy_df <- hpsc_covid_df %>%
  dplyr::select(date, statistics_profile_date,
                confirmed_covid_deaths, deaths_today_dod) %>%
  tidyr::pivot_longer(cols = 3:4,
                      names_to = "measure",
                      values_to = "count") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    prevail_date = case_when(
      measure == "confirmed_covid_deaths" ~ date,
      measure == "deaths_today_dod" ~ statistics_profile_date))

## Making the graphs
hpsc_deaths_ann <- tribble(
  ~measure, ~annotation_label, ~count,
  "confirmed_covid_deaths", "Deaths by reporting date", 77.5,
  "deaths_today_dod", "By date of death", 70) %>%
  dplyr::mutate(prevail_date = as_date("2020-06-14"))

deaths_max_date <- max(hpsc_tidy_df$date)

hpsc_deaths_gg <- hpsc_tidy_df %>%
  ggplot(aes(x = prevail_date, y = count, colour = measure)) +
  geom_line(size = 1.5) +
  scale_x_date(expand = c(0,0),
               date_breaks = "13 weeks",
               date_labels = "%d %b\n%Y") +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#fe8c11")) +
  geom_text(data = hpsc_deaths_ann,
            aes(x = prevail_date, y = count, colour = measure,
                label = annotation_label),
            family = "Spline Sans", fontface = "bold",
            size = 7, hjust = 0) +
  annotate("rect", xmin = deaths_max_date - days(14), xmax = deaths_max_date,
           ymin = 0, ymax = 100,
           alpha = 0.1, fill = "#d3a41b") +
  annotate("rect", xmin = as_date("2021-06-17"), xmax = as_date("2022-02-09"),
           ymin = 0, ymax = 100,
           alpha = 0.5, fill = "#e9eaec") +
  annotate("text", x = as_date("2021-06-30"), y = 90,
           label = "Deaths reported once a week",
           family = "Spline Sans", colour = "black", fontface = "plain",
           size = 7, hjust = 0) +
  labs(title = hpsc_deaths_title,
       subtitle = str_wrap(hpsc_deaths_subtitle, width = 140),
       x = "Date",
       y = "Notified Covid-19 deaths",
       caption = hpsc_source)

hpsc_analysis_end_date <- as_date("2021-03-28")
hpsc_date_breaks <- seq(as_date("2020-03-08"), hpsc_analysis_end_date,
                        by = "13 weeks") %>%
  lubridate::as_date()

hpsc_report_ann <- tribble(
  ~day_of_week, ~confirmed_covid_deaths,
  "Sunday", 80,
  "Monday", 75,
  "Tuesday", 70,
  "Wednesday", 65,
  "Thursday", 60,
  "Friday", 55,
  "Saturday", 50) %>%
  dplyr::mutate(week_end_date = as_date("2020-06-14"))

hpsc_report_gg <- hpsc_covid_df %>%
  dplyr::filter(date <= hpsc_analysis_end_date) %>%
  ggplot(aes(x = week_end_date, y = confirmed_covid_deaths,
             group = day_of_week)) +
  geom_line(aes(colour = day_of_week), size = 1.5) +
  geom_text(data = hpsc_report_ann,
            aes(x = week_end_date, y = confirmed_covid_deaths,
                colour = day_of_week, label = day_of_week),
            family = "Spline Sans", fontface = "bold",
            size = 7, hjust = 0) +
  scale_x_date(expand = c(0,0),
               breaks = hpsc_date_breaks,
               date_labels = "%d %b\n%Y") +
  scale_colour_viridis(guide = "none", discrete = TRUE) +
  labs(title = hpsc_report_title,
       subtitle = str_wrap(hpsc_report_subtitle, width = 140),
       x = "Reporting week: week end date (Sunday)",
       y = "Notified Covid-19 deaths",
       caption = hpsc_source)
