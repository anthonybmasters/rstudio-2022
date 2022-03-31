## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(httr)
library(jsonlite)
library(curl)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)
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
# We draw statistics straight from the Australian Bureau Statistics API
aus_mortality_title <- "Whilst Australian death registrations have generally increased, mortality rates declined."
aus_mortality_subtitle <- "Death registrations in Australia by year from 1970 to 2020, including temporary visitors and deaths in territorial waters. There are delays between date of death and registration. Crude mortality rates are death counts per 1,000 estimated residents. Age-standardised mortality keeps the same age structure as the Australian population in June 2001."
aus_mortality_caption <- "Source: Australian Bureau of Statistics API:\nDeaths, Year of registration, Summary data, Sex, States, Territories and Australia."

abs_api_url <- "https://api.data.abs.gov.au/data/ABS,DEATHS_SUMMARY,1.0.0/5+4+11.3.AUS.A?startPeriod=1971&endPeriod=2020&dimensionAtObservation=AllDimensions"
abs_api_output <- jsonlite::fromJSON(abs_api_url)
abs_api_matrix <- abs_api_output$dataSets %>%
  flatten() %>%
  dplyr::select(-1) %>%
  t()

# This rownames table stores the row names, which contains observation numbers
abs_rownames <- rownames(abs_api_matrix)

# In the flattened version, the value we want is in every fourth row
abs_api_df <- abs_api_matrix %>%
  as_tibble() %>%
  bind_cols(abs_rownames) %>%
  dplyr::rename(value = 1, observation_number = 2) %>%
  unnest(cols = "value") %>%
  dplyr::filter(row_number() %% 4 == 1) %>%
  dplyr::mutate(
    measure = case_when(
      substr(observation_number, 14, 14) == as.character(0) ~ "crude_rate",
      substr(observation_number, 14, 14) == as.character(1) ~ "death_count",
      TRUE ~ "standardised_rate"),
    year_chr = str_sub(observation_number, -2, -1),
    year = 1971 + as.numeric(str_replace(year_chr, "[^0-9.-]", ""))) %>%
  dplyr::select(year, measure, value)

# We draw stats from an online ABS file
aus_standardised_title <- "Age-standardised mortality has been lower in Australia in 2021 than in the years 2015 to 2019."
aus_standardised_subtitle <- "Age-standardised mortality rate for total doctor-certified deaths in Australia, by week of death. These statistics are for registrations up to 28th February 2022. The calculation keeps the same age structure as the Australian population in June 2001. As there are delays between death and registration, these preliminary statistics can revise."
aus_standardised_caption <- "Source: Australian Bureau of Statistics: Provisional Mortality Statistics, January 2020 to December 2021."
aus_standardised_pos <- as_date("2021-02-14")
aus_date_breaks <- seq(as_date("2021-02-28"), as_date("2021-12-12"),
                       by = "13 weeks")

abs_week_url <- "https://www.abs.gov.au/statistics/health/causes-death/provisional-mortality-statistics/jan-2020-dec-2021/Provisional%20Mortality%20Statistics%2C%20Weekly%20Dashboard%2C%20Jan%202020-Dec%202021.xlsx"
abs_week_sheet <- "Table 1.2"
abs_week_range <- "B12:BA17"
abs_date_range <- "B6:BA8"

temp <- tempfile()
temp <- curl_download(url = abs_week_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

abs_dates_tbl <- read_excel(temp,
                            sheet = abs_week_sheet,
                            range = abs_date_range) %>%
  t() %>%
  as_tibble(.name_repair = "minimal") %>%
  dplyr::rename(week_ended_2021 = 1, week_ended_2020 = 2) %>%
  dplyr::mutate(week_ended_2021 = as_date(week_ended_2021),
                week_ended_2020 = as_date(week_ended_2020))

abs_week_tbl <- read_excel(temp,
                           sheet = abs_week_sheet,
                           range = abs_week_range) %>%
  t() %>%
  as_tibble(.name_repair = "minimal") %>%
  dplyr::rename(
    total_deaths_2021 = 1,
    total_deaths_2020 = 2,
    total_deaths_2015_2019_average = 3,
    total_deaths_2015_2019_min = 4,
    total_deaths_2015_2019_max = 5)

abs_week_tidy_df <- bind_cols(abs_dates_tbl, abs_week_tbl) %>%
  dplyr::select(week_ended_2021, total_deaths_2021, total_deaths_2020,
                total_deaths_2015_2019_average) %>%
  tidyr::pivot_longer(cols = 2:4,
                      names_to = "measure",
                      values_to = "value")

abs_week_range_df <- bind_cols(abs_dates_tbl, abs_week_tbl) %>%
  dplyr::select(week_ended_2021, total_deaths_2015_2019_min,
                total_deaths_2015_2019_max)

## Making the graphs
# The first graph shows the overall count of registrations
aus_mortality_gg_1 <- abs_api_df %>%
  dplyr::filter(measure == "death_count") %>%
  ggplot(aes(x = year, y = value)) +
  geom_col(fill = "black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA),
                     expand = c(0,0),
                     labels = scales::comma_format()) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Death registrations in Australia",
       x = "Registration year",
       y = "Count")

# The second graph shows mortality rates
aus_mortality_gg_2 <- abs_api_df %>%
  dplyr::filter(measure %in% c("crude_rate", "standardised_rate")) %>%
  ggplot(aes(x = year, y = value, group = measure, colour = measure)) +
  geom_line(size = 1.5) +
  scale_colour_manual(guide = "none",
                      values = c("#fe8c11", "#008080")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,NA),
                     expand = c(0,0)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  annotate("text", x = 1980, y = 6,
           label = "Crude mortality rate",
           family = "Spline Sans", colour = "#fe8c11",
           fontface = "bold", hjust = 0, size = 7) +
  annotate("text", x = 1995, y = 10,
           label = "Age-standardised\nmortality rate",
           family = "Spline Sans", colour = "#008080",
           fontface = "bold", hjust = 0, size = 7) +
  labs(subtitle = "Deaths per 1,000 people",
       x = "Registration year",
       y = "Rate")

# We join these two graphs together
aus_mortality_gg <- aus_mortality_gg_1 + aus_mortality_gg_2 +
  plot_annotation(title = aus_mortality_title,
                  subtitle = str_wrap(aus_mortality_subtitle, width = 125),
                  caption = aus_mortality_caption)

# The graph shows death counts by week of occurrence in Australia
aus_standardised_week_gg <- abs_week_tidy_df %>%
  ggplot(aes(x = week_ended_2021)) +
  geom_line(aes(y = value, colour = measure, linetype = measure),
            size = 1.5, na.rm = TRUE) +
  geom_ribbon(data = abs_week_range_df,
              aes(ymin = total_deaths_2015_2019_min,
                  ymax = total_deaths_2015_2019_max),
              alpha = 0.1,
              fill = "#008080") +
  scale_x_date(expand = c(0,0),
               date_labels = "%d %b\n%Y",
               breaks = aus_date_breaks) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#0941B3", "#fe8c11")) +
  scale_linetype_manual(guide = "none",
                        values = c("dashed", "solid", "solid")) +
  annotate("text", x = aus_standardised_pos, y = 10,
           label = "Average and range of age-standardised mortality\n2015 to 2019",
           family = "Spline Sans", colour = "#008080",
           fontface = "bold", hjust = 0, size = 7) +
  annotate("text", x = aus_standardised_pos, y = 7,
           label = "Age-standardised mortality in 2021",
           family = "Spline Sans", colour = "#fe8c11",
           fontface = "bold", hjust = 0, size = 7) +
  annotate("text", x = aus_standardised_pos, y = 6.2,
           label = "Age-standardised mortality in 2020",
           family = "Spline Sans", colour = "#0941B3",
           fontface = "bold", hjust = 0, size = 7) +
  labs(title = aus_standardised_title,
       subtitle = str_wrap(aus_standardised_subtitle, width = 125),
       caption = aus_standardised_caption,
       x = "Week of death, end date in 2021 (Sunday)",
       y = "Mortality rate per 100,000 people")