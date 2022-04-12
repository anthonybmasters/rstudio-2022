## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(janitor)
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
# We draw the statistics straight from the online file
ons_rasmr_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fcomparisonsofallcausemortalitybetweeneuropeancountriesandregions%2fcurrent/icdatadownloadqad171120211.xlsx"
ons_rasmr_sheet <- "rASMRs - Countries"
ons_rasmr_range <- "A12:CN309"
ons_2020_w1_end_date <- as_date("2020-01-03")
ons_2021_w35_end_date <- as_date("2021-09-03")

ons_country_selection <- c("Belgium", "Bulgaria", "Czechia",
                           "France", "Netherlands", "Poland",
                           "Slovenia", "Spain", "United Kingdom")
ons_rasmr_title <- "Among European countries, Spain had the highest recorded peak in relative age-standardised mortality in 2020."
ons_rasmr_subtitle <- "Relative age-standardised mortality rates [%] by age group for selected European countries. Relative rates show the change compared to average weekly age-standardised mortality in 2015 to 2019. The series covers week 1 of 2020 (ending 3 January 2020) to week 35 of 2021 (ending 9 September 2021)."
ons_rasmr_caption <- "Source: Office for National Statistics: Comparisons of all-cause mortality between European countries and regions, 18 November 2021."

temp <- tempfile()
temp <- curl_download(url = ons_rasmr_url, destfile = temp,
                      quiet = TRUE, mode = "wb")
ons_rasmr_tbl <- read_excel(temp,
                            sheet = ons_rasmr_sheet,
                            range = ons_rasmr_range)

# Drawing the table of week numbers and dates
ons_week_sheet <- "Week number dates "
ons_week_range <- "A8:D357"
ons_week_tbl <- read_excel(temp,
                           sheet = ons_week_sheet,
                           range = ons_week_range)

ons_week_number_df <- ons_week_tbl %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    week_number = paste0(year, "W", formatC(week, flag = 0, width = 2)),
    start_date = as_date(start_date),
    end_date = as_date(end_date)) %>%
  dplyr::rename(
    week_start_date = start_date,
    week_end_date = end_date)

# Last, we create the tidy data frame
ons_rasmr_df <- ons_rasmr_tbl %>%
  dplyr::na_if("z") %>%
  dplyr::mutate_at(c(5:92), as.numeric) %>%
  tidyr::pivot_longer(
    cols = 5:92,
    names_to = "week_number",
    values_to = "mortality_rate") %>%
  dplyr::left_join(ons_week_number_df, by = "week_number") %>%
  janitor::clean_names() %>%
  tidyr::drop_na()

## Making the graph
# We want the annotation text on only one facet
rasmr_annotation_text <- tribble(
  ~country, ~week_end_date, ~mortality_rate, ~age_groups, ~label,
  "Belgium", as_date("2020-07-01"), 90, "All Ages", "All ages",
  "Belgium", as_date("2020-07-01"), 120, "65+", "65 and older",
  "Belgium", as_date("2020-07-01"), 150, "0-64", "64 and younger")

ons_rasmr_date_breaks <- seq(ons_2020_w1_end_date, by = "52 weeks",
                       length.out = 2)

ons_rasmr_gg <- ons_rasmr_df %>%
  dplyr::filter(country %in% ons_country_selection,
                persons == "Persons") %>%
  ggplot(aes(x = week_end_date,
             y = mortality_rate,
             colour = age_groups)) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_date(expand = c(0,0),
               breaks = ons_rasmr_date_breaks,
               date_labels = "%d %b\n%Y") +
  facet_wrap(~country) +
  geom_text(data = rasmr_annotation_text,
            label = rasmr_annotation_text$label,
            hjust = 0, size = 7, fontface = "bold") +
  scale_colour_manual(guide = "none",
                      values = c("#fe8c11", "#008080", "black")) +
  labs(title = ons_rasmr_title,
       subtitle = str_wrap(ons_rasmr_subtitle, width = 150),
       x = "Week end date (Friday)",
       y = "Relative age-standardised mortality rate [%]",
       caption = ons_rasmr_caption)