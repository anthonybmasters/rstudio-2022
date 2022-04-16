## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(ukcovid19)
library(lubridate)
library(janitor)
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
# UKHSA (PHW) figures for Wales
# First, we set our query filters and structure
ukhsa_query_filters <- c('areaType=nation', 'areaName=Wales')
ukhsa_query_structure <- list(
  date = "date",
  area_name = "areaName",
  area_code = "areaCode",
  new_cases_by_specimen_date = "newCasesBySpecimenDate",
  new_pillar1_tests_by_publish_date = "newPillarOneTestsByPublishDate",
  new_pillar2_tests_by_publish_date = "newPillarTwoTestsByPublishDate",
  new_tests_by_publish_date = "newTestsByPublishDate")

# Next, draw out the figures
ukhsa_wales_df <- ukcovid19::get_data(
  filter = ukhsa_query_filters,
  structure = ukhsa_query_structure) %>%
  tidyr::pivot_longer(
  cols = 4:7,
  names_to = "measure",
  values_to = "count") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(date = as_date(date))

# ONS Covid-19 Infection Survey for Wales
ons_cisw_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fconditionsanddiseases%2fdatasets%2fcovid19infectionsurveywales%2f2022/20220414covid19infectionsurveydatasetswales.xlsx"
ons_cisw_estimate_sheet <- "1a"
ons_cisw_estimate_range <- "A5:D96"
ons_cisw_model_sheet <- "1b"
ons_cisw_model_range <- "A5:D47"

temp <- tempfile()
temp <- curl_download(url = ons_cisw_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

# Read the online file, renaming and creating columns
ons_cisw_est_df <- read_excel(temp,
                               sheet = ons_cisw_estimate_sheet,
                               range = ons_cisw_estimate_range) %>%
  tidyr::drop_na() %>%
  dplyr::rename(
    time_period = 1,
    estimated_average_positivity = 2,
    estimated_lower_credible = 3,
    estimated_upper_credible = 4) %>%
  dplyr::mutate(
    end_date_text = word(time_period, 5, -1),
    end_date = dmy(end_date_text))

cisw_start_date <- min(ons_cisw_est_df$end_date) - days(6)

ons_cisw_model_df <- read_excel(temp,
                                sheet = ons_cisw_model_sheet,
                                range = ons_cisw_model_range) %>%
  dplyr::rename(
    date = 1,
    model_test_positive = 2,
    model_lower_credible = 3,
    model_upper_credible = 4) %>%
  dplyr::mutate(date = as_date(date))

## Making the graphs
# Confirmed cases in Wales
cases_max_date <- max(ukhsa_wales_df$date)

cases_wales_gg <- ukhsa_wales_df %>%
  dplyr::filter(measure == "new_cases_by_specimen_date",
                date >= cisw_start_date) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(size = 1.5) +
  scale_x_date(expand = c(0,0),
               date_breaks = "6 months",
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA),
                     labels = comma_format()) +
  annotate("rect", xmin = cases_max_date - days(7), xmax = cases_max_date,
           ymin = 0, ymax = 17000,
           alpha = 0.1, fill = "#d3a41b") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Confirmed cases by specimen date",
       x = "Specimen date",
       y = "")

# Viral tests in Wales
tests_wales_gg <- ukhsa_wales_df %>%
  dplyr::filter(measure %in% c("new_pillar1_tests_by_publish_date",
                               "new_pillar2_tests_by_publish_date"),
                date >= cisw_start_date) %>%
  ggplot(aes(x = date, y = count, group = measure)) +
  geom_col(aes(fill = measure),
           position = position_stack(reverse = TRUE)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "6 months",
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA),
                     labels = comma_format()) +
  scale_fill_manual(guide = "none",
                      values = c("#008080", "#fe8c11")) +
  annotate("text", x = as_date("2021-01-14"), y = 45000,
           label = "Pillar 1 (NHS & UKHSA tests)", family = "Spline Sans",
           hjust = 0, size = 7, fontface = "bold", colour = "#008080") +
  annotate("text", x = as_date("2021-01-14"), y = 55000,
           label = "Pillar 2 (community tests)", family = "Spline Sans",
           hjust = 0, size = 7, fontface = "bold", colour = "#fe8c11") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Viral tests by reporting date",
       x = "Reporting date",
       y = "")

# Official estimates from the ONS infection survey
cis_wales_gg <- ons_cisw_est_df %>%
  ggplot(aes(x = end_date, y = estimated_average_positivity,
             ymin = estimated_lower_credible,
             ymax = estimated_upper_credible)) +
  geom_pointrange() +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(limits = c(0, 10),
                     labels = label_percent(scale = 1)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Estimated positivity in Welsh private households",
       x = "Weekly specimen date",
       y = "")

# Daily estimates from the ONS infection survey
model_wales_gg <- ons_cisw_model_df %>%
  ggplot(aes(x = date, y = model_test_positive,
             ymin = model_lower_credible, ymax = model_upper_credible)) +
  geom_line(colour = "#0941B3", size = 1.5) +
  geom_ribbon(fill = "#0941B3", alpha = 0.1) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(limits = c(0, 10),
                     labels = label_percent(scale = 1)) +
  annotate("text", x = as_date("2022-03-21"), y = 4,
           label = "Modelled estimates with\n95% credible intervals",
           family = "Spline Sans",
           hjust = 0, size = 6, fontface = "bold", colour = "#0941B3") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Daily positivity in Welsh private households (in the last six weeks)",
       x = "Date",
       y = "")

# Putting the graphs together
wales_title <- "Reduced community testing erodes confirmed case counts in Wales, whilst random testing maintains high positivity."
wales_subtitle <- "Public Health Wales report confirmed SARS-CoV-2 case counts (by specimen date) and reported viral tests (by testing pillar and report date). Confirmed case counts for recent days may be incomplete, and are subject to revision. The ONS Covid-19 infection survey estimates positivity in Welsh private households, excluding institutions such as care homes. Both the reported estimates and daily modelled estimates have 95% credible intervals."
wales_caption <- "Sources: UK Health Security Agency R Package; Office for National Statistics Covid-19 Infection Survey, 14 April 2022."

wales_trend_gg <- (cases_wales_gg + tests_wales_gg) / (cis_wales_gg + model_wales_gg) +
  plot_annotation(title = wales_title,
                  subtitle = str_wrap(wales_subtitle, width = 180),
                  caption = wales_caption)