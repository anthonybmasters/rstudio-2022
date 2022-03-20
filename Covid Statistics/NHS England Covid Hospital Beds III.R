## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(patchwork)
library(sysfonts)
library(showtext)

# We also need to install the UK Health Security Agency package
remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(ukcovid19)

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
# NHS England Primary Diagnosis Supplement
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
# Set the parameters
nhs_england_url_sep21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Primary-Diagnosis-Supplement-20211209-20210618-20210930.xlsx"
nhs_england_range_sep21 <- "C13:DD22"
nhs_england_url_mar22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/03/Primary-Diagnosis-Supplement-20220317.xlsx"
nhs_england_range_mar22 <- "C13:FM22"

# As we undertake the same process four times, set up a function
nhs_england_draw <- function(input_url, input_sheet,
                             input_range, input_name){
  temp <- tempfile()
  temp <- curl_download(url = input_url, destfile = temp,
                        quiet = TRUE, mode = "wb")
  nhs_england_df <- read_excel(temp,
                               sheet = input_sheet,
                               range = input_range) %>%
    drop_na() %>% t() %>%
    as_tibble(rownames = NA, .name_repair = "unique") %>%
    rownames_to_column() %>%
    row_to_names(row_number = 1) %>%
    dplyr::rename(date = 1) %>%
    pivot_longer(cols = 2:9,
                 names_to = "region",
                 values_to = "dummy_name") %>%
    mutate(date = excel_numeric_to_date(as.numeric(date)),
           dummy_name = as.numeric(dummy_name)) %>%
    dplyr::rename(!!sym(input_name) := dummy_name)
  return(nhs_england_df)
}

# Next, draw from the two online files
nhs_tot_tbl_sep21 <- nhs_england_draw(input_url = nhs_england_url_sep21,
                                      input_sheet = "Total Beds Occupied Covid",
                                      input_range = nhs_england_range_sep21,
                                      input_name = "covid_total_beds_occupied")

nhs_pri_tbl_sep21 <- nhs_england_draw(input_url = nhs_england_url_sep21,
                                      input_sheet = "Primarily Covid",
                                      input_range = nhs_england_range_sep21,
                                      input_name = "covid_primary_beds_occupied")

nhs_tot_tbl_mar22 <- nhs_england_draw(input_url = nhs_england_url_mar22,
                                      input_sheet = "Total Beds Occupied Covid",
                                      input_range = nhs_england_range_mar22,
                                      input_name = "covid_total_beds_occupied")

nhs_pri_tbl_mar22 <- nhs_england_draw(input_url = nhs_england_url_mar22,
                                      input_sheet = "Primarily Covid",
                                      input_range = nhs_england_range_mar22,
                                      input_name = "covid_primary_beds_occupied")

# Join the total and primary tables together
nhs_sep21_df <- full_join(nhs_tot_tbl_sep21, nhs_pri_tbl_sep21,
                          by = c("date", "region"))

nhs_mar22_df <- full_join(nhs_tot_tbl_mar22, nhs_pri_tbl_mar22,
                          by = c("date", "region"))

# UK Health Security Agency Covid-19 dashboard
ukhsa_query_filters <- c('areaType=nation', 'areaName=England')
ukhsa_query_structure <- list(
  date = "date",
  area_name = "areaName",
  area_code = "areaCode",
  all_hospital_cases = "hospitalCases")

ukhsa_covid_df <- ukcovid19::get_data(
  filters = ukhsa_query_filters,
  structure = ukhsa_query_structure) %>%
  mutate(date = as_date(date))

# We then join our data frames and calculate new columns
nhs_england_df <- bind_rows(nhs_sep21_df, nhs_mar22_df) %>%
  dplyr::filter(region == "ENGLAND") %>%
  dplyr::select(-region) %>%
  dplyr::left_join(ukhsa_covid_df, by = "date") %>%
  dplyr::mutate(
    covid_total_beds_non_acute = all_hospital_cases - covid_total_beds_occupied,
    non_primary_beds_occupied = covid_total_beds_occupied - covid_primary_beds_occupied,
    covid_primary_share = covid_primary_beds_occupied / covid_total_beds_occupied,
    covid_primary_share_min = covid_primary_beds_occupied / all_hospital_cases,
    covid_primary_share_max = (covid_primary_beds_occupied + covid_total_beds_non_acute) / all_hospital_cases)

nhs_england_tidy_count_df <- nhs_england_df %>%
  dplyr::select(date, contains("_beds_")) %>%
  tidyr::pivot_longer(
    cols = 2:5,
    names_to = "measure",
    values_to = "count")

nhs_england_tidy_count_df$measure <- factor(
  nhs_england_tidy_count_df$measure,
  levels  = c("covid_primary_beds_occupied", "non_primary_beds_occupied",
              "covid_total_beds_occupied", "covid_total_beds_non_acute"))

## Making the graphs
# The graphs show SARS-CoV-2 positive patients by diagnosis and trust type
nhs_graph_pos_date <- as_date("2021-06-21")

nhs_england_gg1 <- nhs_england_tidy_count_df %>%
  dplyr::filter(measure != "covid_total_beds_occupied") %>%
  ggplot(aes(x = date, y = count, group = measure)) +
  geom_col(aes(fill = measure),
           position = position_stack(reverse = TRUE)) +
  scale_x_date(date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                      expand = c(0,0)) +
  scale_fill_manual(guide = "none",
                    values = c("#008080", "#fe8c11", "#0941B3")) +
  annotate("text", x = nhs_graph_pos_date, y = 8000,
           label = "Primarily Covid-19 in acute trusts",
           family = "Spline Sans", fontface = "bold",
           hjust = 0, size = 7, colour = "#008080") +
  annotate("text", x = nhs_graph_pos_date, y = 9000,
           label = "Other primary diagnosis (acute)",
           family = "Spline Sans", fontface = "bold",
           hjust = 0, size = 7, colour = "#fe8c11") +
  annotate("text", x = nhs_graph_pos_date, y = 10000,
           label = "Covid patients in non-acute trusts",
           family = "Spline Sans", fontface = "bold",
           hjust = 0, size = 7, colour = "#0941B3") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Total beds occupied by confirmed Covid-19 patients",
       x = "Date",
       y = "")

nhs_england_gg2 <- nhs_england_df %>%
  dplyr::select(date, contains("_share")) %>%
  ggplot(aes(x = date, y = covid_primary_share,
             ymin = covid_primary_share_min, ymax = covid_primary_share_max)) +
  geom_line(size = 1.5, colour = "#008080") +
  geom_ribbon(alpha = 0.2, fill = "#008080") +
  scale_x_date(date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_percent(scale = 100),
                     expand = c(0,0),
                     limits = c(0,1)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "#008080")) +
  annotate("text", x = nhs_graph_pos_date, y = 0.5,
           label = "The shaded area shows the possible range,\ngiven patients in non-acute trusts.",
           family = "Spline Sans", hjust = 0, size = 6) +
  geom_curve(x = as_date("2021-12-07"), xend = as_date("2022-01-01"),
             y = 0.45, yend = 0.5,
             colour = "#008080", size = 1.1, curvature = 0.2,
             arrow = arrow(length = unit(0.3, "cm"),
                           type = "closed")) +
   labs(subtitle = "Primarily Covid-19 share of Covid-19 beds",
       x = "Date",
       y = "")

# We put these two graphs together
nhs_england_title <- "On 15 March 2022, 41% - 48% of positive patients in England had their primary treatment for Covid-19."
nhs_england_subtitle <- "Total beds occupied in England by patients with a positive SARS-CoV-2 test (fewer than 14 days before admission or after admission. This is split by primary diagnosis in acute trusts on a best endeavours basis."
nhs_england_caption <- "Author's calculations. Sources: NHS England Covid-19 Hospital Activity: Primary Diagnosis Supplements (up to 15th March 2021); UK Health Security Agency Covid-19 R Package."

nhs_england_gg <- nhs_england_gg1 + nhs_england_gg2 +
  plot_annotation(
    title = nhs_england_title,
    subtitle = str_wrap(nhs_england_subtitle, width = 115),
    caption = str_wrap(nhs_england_caption, width = 100))

## Saving the graph
ggsave(file = "/cloud/project/Covid Statistics/nhs_england_gg.jpeg",
       plot = nhs_england_gg,
       device = "jpeg",
       height = 900/96, width = 1800/96, dpi = 96)