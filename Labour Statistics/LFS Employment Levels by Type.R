## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(stringr)
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
# We draw the statistics from an online file from the ONS
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/summaryoflabourmarketstatistics
ons_lms_url <- "https://www.ons.gov.uk/file?uri=%2femploymentandlabourmarket%2fpeopleinwork%2femploymentandemployeetypes%2fdatasets%2fsummaryoflabourmarketstatistics%2fcurrent/a01mar2022.xls"
ons_lms_sheet <- "3"
ons_lms_range <- "A8:F365"

# This code sets values for the analysis and graphs
ons_lms_start_date <- as_date("2012-01-01")
ons_lms_end_date <- as_date("2022-01-01")

ons_lms_title <- "For November to January 2022, total employment is lower than before the pandemic."
ons_lms_subtitle <- "Estimated levels of UK employment by type, from the ONS Labour Force Survey, with seasonal adjustments. The estimates are for Nov-Jan 2012 to Nov-Jan 2022."
ons_lms_caption <- "Source: Office for National Statistics: Labour market overview, UK: March 2022."

ons_change_title <- "Since the pandemic started, there has been a sustained reduction in self-employment in the UK."
ons_change_subtitle <- "Cumulative changes since December to February 2020 in estimated levels of UK employment by type, calculated from the ONS Labour Force Survey, with seasonal adjustments. The estimates are for Dec-Feb 2020 to Nov-Jan 2022."
ons_change_caption <- paste0("Author's calculations. ", ons_lms_caption)

# Now, we draw from the online file
temp <- tempfile()
temp <- curl_download(url = ons_lms_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ons_lms_df <- read_excel(temp,
                         sheet = ons_lms_sheet,
                         range = ons_lms_range) %>%
  dplyr::rename(
    date_text = 1,
    total_employment = 2,
    employees = 3,
    self_employed = 4,
    unpaid_family_workers = 5,
    government_supported_programmes = 6) %>%
  dplyr::mutate(
    other_employment = unpaid_family_workers + government_supported_programmes,
    date_month_text = str_sub(date_text, start = -8),
    date_3m = my(date_month_text))

# Put that data frame into a tidy frame
ons_lms_tidy_df <- ons_lms_df %>%
  dplyr::select(date_text, date_3m, total_employment, employees,
                self_employed, other_employment) %>%
  tidyr::pivot_longer(
    cols = 3:6,
    names_to = "employment_type",
    values_to = "levels")

# We can also calculate changes from Dec-Feb 2020
ons_dec_feb_2020_df <- ons_lms_tidy_df %>%
  dplyr::filter(date_3m == as_date("2020-02-01")) %>%
  rename(dec_feb_2020_levels = levels) %>%
  select(employment_type, dec_feb_2020_levels)

ons_change_tidy_df <- ons_lms_tidy_df %>%
  dplyr::filter(date_3m >= as_date("2020-02-01")) %>%
  left_join(ons_dec_feb_2020_df, by = "employment_type") %>%
  mutate(cumulative_change = levels - dec_feb_2020_levels)

## Making the graphs
# First, we set the date breaks
ons_lms_breaks <- seq(ons_lms_start_date, ons_lms_end_date,
                      by = "5 years") %>%
  as_tibble() %>%
  rename(date_3m = 1) %>%
  left_join(ons_lms_df, by = "date_3m") %>%
  dplyr::select(starts_with("date")) %>%
  mutate(date_text = str_replace(date_text, " ", "\n"))

ons_change_breaks <- seq(as_date("2020-02-01"), ons_lms_end_date,
                         by = "6 months") %>%
  as_tibble() %>%
  rename(date_3m = 1) %>%
  left_join(ons_lms_df, by = "date_3m") %>%
  dplyr::select(starts_with("date")) %>%
  mutate(date_text = str_replace(date_text, " ", "\n"))

# Next, set the direct label tables
ons_lms_labels <- tribble(
  ~employment_type, ~lms_label, ~lms_date, ~level, ~change_date, ~cumulative_change,
  "employees", "Employees", "2012-01-01", 27000, "2021-05-01", 200,
  "other_employment", "Other unemployment (unpaid family work & government programme)", "2012-01-01", 2000, "2020-04-01", 100,
  "self_employed", "Self-employed", "2012-01-01", 6000, "2021-05-01", -600,
  "total_employment", "Total in employment", "2012-01-01", 33000, "2021-08-01", -500) %>%
  mutate(lms_date = as_date(lms_date),
         change_date = as_date(change_date))

# The first graph shows levels
ons_lms_gg <- ons_lms_tidy_df %>%
  dplyr::filter(date_3m >= ons_lms_start_date) %>%
  ggplot(aes(x = date_3m, y = levels/1000,
             group = employment_type)) +
  geom_line(size = 1.5, aes(colour = employment_type)) +
  geom_vline(xintercept = as_date("2020-02-01"),
             linetype = "dashed") +
  scale_x_date(breaks = ons_lms_breaks$date_3m,
               labels = ons_lms_breaks$date_text) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0),
                     limits = c(0, 35000)) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#0941B3", "#fe8c11", "black")) +
  geom_text(data = ons_lms_labels,
            aes(x = lms_date, y = level,
                colour = employment_type, label = lms_label),
            family = "Spline Sans", fontface = "bold",
            size = 7, hjust = 0) +
  annotate("text", x = as_date("2020-03-01"), y = 20000,
           label = "December to February\n2020", family = "Spline Sans",
           fontface = "plain", size = 6, hjust = 0) +
  labs(title = ons_lms_title,
       subtitle = str_wrap(ons_lms_subtitle, width = 120),
       x = "Date [three-month rolling]",
       y = "Estimated employment level [thousands]",
       caption = ons_lms_caption)

# The second graph shows changes in those levels
ons_change_gg <- ons_change_tidy_df %>%
  ggplot(aes(x = date_3m, y = cumulative_change/1000,
             group = employment_type)) +
  geom_line(size = 1.5, aes(colour = employment_type)) +
  scale_x_date(breaks = ons_change_breaks$date_3m,
               labels = ons_change_breaks$date_text,
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(-1000, 500)) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#0941B3", "#fe8c11", "black")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data = ons_lms_labels,
            aes(x = change_date, y = cumulative_change,
                colour = employment_type, label = lms_label),
            family = "Spline Sans", fontface = "bold",
            size = 7, hjust = 0) +
  labs(title = ons_change_title,
       subtitle = str_wrap(ons_change_subtitle, width = 120),
       x = "Date [three-month rolling]",
       y = "Cumulative change [thousands]",
       caption = ons_change_caption)

## Saving the graphs
ggsave(file = "/cloud/project/Labour Statistics/ons_lms_gg.jpeg",
       plot = ons_lms_gg,
       device = "jpeg",
       height = 850/96, width = 1700/96, dpi = 96)

ggsave(file = "/cloud/project/Labour Statistics/ons_change_gg.jpeg",
       plot = ons_change_gg,
       device = "jpeg",
       height = 850/96, width = 1600/96, dpi = 96)