## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(readxl)
library(curl)
library(janitor)
library(lubridate)

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

## Drawing and tidying the data
# Draw the data straight from the online NISRA file
# https://www.nisra.gov.uk/publications/weekly-deaths-week-ending-31-december-2021
# First, set some parameters
nisra_deaths_url <- "https://www.nisra.gov.uk/system/files/statistics/Weekly_Deaths%20-%20w%20e%2031st%20December%202021_0.XLSX"
nisra_caption <- "Source: Northern Ireland Statistics and Research Agency: Weekly Deaths - Week ending 31st December 2021."
nisra_2020_first_date <- as_date("2020-01-10")
nisra_2020_last_date <- as_date("2021-01-01")

# Next, draw two tables from the file
temp <- tempfile()
temp <- curl_download(url = nisra_deaths_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

nisra_table1_tbl <- read_excel(temp,
                               sheet = "Table 1",
                               range = "A4:E56") %>%
  janitor::clean_names() %>%
  dplyr::rename(date = 2, total_deaths = 3, total_deaths_2020 = 4,
                total_deaths_average_2015_2019 = 5) %>%
  dplyr::mutate(date_2020 = seq(nisra_2020_first_date,
                                nisra_2020_last_date, "7 days"),
                date = as_date(date))

# This creates the 2020 and 2021 values
nisra_table1_2020_df <- nisra_table1_tbl %>%
  dplyr::select(4:6) %>%
  dplyr::rename(total_deaths = total_deaths_2020,
                date = date_2020) %>%
  dplyr::select(date, total_deaths, total_deaths_average_2015_2019)

nisra_table1_2021_df <- nisra_table1_tbl %>%
  dplyr::select(2, 3, 5) %>%
  dplyr::select(date, total_deaths, total_deaths_average_2015_2019)

# We then bind the two data frames by their rows
nisra_deaths_df <- bind_rows(nisra_table1_2020_df,
                             nisra_table1_2021_df)

# Next, we draw deaths involving Covid-19 from Table 5
nisra_table5_tbl <- read_excel(temp,
                               sheet = "Table 5",
                               range = "D3:CS5") %>%
  t() %>% as_tibble(.name_repair = "minimal") %>%
  dplyr::rename(date = 1, covid_deaths = 2) %>%
  mutate(date = gsub(".*\\\n", "", date),
         date = as_date(parse_date_time(date, orders = "dBY")),
         covid_deaths = as.numeric(covid_deaths))

# We then join the two data frames
nisra_deaths_df <- nisra_deaths_df %>%
  left_join(nisra_table5_tbl, by = "date") %>%
  mutate(covid_deaths = replace_na(covid_deaths, 0),
         other_deaths = total_deaths - covid_deaths)

nisra_deaths_tidy_df <- nisra_deaths_df %>%
  select(date, covid_deaths, other_deaths) %>%
  pivot_longer(cols = 2:3,
               names_to = "nisra_measure",
               values_to = "count")

## Making the graph
# We test two options for the average deaths (2015-2019) line
nisra_deaths_test_gg1 <- nisra_deaths_tidy_df %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = count, fill = nisra_measure),
           position = "stack") +
  geom_step(data = nisra_deaths_df,
              aes(x = date, y = total_deaths_average_2015_2019),
              stat = "identity", direction = "mid", size = 2)

nisra_deaths_test_gg2 <- nisra_deaths_tidy_df %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = count, fill = nisra_measure),
           position = "stack") +
  geom_errorbarh(data = nisra_deaths_df,
                 aes(xmin = date - days(3),
                     xmax = date + days(3),
                     y = total_deaths_average_2015_2019),
                 stat = "identity", size = 1.5, height = 0)

# Next, we make the full graph
nisra_date_breaks <- seq(as_date("2020-03-27"),
                         as_date("2021-12-24"), "13 weeks")

nisra_deaths_gg <- nisra_deaths_tidy_df %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = count, fill = nisra_measure),
           position = "stack") +
  geom_errorbarh(data = nisra_deaths_df,
                 aes(xmin = date - days(3),
                     xmax = date + days(3),
                     y = total_deaths_average_2015_2019),
                 stat = "identity", size = 1.5, height = 0) +
  scale_x_date(breaks = nisra_date_breaks,
               date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#800000", "#008080"),
                    guide = "none") +
  annotate("text", x = as_date("2020-06-12"), y = 500,
           label = "Deaths involving Covid-19", hjust = 0,
           size = 7, fontface = "bold", colour = "#800000") +
  annotate("text", x = as_date("2020-06-12"), y = 450,
           label = "Other deaths", hjust = 0,
           size = 7, fontface = "bold", colour = "#008080") +
  annotate("text", x = as_date("2021-06-11"), y = 450,
           label = "Average deaths\nin 2015-2019", hjust = 0,
           size = 7, fontface = "bold", colour = "black") +
  annotate("curve", x = as_date("2021-06-10"), xend = as_date("2021-04-30"),
           y = 450, yend = 330, curvature = 0.2,
           arrow = arrow(length = unit(2, "mm")), colour = "black") +
  labs(title = "Northern Ireland has sustained periods of deaths above the 2015-2019 average.",
       subtitle = str_wrap("Weekly registered deaths in the NI General Registrar's Office Registration System, between 4th January 2020 and 31st December 2021. Public holidays can affect delays between death and registration. Registered deaths are split by whether Covid-19 was mentioned on the certificate.",
                           110),
       x = "Week end date (Friday)",
       y = "Weekly registered deaths",
       caption = nisra_caption)