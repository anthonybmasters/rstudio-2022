## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
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
# We draw the statistics from two online ONS files
ons_avg_url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/14188numberofdeathsregisteredfiveyearaverage20162017201820192020inenglandandwalesbyweek/fiveyearaverageenglandandwales1.xlsx"
ons_avg_sheet <- "1"
ons_avg_range <- "A5:G57"

ons_2021_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek522021.xlsx"
ons_2021_sheet <- "Weekly figures 2021"
ons_2021_range <- "C10:BB12"

ons_graph_title <- "The five-year average of 2016-2019 & 2021 had about 11,000 more deaths in England & Wales than 2015-2019."
ons_graph_subtitle <- "Average number of registered deaths by different groups of years, for England and Wales. Public holidays affect the number of death registrations in each week."
ons_graph_caption <- "Author's calculation for the 2017-2021 average. Sources: Office for National Statistics: Weekly registered deaths, up to 31 December 2021;\nNumber of deaths registered in 2016, 2017, 2018, 2019, 2021 and the five-year average in England and Wales by week."

# The first file gives a table of weekly deaths
temp <- tempfile()
temp <- curl_download(url = ons_avg_url, destfile = temp,
                      quiet = TRUE, mode = "wb")
ons_avg_tbl <- read_excel(temp,
                          sheet = ons_avg_sheet,
                          range = ons_avg_range) %>%
  janitor::clean_names() %>%
  dplyr::rename(ons_5_year_average = 7)

# The second is the 2021 weekly deaths file
temp <- curl_download(url = ons_2021_url, destfile = temp,
                      quiet = TRUE, mode = "wb")
ons_2021_tbl <- read_excel(temp,
                           sheet = ons_2021_sheet,
                           range = ons_2021_range) %>%
  t() %>%
  dplyr::as_tibble(.name_repair = "unique") %>%
  dplyr::rename(x2020 = 1,
                ons_2015_2019_average = 2) %>%
  dplyr::mutate(week_number = seq(1:52))

# Join those two tables together by week number, and tidy the data frame
ons_weekly_deaths_df <- full_join(ons_avg_tbl, ons_2021_tbl,
                                  by = "week_number") %>%
  dplyr::mutate(ons_2017_2021_average = round((x2017+x2018+x2019+x2020+x2021)/5))

ons_weekly_deaths_tidy_df <- ons_weekly_deaths_df %>%
  dplyr::select(week_number, starts_with("ons")) %>%
  tidyr::pivot_longer(cols = 2:4,
                      names_to = "measure",
                      values_to = "average")

## Making the graph
ons_deaths_baseline_gg <- ons_weekly_deaths_tidy_df %>%
  ggplot(aes(x = week_number, y = average,
             group = measure, colour = measure)) +
  geom_line(size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 52)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(5000, 15000),
                     labels = scales::comma_format()) +
  scale_colour_manual(guide = "none",
                      values = c("#008080", "#0941B3", "#fe8c11")) +
  annotate("text", x = 8, y = 13000, hjust = 0, family = "Spline Sans",
           label = "Average of 2017 to 2021",
           size = 7, colour = "#0941B3", fontface = "bold") +
  annotate("text", x = 8, y = 9000, hjust = 0, family = "Spline Sans",
           label = "Average of 2016 to 2019 & 2021",
           size = 7, colour = "#fe8c11", fontface = "bold") +
  annotate("text", x = 8, y = 8000, hjust = 0, family = "Spline Sans",
           label = "Average of 2015 to 2019",
           size = 7, colour = "#008080", fontface = "bold") +
  labs(title = ons_graph_title,
       subtitle = str_wrap(ons_graph_subtitle, width = 150),
       caption = ons_graph_caption,
       x = "Week number",
       y = "Average weekly death registrations")

## Saving the graph
ggsave(file = "/cloud/project/Mortality/ons_deaths_baseline.jpeg",
       plot = ons_deaths_baseline_gg,
       device = "jpeg",
       height = 1000/96, width = 2000/96, dpi = 96)