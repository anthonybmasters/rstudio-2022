## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(lubridate)
library(scales)

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
# We read the data straight from the online flat file
covid_owid_df <- read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Next, filter on rows and select the columns
lux_uk_owid_df <- covid_owid_df %>%
  dplyr::filter(location %in% c("Luxembourg", "United Kingdom"),
                date >= as_date("2021-06-01"),
                date <= as_date("2022-02-13")) %>%
  dplyr::select(location, date, new_cases_smoothed_per_million)

## Create the graph
# For this graph, we want a joined line across a missing value
lux_uk_cases_gg <- lux_uk_owid_df %>%
  drop_na() %>%
  ggplot(aes(x = date, y = new_cases_smoothed_per_million,
             group = location)) +
  geom_line(aes(colour = location), size = 1.5) +
  scale_colour_manual(guide = "none",
                      values = c("#fe8c11", "#008080")) +
  scale_x_date(date_labels = "%d %b\n%Y",
               date_breaks = "3 months",
               expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0)) +
  labs(title = "Both Luxembourg and the UK had large winter waves of confirmed cases.",
       subtitle = str_wrap("7-day rolling averages of new confirmed SARS-CoV-2 cases per million people by reporting date. Testing limitations mean confirmed cases are less than the true number of infections.",
                           width = 100),
       x = "Reporting date",
       y = "Confirmed cases per million (7DA)",
       caption = "Source: Our World in Data - COVID-19 Pandemic Data Explorer.") +
  annotate("text", x = as_date("2021-11-18"), y = 2680,
           label = "United Kingdom", colour = "#008080",
           hjust = 0, size = 7, fontface = "bold") +
  annotate("text", x = as_date("2021-12-18"), y = 3500,
           label = "Luxembourg", colour = "#fe8c11",
           hjust = 0, size = 7, fontface = "bold") +
  annotate("curve", x = as_date("2022-01-15"), xend = as_date("2022-02-01"),
           y = 500, yend = 1000, arrow = arrow(),
           curvature = -0.2, size = 1.2, colour = "#008080") +
  annotate("text", x = as_date("2022-01-01"), y = 300,
           label = "A missing value\ncreates distortions",
           hjust = 0, size = 6)