## Packages and Themes
library(tidyverse)
library(readxl)
library(googlesheets4)
library(curl)
library(sf)
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
# The table is in a Google Sheet file
survation_topissue_url <- "https://docs.google.com/spreadsheets/d/1oGsSNopxoRwbG2sVJpAY4a8kKYVZxlmcfVVcyGdJUv4/edit#gid=0"
survation_topissue_range <- "A1:F633"
gs4_deauth()
survation_topissue_tbl <- read_sheet(
  ss = survation_topissue_url,
  range = survation_topissue_range)

# Next, set the graph labels
survation_topissue_title <- "In around 460 constituencies in Great Britain, the economy was the top issue."
survation_topissue_subtitle <- "Survation surveyed 14,304 people in Great Britain between 16th February and 30th March 2022. Respondents had a list of 22 issues and were asked to pick the two most important issues facing the country right now. Analysts at Royal Holloway used a model to estimate the top issue in each constituency. The map is a non-contiguous cartogram: the space between blocks does not represent missing data. "
survation_topissue_caption <- "Source: Survation and Royal Holloway MRP Top Issue Estimates, Q1 2022.\nUse of the cartogram is under v3.0 of the Open Parliament License."

# Here, we use the maps from Carl Baker (House of Commons Library)
# https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous
hocl_map <- tempfile()
hocl_constituency_source <- "https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/Constituencies.gpkg"
hocl_map <- curl_download(
  url = hocl_constituency_source,
  destfile = hocl_map, quiet = TRUE, mode = "wb")

hocl_background <- st_read(hocl_map, layer = "5 Background") %>%
  filter(Name != "Ireland")

hocl_constituencies <- st_read(hocl_map, layer = "4 Constituencies") %>%
  filter(RegionNati != "Northern Ireland")

hocl_group_outlines <- st_read(hocl_map, layer = "2 Group outlines") %>%
  filter(RegionNati != "Northern Ireland")

# Join the data table to the map information
survation_topissue_df <- left_join(
  survation_topissue_tbl, hocl_constituencies,
  by = c("ONSConstID" = "pcon.code")) %>%
  janitor::clean_names()

## Making the map
survation_topissue_cartogram_gg <- ggplot() +
  geom_sf(data = hocl_background, aes(geometry = geom),
          fill = "grey90") +
  geom_sf(data = survation_topissue_df,
          aes(geometry = geom, fill = most_important_issue)) +
  geom_sf(data = hocl_group_outlines, aes(geometry = geom),
          fill = NA, colour = "black") +
  scale_fill_manual(
    name = "Modelled most important issue",
    values = c("#faed5a", "#fe8c11", "#0941b3", "#008080", "#a6051a")) +
  labs(title = survation_topissue_title,
       subtitle = str_wrap(survation_topissue_subtitle, width = 105),
       x = "",
       y = "",
       caption = survation_topissue_caption) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        plot.caption.position = "plot")
