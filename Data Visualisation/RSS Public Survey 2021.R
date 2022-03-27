## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(curl)
library(readxl)
library(DirichletReg)
library(scales)
library(ggdist)
library(sysfonts)
library(showtext)

# Next, add the Frutiger LT font
font_add(family = "Frutiger LT Condensed",
         regular = "/cloud/project/Data Visualisation/fonts/Frutiger LT 47 Light Condensed.ttf",
         bold = "/cloud/project/Data Visualisation/fonts/Frutiger LT 67 Bold Condensed.ttf",
         italic = "/cloud/project/Data Visualisation/fonts/Frutiger LT 48 Light Condensed Italic.ttf",
         bolditalic = "/cloud/project/Data Visualisation/fonts/Frutiger LT 68 Bold Condensed Italic.ttf")

showtext_auto()

# Next, set the plotting theme
theme_clean_rss <- theme_bw(base_family = "Frutiger LT Condensed") + 
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
theme_set(theme_clean_rss)

## Drawing and tidying the data
# We draw the figures straight from the online file
opinium_url <- "https://rss.org.uk/RSS/media/File-library/News/2022/Opinium-RSS-Survey-of-public.xlsx"
opinium_sheet <- "OP18302_Q1_"
opinium_range <- "A3:B18"
number_sims <- 10000

temp <- tempfile()
temp <- curl::curl_download(url = opinium_url, destfile = temp,
                           quiet = TRUE, mode = "wb")

opinium_survey_df <- read_excel(temp,
                          sheet = opinium_sheet,
                          range = opinium_range) %>%
  dplyr::rename(response = 1, weighted = 2) %>%
  tidyr::fill(response, .direction = "down") %>%
  dplyr::slice(-1) %>%
  dplyr::filter(weighted > 1) %>%
  dplyr::mutate(
    company = "Opinium",
    population = "UK adults",
    question = "Question 1",
    response = case_when(response == "Don’t know" ~ "Don't Know",
                         TRUE ~ response))

# We draw from the posterior Dirichlet distribution
opinium_dirichlet_df <- DirichletReg::rdirichlet(
  n = number_sims,
  alpha = opinium_survey_df$weighted + 1) %>%
  as_tibble(.name_repair = "minimal") %>% 
  dplyr::rename("15%" = 1, "25%" = 2, "40%" = 3, "50%"= 4,
                "75%" = 5, "Other" = 6, "Don't Know" = 7) %>%
  dplyr::mutate(draw = 1:number_sims) %>%
  tidyr::pivot_longer(cols = 1:7,
                      names_to = "response",
                      values_to = "share")

opinium_dirichlet_df$response <- factor(
  opinium_dirichlet_df$response,
  levels = c("15%", "25%", "40%", "50%",
             "75%", "Other", "Don't Know"))

# We can summarise those distributions in a table
opinium_summary_df <- opinium_dirichlet_df %>%
  group_by(response) %>%
  mean_hdi(share) %>%
  mutate(mean = 100*share, lower = 100*.lower, upper = 100*.upper) %>%
  select(response, mean, lower, upper)

## Creating the graph
opinium_title <- "In Opinium's survey, around one in four UK adults gave the right answer to the coin toss question."
opinium_subtitle <- "Estimated shares of UK adults giving answers to Opinium's online survey question: 'If you toss a fair coin, what is the probability of getting two heads?'. Posterior distributions with 95% mean highest density intervals, with a uniform prior."
opinium_caption <- "Survey data: Opinium (online survey of 2,001 UK adults), 17th to 21st December 2021."

opinium_survey_summary_gg <- opinium_dirichlet_df %>% 
  ggplot(aes(x = share, y = fct_rev(response))) +
  stat_dotsinterval(point_interval = mean_hdi,
                    quantiles = 100, point_size = 3,
                    alpha = 0.5, point_alpha = 1, interval_alpha = 1,
                    fill = "#00bcf2", slab_colour = "#00bcf2") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.5),
                     expand = c(0,0)) +
  annotate("rect", xmin = 0, xmax = 0.5,
           ymin = 5.6, ymax = 6.4,
           alpha = 0.1, fill = "#d3a41b") +
  annotate("text", x = 0.05, y = 6,
           label = "Correct answer", 
           family = "Frutiger LT Condensed",
           hjust = 0, fontface = "bold",
           colour = "black", size = 5) +
  labs(title = opinium_title,
       subtitle = str_wrap(opinium_subtitle, width = 125),
       x = "Shares of UK adults",
       y = "Response options",
       caption = opinium_caption)