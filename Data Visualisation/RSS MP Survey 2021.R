## Packages and Themes
# First, install the packages we need
library(tidyverse)
library(sysfonts)
library(showtext)
library(curl)
library(readxl)
library(lubridate)
library(DescTools)
library(scales)

# Next, add the Frutiger LT font
font_add(family = "Frutiger LT Condensed",
         regular = "/cloud/project/Data Visualisation/fonts/Frutiger LT 47 Light Condensed.ttf",
         bold = "/cloud/project/Data Visualisation/fonts/Frutiger LT 67 Bold Condensed.ttf",
         italic = "/cloud/project/Data Visualisation/fonts/Frutiger LT 48 Light Condensed Italic.ttf",
         bolditalic = "/cloud/project/Data Visualisation/fonts/Frutiger LT 68 Bold Condensed Italic.ttf")

showtext_auto()

# After that, set the plotting theme
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
# We draw the data table straight from the online file
savanta_comres_url <- "https://2sjjwunnql41ia7ki31qqub1-wpengine.netdna-ssl.com/wp-content/uploads/2022/02/P010473-November_MPs-RSS-tables-V1.0.xlsx"
savanta_comres_range <- "A7:B30"

temp <- tempfile()
temp <- curl::curl_download(url = savanta_comres_url, destfile = temp,
                            quiet = TRUE, mode = "wb")

savanta_comres_tbl <- read_excel(temp,
                                 sheet = "Table 1",
                                 range = savanta_comres_range) %>%
  dplyr::rename(response = 1, reported_value = 2)

mp_savanta_base <- savanta_comres_tbl$reported_value %>% max(na.rm = TRUE)

mp_savanta_df <- savanta_comres_tbl %>%
  drop_na() %>%
  filter(!response %in% c("Unweighted row", "Total")) %>%
  mutate(response = case_when(response == 0.15 ~ "15%",
                              response == 0.25 ~ "25%",
                              response == 0.4 ~ "40%",
                              response == 0.5 ~ "50%",
                              response == 0.75 ~ "75%",
                              TRUE ~ as.character(response))) %>%
  mutate(company = "Savanta ComRes",
         population = "MPs",
         question = "Question 1",
         fw_start_date = as_date("2021-11-17"),
         fw_end_date = as_date("2022-01-18"),
         calc_share = mp_savanta_base*reported_value/sum(reported_value))

# The Ipsos MORI survey results come from this web page
# https://www.ipsos.com/en-uk/use-data-and-statistics
mp_ipsos_base <- 97

mp_ipsos_df <- tribble(
  ~response, ~reported_value,
  "15%", 2,
  "25%", 40,
  "40%", 1,
  "50%", 45,
  "75%", 1,
  "Other", 5,
  "Don't Know", 7) %>%
  mutate(company = "Ipsos MORI",
         population = "MPs",
         question = "Question 1",
         fw_start_date = as_date("2011-11-01"),
         fw_end_date = as_date("2011-12-20"),
         calc_share = mp_ipsos_base*reported_value/sum(reported_value))

# Next, we want to calculate approximate confidence intervals
# This is using the Sison-Glaz method
# https://www.jstor.org/stable/2291162
multinomial_ci_sg <- function(x, conf_level){
  multinomial_df <- DescTools::MultinomCI(x, conf.level = conf_level,
                                          sides = "two.sided",
                                          method = "sisonglaz") %>%
    as_tibble(.name_repair = "minimal") %>%
    dplyr::rename(est_central = est,
                  est_lower = lwr.ci,
                  est_upper = upr.ci)
}

mp_savanta_ci_df <- bind_cols(mp_savanta_df,
                              multinomial_ci_sg(mp_savanta_df$calc_share, 0.95))

mp_ipsos_ci_df <- bind_cols(mp_ipsos_df,
                            multinomial_ci_sg(mp_ipsos_df$calc_share, 0.95))

# Bind these rows together
mp_survey_summary_df <- dplyr::bind_rows(mp_savanta_ci_df,
                                         mp_ipsos_ci_df)

mp_survey_summary_df$response <- factor(mp_survey_summary_df$response,
                                        levels = c("15%", "25%", "40%", "50%",
                                                   "75%", "Other", "Don't Know"))
## Make the graph
# In R Markdown, fig.width = 15.0, fig.height = 7.5, fig.showtext = TRUE
mp_survey_summary_gg <- mp_survey_summary_df %>% 
  ggplot(aes(x = est_central, y = response,
             group = company)) +
  geom_pointrange(aes(xmin = est_lower, xmax = est_upper,
                      colour = company),
                  size = 1.1,
                  position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.65)) +
  scale_y_discrete(expand = c(0,0),
                   limits = rev) +
  scale_colour_manual(guide = "none",
                      values = c("#425ca9", "#f5aeb4")) +
  annotate("text", x = 0.15, y = 5.2, hjust = 0, fontface = "bold",
           label = "Winter 2021 (Savanta ComRes, online survey of 101 MPs)",
           colour = "#f5aeb4", size = 7) +
  annotate("text", x = 0.15, y = 4.8, hjust = 0, fontface = "bold",
           label = "Winter 2011 (Ipsos MORI, face-to-face survey of 97 MPs)",
           colour = "#425ca9", size = 7) +
  annotate("rect", xmin = 0, xmax = 0.62, ymin = 5.6, ymax = 6.4,
           alpha = 0.1, fill = "#d3a41b") +
  annotate("text", x = 0.05, y = 6, hjust = 0, fontface = "bold",
           label = "Correct answer",
           colour = "black", size = 5) +
  labs(title = "In the 2021 survey, around half of MPs gave the right answer to the coin toss question.",
       subtitle = str_wrap("Estimated shares of MPs giving answers to the Savanta ComRes question: 'If you toss a fair coin twice, what is the probability of getting two heads?'. The Ipsos MORI question in 2011 started with: 'If you spin a coin twice...'. Simultaneous confidence intervals are at 95% confidence, calculated through the Sison-Glaz method.", width = 125),
       x = "Shares of MPs",
       y = "Response options",
       caption = "Survey data: Savanta ComRes (online, 17th November 2021 to 18th January 2022)\nand Ipsos MORI (face-to-face, 1st November to 20th December 2011).")