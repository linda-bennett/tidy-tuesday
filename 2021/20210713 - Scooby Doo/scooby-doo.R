library(extrafont)
library(tidyverse)
library(ggpubr)
library(ggwordcloud)
library(patchwork)
library(grid)
library(palr)

# Data loading and wrangling ----------------------------------------------

# load in data
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)

scooby_doo <- tuesdata$scoobydoo

# font_import(paths = "C:/Users/urUserName/AppData/Local/Microsoft/Windows/Fonts"

word_cols <- c("jeepers", "jinkies", "my_glasses", "just_about_wrapped_up", "zoinks", "groovy", "scooby_doo_where_are_you", "rooby_rooby_roo")

words <- scooby_doo %>%
  select(date_aired, jeepers:rooby_rooby_roo) %>%
  mutate(
    across(word_cols, ~ case_when(. == "NULL" ~ "0", TRUE ~ .))
  ) %>%
  mutate(
    across(word_cols, ~as.character(.x) %>% as.integer())) %>%
  pivot_longer(cols = word_cols,
               names_to = "word",
               values_to = "count")


words_grouped <- words %>%
  group_by(word) %>%
  summarise(count = sum(count)) %>%
  ungroup()


# Visualisation -----------------------------------------------------------

set.seed(42)
x <- "2021/20210713 - Scooby Doo/scooby_van_front.png"

# Create a paletter for the map
scooby_palette <- imgpalr::image_pal(x[1], type = "seq", n=8,
                            saturation = c(0.75, 1), brightness = c(0.75, 1), plot = TRUE)

ggplot(words_grouped %>%
         mutate(word = toupper(str_replace_all(word, "_", " "))),
       aes(label = word, size = count, color = word)) +
  geom_text_wordcloud_area(shape = "square", family = "Scooby Doo") +
  scale_size_area(max_size = 40) +
  scale_color_manual(values = scooby_palette) +
  draw_image(x,  x = 0.4, y = 0.4, scale = .3) +
  theme_minimal() +
  theme(plot.title = element_text(family = "Scooby Doo", size = 30, colour = "white"),
        plot.background = element_rect(fill="black", colour = 'black'),
        plot.caption = element_text(size = 10, colour = "white")) +
  labs(title = 'Jinkies, zoinks is the most \n popular phrase in Scooby Doo...', 
       caption = "#TidyTuesday | Data: Kaggle | Graphic: Linda Bennett")

ggsave( "2021/20210713 - Scooby Doo/scooby_vis.png")

