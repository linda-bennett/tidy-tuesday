library(extrafont)
loadfonts()
library(tidyverse)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

forest <- tuesdata$forest

forest_area <- tuesdata$forest_area

brazil_loss <- tuesdata$brazil_loss

soybean_use <- tuesdata$soybean_use

vegetable_oil <- tuesdata$vegetable_oil

brazil_longer <- brazil_loss %>%
  pivot_longer(
    cols = c(-entity, -code, -year),
    names_to = "category",
    values_to = "values"
  ) %>%
  mutate(category = toupper(str_replace_all(category, "_", " ")))

factor_values <- brazil_longer %>%
  filter(year == 2013) %>%
  arrange(values)


color_palette <- data.frame(
  category = factor_values$category,
  hex_codes  = c("#D0C09D",
                 "#D0C19D",
                 "#D4C99C",
                 "#D6CC9E",
                 "#D8D09B",
                 "#CCC681",
                 "#C0C49B",
                 "#96A658",
                 "#5C872D",
                 "#35582D",
                 "#2D5C2B"
  )
)

brazil_longer <- brazil_longer %>%
  left_join(color_palette,
            by = "category")
#28726E

animation <- brazil_longer %>%
  mutate(category = factor(category, factor_values$category)) %>%
  ggplot(aes(x = year, y = category, size = values, colour = hex_codes)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "white",
                                   family = "Arial Nova",
                                   face = "bold",
                                   size = 12),
        axis.text.x = element_text(colour = "white",
                                   family = "Arial Nova",
                                   face = "bold",
                                   size = 12),
        axis.line.y = element_line(size = 1, colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#ABA5A3"),
        ) +
  scale_x_continuous(breaks = brazil_loss$year) +
  scale_color_identity() +
  transition_states(year, wrap = FALSE) +
  shadow_mark() +
  enter_fade()

# animate in a two step process:
animate(animation, height = 600, width = 650)
anim_save("brazil-loss.gif")

