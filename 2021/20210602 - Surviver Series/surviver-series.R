library(tidyverse)
library(usmap)
library(imgpalr)
library(grid)
library(patchwork)
library(png)
library(showtext)


castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')


font_add_google("Sigmar One", "Sigmar")
font_add_google("Teko", "Teko")
showtext_auto()

# number of castaways per state
states <- castaways %>%
  group_by(state) %>%
  summarise(count = n())

# us long and lat data
us_states <- usmap::us_map(regions = "states") %>%
  left_join(states, by = c("full" = "state"))

set.seed(1)
x <- "2021\\20210602 - Surviver Series\\400px-Survivor.borneo.logo.png"

logo_tile <- readPNG("2021\\20210602 - Surviver Series\\149479a59eb768e7336dbb290fdbb863.png", native = TRUE)

# Create a paletter for the map
suvivor_palette <- image_pal(x[1], type = "div",
          saturation = c(0.75, 1), brightness = c(0.75, 1), plot = TRUE)

# Create map
my_plot <- ggplot(data=us_states, aes(x=x, y=y, fill=count, group = abbr)) + 
  geom_polygon(color = "white") +
  coord_fixed(clip="off") +
  scale_fill_gradient(low = "#2283CF", high = "#3CAC46") +
  labs(fill = "Contestants",
       subtitle = "Only 5 of the 50 states did not have a single contestant") +
  annotate(geom = "text", x = -2.5e+06, y= -0.9e+06, hjust=0.5, label="208 of the 744 \n contestants are \n from California", family="Teko", size=12, lineheight = 0.3) +
  geom_curve(x = -2.5e+06, y= -0.5e+06, xend = -2e+06, yend= -0.5e+06, arrow = arrow(length=unit(0.03, "npc")), curvature = -0.5) +
  annotate(geom = "text", x = 1644633, y= 1e+06, hjust=0, label="New York has the second highest \n number of contestants, with 68", family="Teko", size=12, lineheight = 0.3) +
  geom_curve(x = 1.9e+06, y= 0.8e+06, xend = 1.8e+06, yend= 19459.823, arrow = arrow(length=unit(0.03, "npc")), curvature = 0.2) +
  theme_void() +
  theme(text = element_text(family="Teko", size = 40),
        plot.margin = margin(0, 0, 0, 25),
        plot.subtitle = element_text(hjust = 0.4))

# create logo
logo <- ggplot() +
  background_image(logo_tile) +
  theme_void() +
  coord_fixed()

#final plot
final_plot <- (wrap_elements(grid::textGrob('Survivor Contestants \n by State', gp=gpar(fontfamily="Sigmar", fontsize = 70, lineheight = 0.3))) + logo) / my_plot + 
  plot_layout(heights = c(0.2, 1)
  ) +
  plot_annotation(theme = theme(plot.background = element_rect(fill=NA, colour = 'black', size = 3),
                                text = element_text(size = 30)),
                  caption = "#TidyTuesday | Data: survivorR package | Graphic: Linda Bennett")
  

 
ggsave("2021\\20210602 - Surviver Series\\survivor.png", final_plot, dpi = 320)

