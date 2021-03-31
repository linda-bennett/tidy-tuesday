library(tidyverse)
library(ggtext)
library(patchwork)
library(cowplot)
library(ggridges)

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

allShades <- tuesdata$allShades %>%
  arrange(lightness)
allNumbers <- tuesdata$allNumbers
allCategories <- tuesdata$allCategories

#Create color scale for use in plots.
shade_scale = c(head(allShades$hex,1), nth(allShades$hex, (nrow(allShades)/8)), nth(allShades$hex, (nrow(allShades)/4)), nth(allShades$hex, (nrow(allShades)/2)), nth(allShades$hex, (nrow(allShades)/2)+(nrow(allShades)/4)), nth(allShades$hex, (nrow(allShades)/2)+(nrow(allShades)/4)+(nrow(allShades)/8)), tail(allShades$hex,1))


#Density plot to see the difference between brands with different foundation ordering
density_plot <- allNumbers %>%
  mutate(brand = toupper(brand),
         number_system = case_when(lightToDark == TRUE ~ "LIGHT TO DARK",
                                   lightToDark == FALSE ~ "DARK TO LIGHT",
                                   TRUE ~ "UNORDERED")) %>%
  ggplot(aes(x=lightness, y=number_system)) +
  geom_density_ridges_gradient(aes(fill = ..x..)) +
  theme_minimal() +
  scale_fill_gradientn(colors =shade_scale) +
  labs(x = "LIGHTNESS <br> <span style = 'text-align:left;'><b style='font-size:8pt;'>***Scale 0-1*** </b></span>") +#
  theme(axis.title.x = element_markdown(family = "Verdana",
                                    hjust = 1,
                                    size=12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(vjust = -2,
                                   hjust = 0,
                                   margin = margin(l = 50, r = -150),
                                   family="Verdana",
                                   size=12),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')


# Function to create multiple shade variety circles.
create_color_circle <- function(.data, brand_filter, chart_label){
  
  .data %>%
    filter(brand %in% brand_filter) %>%
    mutate(idx = row_number()) %>%
    mutate(idy = seq_along(id)) %>%
    ggplot(aes(idx, idy, fill = hex)) +
    geom_tile() +
    scale_fill_identity() +
    ylim(c(-1.5, 1.5)) +
    geom_text(x = 132,
              y = -1.5,
              size = 3,
              family = "Verdana",
              label = chart_label) +
    coord_polar() +
    theme_void()

}

cq <- create_color_circle(allShades, "Clinique", "Clinique")
bm <- create_color_circle(allShades, "bareMinerals", "bareMinerals")
itc <- create_color_circle(allShades, "It Cosmetics", "It Cosmetics")
tarte <- create_color_circle(allShades, "Tarte", "Tarte")

exa <- create_color_circle(allShades, "Exa", "Exa")
jp <- create_color_circle(allShades, "Juvia's Place", "Juvia's Place")
pac <- create_color_circle(allShades, "Pacifica", "Pacifica")
bb <- create_color_circle(allShades, "Beauty Bakerie", "Beauty Bakerie")

header <- ggdraw() +
  draw_text("MAKEUP SHADE ORDERING", x= 0.5, y = 0.8, size = 20, family = "Verdana") +
  draw_text("Beauty brands often label foundation shades with sequential numbers, giving a priority to the shades that come first. \nIn most cases these numbers are labelled from light to dark, giving lighter shades and users of those shades precedence. \n\nFor products that deviate from this, using a dark to light sequence instead, \nthere is a more even spread of dark and light shades available.", 
            x = 0.5, y = 0.4, size = 12, family = "Verdana Pro Light")

darklight_header <- ggdraw() +
  draw_text("Most brands put lighter shades first, with only 4 of 64 brands ordering from dark to light shades:", 
            x = 0.5, y = 0.1,size = 12, family = "Verdana Pro Light")

lightdark_header <- ggdraw() +
  draw_text("Here are some examples of the shade variety for brands that use a light to dark ordering:", 
            x = .5, y = 0.1, size = 12, family = "Verdana Pro Light")


final_plot <- header / density_plot / darklight_header / (exa | jp | pac | bb) / lightdark_header /(cq | bm | itc | tarte) + plot_layout(heights = c(0.4, 0.6, 0.1, 0.4, 0.1, 0.4))
