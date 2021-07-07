library(tidyverse)
library(skimr)
library(patchwork)
library(countrycode)
library(showtext)
# set ggplot theme
# Add google fonts
font_add_google("Raleway", "Raleway")
showtext_auto()

theme_set(theme_wsj(base_size = 12, color = "brown", base_family = "Raleway", title_family = "Raleway"))


# Data loading and wrangling ----------------------------------------------

# load in data
tuesdata <- tidytuesdayR::tt_load(2021, week = 28)
# mutate data - change name of countries to match mapping data
holidays <- tuesdata$holidays %>%
  mutate(independence_from_yn = ifelse(is.na(independence_from)!=TRUE,TRUE,FALSE)) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Russian Soviet Federative Socialist Republic" = "Soviet Union")) %>% 
  mutate(independence_from = 
           recode_factor(independence_from, "Russian Soviet Federative Socialist Republic and German Empire" = "Soviet Union")) %>% 
  mutate(independence_from = 
           recode_factor(independence_from, "Spanish Empire" = "Spain")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "United Kingdom of Great Britain" = "United Kingdom")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Kingdom of Great Britain" = "United Kingdom")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "United Kingdom of Great Britain and Ireland" = "United Kingdom")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "United Kingdom of Portugal, Brazil and the Algarves" = "Portugal")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "United Kingdom and the British Mandate for Palestine" = "United Kingdom")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "SFR Yugoslavia" = "Yugoslavia")) %>% 
  mutate(independence_from = 
           recode_factor(independence_from, "Socialist Federal Republic of Yugoslavia" = "Yugoslavia")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Empire of Japan and France" = "Empire of Japan")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Spanish Empire[72]" = "Spain")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Soviet Union[80]" = "Soviet Union")) %>%
  mutate(independence_from = 
           recode_factor(independence_from, "Soviet Union[55]" = "Soviet Union")) %>%
  mutate(country = case_when(str_detect(country, "China") == TRUE ~ "China",
                             country == "Antigua and Barbuda" ~ "Antigua", #aDD bARBUDA
                             country == "Bahamas, The" ~ "Bahamas",
                             str_detect(country, "Congo") == TRUE ~  "Republic of Congo",
                             country == "East Timor" ~ "Timor-Leste",
                             country == "Gambia, The" ~ "Gambia",
                             country == "Netherlands, The" ~ "Netherlands",
                             country == "North Macedonia" ~ "Macedonia",
                             country == "Saint Kitts and Nevis" ~ "Saint Kitts", # add Nevis
                             country == "Saint Vincent and the Grenadines" ~ "Saint Vincent", #Add Grenadines
                             country == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
                             country == "Trinidad and Tobago" ~ "Trinidad",
                             country == "Vatican City" ~ "Vatican",
                             TRUE ~ country)
  )

holidays$continent <- countrycode(sourcevar = holidays[["country"]],
                                  origin = "country.name",
                                  destination = "continent")

holidays$continent_independence_from <- countrycode(sourcevar = holidays[["independence_from"]],
                                  origin = "country.name",
                                  destination = "continent")


# load in map data
mapdata <- map_data("world") %>%
  filter(!(region=="Antarctica")) %>%
  filter(!(region=="Greenland")) %>%
  filter(!(region=="French Southern and Antarctic Lands")) %>% 
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))



# maps --------------------------------------------------------------------

map_independence <- holidays %>%
  filter(independence_from_yn == TRUE) %>%
  select(country) %>%
  unique() %>%
  ggplot() + 
  geom_map(dat = mapdata, map = mapdata, aes(map_id=region), fill="grey") +
  geom_map(aes(map_id=country, fill = "#481567FF"), map=mapdata) +
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "gilbert", xlim = c(-180, 180)) +
  ggtitle(label = 'These Countries Gained Independence...') +
  # ggthemes::theme_map() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 40, lineheight = 0.2)) +
  scale_fill_identity()

map_independence_from <- holidays %>%
  select(independence_from) %>%
  unique() %>%
  ggplot() + 
  geom_map(dat = mapdata, map = mapdata, aes(map_id=region), fill="grey") +
  geom_map(aes(map_id=independence_from, fill = "#FDE725FF"), map=mapdata) +
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "gilbert", xlim = c(-180, 180)) +
  ggtitle(label = '...From These Countries') +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 40, lineheight = 0.2)) +
  scale_fill_identity()


# continent bar charts ----------------------------------------------------

continent_independence <- holidays %>%
  filter(independence_from_yn == TRUE,
         is.na(continent) == FALSE) %>%
  group_by(continent) %>%
  summarise(count = n()) %>%
  ggplot(aes(y=reorder(continent, count), x = count)) +
  geom_bar(stat = 'identity', fill = '#481567FF') +
  ggtitle(label = 'The majority of countries that gained \n independence are on the African continent') +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0),
        plot.title = element_text(size = 40, lineheight = 0.2))
  
continent_independence_from <- holidays %>%
  filter(is.na(continent_independence_from) == FALSE) %>%
  group_by(continent_independence_from) %>%
  summarise(count = n()) %>%
  ggplot(aes(y=reorder(continent_independence_from, count), x = count)) +
  geom_bar(stat = 'identity', fill = '#FDE725FF') +
  ggtitle(label = 'The vast majority of independence was gained \n from countries on the European continent') +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0),
        plot.title = element_text(size = 40, lineheight = 0.2))



# gained independence and independence from countries plot ----------------


independence_both <- holidays %>%
  filter(independence_from_yn == TRUE) %>%
  select(country) %>% unique() %>%
  inner_join(
    holidays %>%
      select(independence_from) %>%
      unique(),
    by = c("country" = "independence_from")
  )


independence_both_data <- holidays %>%
  filter(country %in% independence_both$country |
           independence_from %in% independence_both$country)



independence_both_plot <- ggplot() +
  geom_point(data = subset(holidays, country %in% independence_both$country),
             aes(y = reorder(country, desc(year)), 
                 x = year),
             color = '#481567FF',
             size = 4) +
  geom_text(data = subset(holidays, country == "Colombia"),
            aes(y = country,
                x = year,
                label = "Year independence was gained"),
            size = 12,
            family = "Raleway",
            nudge_x = -150,
            nudge_y = -2,
            hjust = 0,
            vjust = 0) +
  # geom_text(data = subset(holidays, country %in% independence_both$country),
  #           aes(y = country,
  #               x = year,
  #               label = independence_from),
  #           hjust = 1,
  #           vjust = 2) +
  geom_point(data = subset(holidays, independence_from %in% independence_both$country),
             aes(y = independence_from,
                 x = year),
             color = "#FDE725FF",
             size = 4) +
  geom_text(data = subset(holidays, independence_from == "Netherlands"),
            aes(y = independence_from,
                x = year,
                label = "Year countries gained \n independence from them"),
            size = 12,
            lineheight = 0.2,
            family = "Raleway",
            nudge_x = -120,
            nudge_y = 0.5,
            hjust = 0,
            vjust = 1) +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0)) +
  geom_curve(aes(x = 1700, y = 4.5, xend = 1830, yend = 6),arrow = arrow(length = unit(0.5,"cm"))) +
  geom_curve(aes(x = 1750, y = 7.2, xend = 1805, yend = 7),arrow = arrow(length = unit(0.5,"cm")), curvature = -0.5) +
  geom_curve(aes(x = 1850, y = 9.2, xend = 1825, yend = 9),arrow = arrow(length = unit(0.5,"cm")), curvature = 0) +
  geom_curve(aes(x = 1910, y = 9, xend = 1980, yend = 8),arrow = arrow(length = unit(0.5,"cm")), curvature = 0) +
  ggtitle(label ='There are 10 countries that appear on both sides of this. They gained independence from a country, \n and other countries have gained independence from them.') +
  theme(plot.title = element_text(size = 40, lineheight = 0.2))
  # geom_text_repel(data = subset(holidays, independence_from %in% independence_both$country),
  #           aes(y = independence_from,
  #               x = year,
  #               label = country),
  #           hjust = -0.5,
  #           vjust = 1) 


# final plot --------------------------------------------------------------


layout <- "
AAABBB
AAABBB
CCCDDD
EEEEEE
"

final_plot <- map_independence + map_independence_from +
  continent_independence + continent_independence_from + independence_both_plot +
  plot_annotation('Independence Across The Globe') +
  plot_layout(design = layout) & theme(text = element_text(size = 40),
                                       plot.title = element_text(hjust = 0.5))

  

ggsave("2021\\20210706 - Independence Days\\independence_days.png", final_plot, dpi = 320, width = 10, height = 10)
