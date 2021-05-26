library(tidyverse)
library(tidytext)
library(lubridate)
library(plotly)
library(cowplot)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)


netflix <- tuesdata$netflix

netflix %>% 
  mutate(listed_in = strsplit(listed_in, ", ")) %>% 
  unnest(listed_in) %>%
  group_by(listed_in) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

top_countries_tv %>%
  ggplot() +
  geom_point(mapping=aes(y=reorder(country, -order), x=0, size = count), shape=15) +
  theme_void() +
  theme(legend.position = 'none') +
  geom_text(aes(x=0, y=country, hjust=0, label = country), size=2, color="#E94F64", fontface="bold", family="Aleg")


top_countries_mv %>%
  ggplot() +
  geom_point(mapping=aes(y=reorder(country, -order), x=0, size = count), shape=15) +
  theme_void() +
  theme(legend.position = 'none') +
  geom_text(aes(x=0, y=country, hjust=0, label = country), size=2, color="#E94F64", fontface="bold", family="Aleg")


listed <- netflix %>% 
  mutate(listed_in = strsplit(listed_in, ", ")) %>% 
  unnest_wider(listed_in)


group_1 <- listed %>%
  group_by(...1, ...2) %>%
  summarise(count = n()) %>%
  rename(cat_1 = ...1,
         cat_2 = ...2)

group_2 <- listed %>%
  group_by(...2, ...3) %>%
  summarise(count = n()) %>%
  filter(is.na(...3)==FALSE) %>%
  rename(cat_1 = ...2,
         cat_2 = ...3)

group_3 <- listed %>%
  group_by(...1, ...3) %>%
  summarise(count = n()) %>%
  filter(is.na(...3)==FALSE) %>%
  rename(cat_1 = ...1,
         cat_2 = ...3)

group_4 <- listed %>%
  group_by(...2, ...1) %>%
  summarise(count = n()) %>%
  filter(is.na(...2)==FALSE) %>%
  rename(cat_1 = ...2,
         cat_2 = ...1)

group_5 <- listed %>%
  group_by(...3, ...1) %>%
  summarise(count = n()) %>%
  filter(is.na(...3)==FALSE) %>%
  rename(cat_1 = ...3,
         cat_2 = ...1)

group_6 <- listed %>%
  group_by(...3, ...2) %>%
  summarise(count = n()) %>%
  filter(is.na(...3)==FALSE,
         is.na(...2)==FALSE) %>%
  rename(cat_1 = ...3,
         cat_2 = ...2)


all_groups <- group_1 %>%
  rbind(group_2) %>%
  rbind(group_3) %>%
  rbind(group_4) %>%
  rbind(group_5) %>%
  rbind(group_6) %>%
  group_by(cat_1, cat_2) %>%
  summarise(count = sum(count))


action_links <- all_groups %>%
  select(source = cat_1, target = cat_2, importance = count) %>%
  unique()  %>% filter(source == "Action & Adventure",
                       target != "Action & Adventure")


international_links <- all_groups %>%
  select(source = cat_1, target = cat_2, importance = count) %>%
  unique()  %>% filter(source == "International Movies",
                       target != "International Movies")


comedies_links <- all_groups %>%
  select(source = cat_1, target = cat_2, importance = count) %>%
  unique()  %>% filter(source == "Comedies",
                       target != "Comedies")


scifi_links <- all_groups %>%
  select(source = cat_1, target = cat_2, importance = count) %>%
  unique()  %>% filter(source == "Sci-Fi & Fantasy",
                       target != "Sci-Fi & Fantasy")


# Libraries
library(ggraph)
library(igraph)
library(tidyverse)


action_edges <- action_links %>%
  select(from = source, to = target)

action_vertices <- action_links %>%
  ungroup() %>%
  select(name=target, size=importance) %>%
  rbind(tibble(name="Action & Adventure", size = 0))


international_edges <- international_links %>%
  select(from = source, to = target)

international_vertices <- international_links %>%
  ungroup() %>%
  select(name=target, size=importance) %>%
  rbind(tibble(name="International Movies", size = 0))

comedies_edges <- comedies_links %>%
  select(from = source, to = target)

comedies_vertices <- comedies_links %>%
  ungroup() %>%
  select(name=target, size=importance) %>%
  rbind(tibble(name="Comedies", size = 0))

scifi_edges <- scifi_links %>%
  select(from = source, to = target)

scifi_vertices <- scifi_links %>%
  ungroup() %>%
  select(name=target, size=importance) %>%
  rbind(tibble(name="Sci-Fi & Fantasy", size = 0))


# Then we have to make a 'graph' object using the igraph library:
action_graph <- graph_from_data_frame(action_edges, vertices=action_vertices)
international_graph <- graph_from_data_frame(international_edges, vertices=international_vertices)
comedies_graph <- graph_from_data_frame(comedies_edges, vertices=comedies_vertices)

scifi_graph <- graph_from_data_frame(scifi_edges, vertices=scifi_vertices)


ggraph(action_graph, layout='dendrogram', circular=TRUE) + 
  geom_edge_diagonal(edge_colour = "#db0510") +
  geom_node_point(aes(size=size, color="#db0510")) +
  geom_node_text(aes(label=name, color="#db0510"), nudge_y=-0.1) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black", colour = "black")) 


ggraph(international_graph, layout='dendrogram', circular=TRUE) + 
  geom_edge_arc(edge_colour = "#db0510", 
                start_cap = circle(10, 'mm'),
                end_cap = circle(10, 'mm')) +
  geom_node_point(aes(size=size, color="#db0510")) +
  geom_node_text(aes(label=name, color="#db0510"), nudge_y=-0.1) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black", colour = "black")) 


ggraph(comedies_graph, layout='dendrogram', circular=TRUE) + 
  geom_edge_diagonal(edge_colour = "#db0510") +
  geom_node_point(aes(size=size, color="#db0510")) +
  geom_node_text(aes(label=name, color="#db0510"), nudge_y=-0.1) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black", colour = "black")) 

ggraph(scifi_graph, layout='dendrogram', circular=TRUE) + 
  geom_edge_diagonal(edge_colour = "#db0510") +
  geom_node_point(aes(size=size, color="#db0510")) +
  geom_node_text(aes(label=name, color="#db0510"), nudge_y=-0.1) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black", colour = "black")) 


ggraph(comedies_graph, layout='dendrogram', circular=TRUE) + 
  geom_edge_diagonal(edge_colour = "#db0510") +
  geom_node_point(aes(size=size, color="#db0510")) +
  geom_node_text(aes(label=name, color="#db0510"), nudge_y=-0.1) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="black", colour = "black")) 



ggraph(comedies_graph, layout = 'circlepack', weight=size) + 
  geom_node_circle(color="red", fill="black") +
  theme_void() +
  geom_node_text(aes(label=name, size=size), color="red") +
  theme(plot.background = element_rect(fill="black"))

ggraph(action_graph, layout = 'circlepack', weight=size) + 
  geom_node_circle(color="red", fill="black") +
  theme_void() +
  geom_node_text(aes(label=name, size=size), color="red") +
  theme(plot.background = element_rect(fill="black"))

ggraph(international_graph, layout = 'circlepack', weight=size) + 
  geom_node_circle(color="red", fill="black") +
  theme_void() +
  geom_node_text(aes(label=name, size=size), color="red") +
  theme(plot.background = element_rect(fill="black"))

