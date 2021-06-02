library(tidyverse)

library(patchwork)

library(lubridate)


tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

drivers <- tuesdata$drivers

records <- tuesdata$records

summary(drivers)

summary(records)


records %>%
  group_by(track) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(aes(x=reorder(track, count),y=count,fill=track), stat="identity") +
  scale_fill_viridis_d() +
  coord_flip()


records %>%
  group_by(track) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(aes(x=reorder(track, count),y=count,fill=track), stat="identity") +
  scale_fill_viridis_d() +
  coord_flip()




records %>%
  group_by(track) %>%
  summarise(average = mean(time),
            min = min(time),
            max = max(time)) %>%
  ggplot() +
  geom_point(aes(x=reorder(track, average),y=average)) +
  geom_point(aes(x=track,y=min)) +
  geom_point(aes(x=track,y=max)) +
  scale_fill_viridis_d() +
  coord_flip()


records %>%
  ggplot() +
  geom_jitter(aes(x=time,y=reorder(track, time),color=type), alpha=0.3)
  

p1 <- records  %>%
  mutate(year = year(date)) %>%
  left_join(drivers %>%
              select(player, nation) %>%
              unique(),
            by = "player") %>%
  ggplot() +
  geom_bar(aes(x=year, fill=nation), position = "fill") +
  scale_fill_viridis_d(na.value="grey") +
  theme_minimal() +
  theme(panel.grid = element_blank())


p2 <- records %>%
  mutate(year = year(date)) %>%
  ggplot() +
  geom_bar(aes(x=year)) +
  geom_text(x=2000, y = 350, label = "381 WORLD RECORDS", hjust=0) +
  geom_curve(aes(x = 2000, xend = 1998, y=350, yend=380),
             arrow = arrow(length = unit(0.03, "npc")))+
  theme_void()

p2/p1


p3 <- drivers %>% 
  filter(is.na(records) != TRUE) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(aes(x=year, y=-count), stat="identity") +
  geom_text(x=1999, y = -20, label = "25 DRIVERS", hjust=0) +
  geom_curve(aes(x = 1999, xend = 1997.5, y=-20, yend=-23),
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.2) +
  theme_void()


p2/p3

ordering <- drivers %>%
  filter(is.na(records) != TRUE) %>%
  group_by(player, year) %>%
  summarise(records = sum(records, na.rm=TRUE)) %>%
  group_by(player) %>%
  summarise(count = n(),
            records = sum(records),
            first_year = min(year)) %>%
  arrange(records, first_year)


p4 <- drivers %>%
  mutate(player = factor(player, levels = ordering$player)) %>%
  group_by(player) %>%
  summarise(records = sum(records, na.rm=TRUE)) %>%
  ggplot() +
  geom_bar(aes(y=player, x=records), stat='identity') +
  theme_void()


p5 <- drivers %>%
  mutate(player = factor(player, levels = ordering$player)) %>%
  filter(is.na(records) != TRUE) %>%
  ggplot(aes(y=player, x=year)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none')


p6 <- drivers %>%
  mutate(player = factor(player, levels = ordering$player)) %>%
  filter(is.na(records) != TRUE) %>%
  group_by(player, year) %>%
  summarise(records = sum(records, na.rm=TRUE)) %>%
  group_by(player) %>%
  summarise(count = n(),
            records = sum(records)) %>%
  ggplot() +
  geom_bar(aes(y=player, x=count), stat='identity') +
  theme_void()


p5 |p4

p5 | p6

p5 +
  annotate(geom="text",
           x=2000,
           y=95,
           label="text")

records %>%
  mutate(player = factor(player, levels = ordering$player)) %>%
  filter(is.na(records) != TRUE) %>%
  ggplot(aes(y=player, x=date)) +
  geom_point(shape=124) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none')

records %>%
  ggplot(aes(x=date, y=track)) +
  geom_point(shape=124)


records %>%
  group_by(track) %>%
  filter(type == "Three Lap",
         shortcut == "No") %>%
  mutate(max_record_duration = case_when(
    record_duration == max(record_duration) ~ TRUE,
    TRUE ~ FALSE),
    broke_max_record = lag(max_record_duration, n=1)) 


test %>%
  filter(max_record_duration == TRUE | broke_max_record == TRUE) %>%
  ggplot(aes(x=date, y=track, label = player)) +
  geom_point() +
  geom_line() +
  geom_text(data = subset(test, broke_max_record == TRUE), hjust = -1) +
  geom_text(data = subset(test, max_record_duration == TRUE), hjust = 1.5)