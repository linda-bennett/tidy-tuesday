library(tidyverse)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues

yes_votes <- unvotes %>%
  left_join(issues, by = "rcid") %>%
  left_join(roll_calls, by = "rcid") %>%
  group_by(issue, vote) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count) * 100) %>% 
  filter(vote == "yes",
         is.na(issue) == FALSE)

ggplot(yes_votes,
       aes(x=prop,
           y=reorder(issue, prop),
           fill=factor(ifelse(issue=="Human rights", "Highlighted", "Normal"))))+
  geom_bar(stat="identity") +
  scale_fill_manual(name = "issue", values=c("#FFC300","grey50")) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(x=prop, hjust=1.05, label = issue)) +
  labs(title = "UN votes on Human Rights have the lowest proportion of yes votes",
         subtitle = "Proportion of all UN yes votes by issue")
