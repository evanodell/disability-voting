pacman::p_load(readr)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)

system.time(
  bes13 <- read_rds("bes13.rds")
)

summary(bes13$profile_eurefvote)

eu_vote_dis <- bes13[bes13$profile_eurefvote!="Don't know"
                     & is.na(bes13$profile_eurefvote)==FALSE,]


#tibble::as.tibble(data.matrix(xtabs(eu_vote_dis$wt_full_W7W8W9 ~ eu_vote_dis$profile_eurefvote + eu_vote_dis$disability)))

eu_votes_disability <- eu_vote_dis %>% 
  group_by(disability, profile_eurefvote) %>%
  tally(wt_full_W7W8W9) %>%
  ungroup()  %>%
  group_by(disability) %>%
  mutate(percent=n/sum(n)) %>%
  group_by(disability) %>%
  mutate(total_percent=n/sum(n))
         

summary(eu_votes_disability$profile_eurefvote)

eu_votes_disability <- eu_votes_disability[is.na(eu_votes_disability$disability)==FALSE,]

eu_votes_disability$disability <- factor(eu_votes_disability$disability, levels=c("No", "Yes, limited a little", "Yes, limited a lot"))

p_eu_vote_disab <- ggplot(eu_votes_disability, aes(x=factor(profile_eurefvote), y=percent, fill=factor(profile_eurefvote))) +
  geom_col() + 
  scale_fill_discrete(name=NULL) + 
  scale_y_continuous(labels=scales::percent, name="Vote Share", breaks = seq(0.1,0.7, by=0.1)) + 
  scale_x_discrete("EU Referendum Vote") + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") + 
  ggtitle("2016 EU Referendum Vote By Disability (British Election Study W7W8W9)") +
  facet_wrap(~disability) + 
  labs(caption = "Evan Odell | 2017 | CC-BY")

p_eu_vote_disab

ggsave(p_eu_vote_disab, filename = "p_eu_vote_disab.png", width = 24, units = "cm")
