## Script to analyse relationship between disability and vote


#pacman::p_load(haven)
pacman::p_load(readr)
pacman::p_load(tidyr)
pacman::p_load(nnet)
pacman::p_load(ggplot2)
pacman::p_load(Rmisc)
pacman::p_load(dplyr)

#bes13 <- read_dta("BES2017_W13_Panel_v1.0.dta")

#write_rds(bes13, "bes13.rds")

# lapply(bes13, class)
# 
# system.time(
#   bes13 <- as_factor(bes13, only_labelled=TRUE)
# )
# 
# summary(bes13$disability)
# 
# write_rds(bes13, "bes13.rds")

system.time(
  bes13 <- read_rds("bes13.rds")
)

# disability vote -------------------------------------------------------


bes_2017_constits <- parlitools::bes_2017

ge_2017_dis <- bes13[is.na(bes13$wt_new_W13)==FALSE
                     & bes13$generalElectionVoteW13!="I would not vote"
                     & bes13$generalElectionVoteW13!="Don't know"
                     #& is.na(bes13$disability)==FALSE
                     & is.na(bes13$generalElectionVoteW13)==FALSE,]

ge_2017_dis$disabled_bi <- ifelse(ge_2017_dis$disability=="No", FALSE, TRUE)

ge_2017_dis$generalElectionVoteW13 <- factor(ge_2017_dis$generalElectionVoteW13)
#disabled_voters_summary <- tibble::as.tibble(data.matrix(xtabs(ge_2017_dis$wt_new_W13~ge_2017_dis$generalElectionVoteW13+ge_2017_dis$disability)))

data.matrix(xtabs(ge_2017_dis$wt_new_W13~ge_2017_dis$generalElectionVoteW13))

#svyby(bes13$wt_new_W13~bes13$generalElectionVoteW13+bes13$disability, vartype="ci",method="beta")

#disabled_voters_2017$disability <- factor(disabled_voters_2017$disability)

disabled_voters_2017 <- ge_2017_dis %>% 
  group_by(disability, generalElectionVoteW13) %>%
  tally(wt_new_W13) %>%
   group_by(disability) %>%
   mutate(percent=n/sum(n)) %>%
  ungroup() %>%
  mutate(total_percent=n/sum(n),
         total = total_percent * sum(bes_2017_constits$total_vote_17))

disabled_voters_2017

summary(disabled_voters_2017$generalElectionVoteW13)

#disabled_voters_2017 <- disabled_voters_2017[is.na(disabled_voters_2017$generalElectionVoteW13)==FALSE,]

disabled_voters_2017$generalElectionVoteW13 <- factor(disabled_voters_2017$generalElectionVoteW13, levels=c("Conservative", "Labour", "Liberal Democrat", "Scottish National Party (SNP)", "United Kingdom Independence Party (UKIP)", "Green Party", "Plaid Cymru", "I would not vote", "Other", "Don't know", "British National Party (BNP)"))

disabled_voters_2017$generalElectionVoteW13 <- recode(disabled_voters_2017$generalElectionVoteW13,
                                                          "Scottish National Party (SNP)" = "SNP",
                                                          "United Kingdom Independence Party (UKIP)" = "UKIP",
                                                          "British National Party (BNP)" = "BNP")

#disabled_voters_2017 <- disabled_voters_2017[is.na(disabled_voters_2017$disability)==FALSE,]

disabled_voters_2017 <- disabled_voters_2017[is.na(disabled_voters_2017$disability)==FALSE,]

disabled_voters_2017$disability <- factor(disabled_voters_2017$disability, levels=c("No", "Yes, limited a little", "Yes, limited a lot"))#, NA))

party_colour <- c("Liberal Democrat" = "#FDBB30",
                  "Conservative" = "#0087DC",
                  "Labour" = "#DC241f",
                  "Green Party" = "#6AB023",
                  "Other" = "#2b2b2b",
                  "Plaid Cymru" = "#008142", 
                  "SNP" = "#FFFF00",
                  "UKIP" = "#70147A",
                  "BNP" = "#2e3b74",
                  "Don't know" = "#838383")

p_vote_disab <- ggplot(disabled_voters_2017, aes(x=factor(generalElectionVoteW13), y=percent, fill=factor(generalElectionVoteW13))) +
  geom_col() + 
  scale_fill_manual(values=party_colour, name=NULL) + 
  scale_y_continuous(labels=scales::percent, name="Popular Vote", breaks = seq(0.00,0.5, by=0.1)) + 
  scale_x_discrete("Party") + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") + 
  ggtitle("2017 General Election Vote By Disability (British Election Study W13)") +
  facet_wrap(~disability) + 
  labs(caption = "Evan Odell | 2017 | CC-BY")

p_vote_disab

ggsave(p_vote_disab, filename = "p_vote_disab_2017.png", width = 24, units = "cm")



# disability turnout ------------------------------------------------------

bes13$turnoutUKGeneralW12 <- factor(bes13$turnoutUKGeneralW12)

#disabled_turnout_summary <- tibble::as.tibble(data.matrix(xtabs(bes13$wt_new_W13~ bes13$genElecTurnoutRetroW13  + bes13$disability)))

disabled_turnout_2017 <- bes13 %>% 
  group_by(disability, turnoutUKGeneralW12) %>%
  tally(wt_new_W12) %>%
  group_by(disability) %>%
  mutate(percent=n/sum(n)) %>%
  ungroup() %>%
  mutate(total_percent=n/sum(n),
         total = total_percent * sum(bes_2017_constits$total_vote_17))

disabled_turnout_2017
sum(disabled_turnout_2017$percent)

disabled_turnout_2017 <- disabled_turnout_2017[is.na(disabled_turnout_2017$turnoutUKGeneralW12)==FALSE
                                               & is.na(disabled_turnout_2017$disability)==FALSE,]

disabled_turnout_2017$turnoutUKGeneralW12 <- factor(disabled_turnout_2017$turnoutUKGeneralW12, levels=c("Very likely that I would vote", "Fairly likely", "Neither likely nor unlikely", "Fairly unlikely", "Very unlikely that I would vote", "Don't know"))

disabled_turnout_2017 <- disabled_turnout_2017[is.na(disabled_turnout_2017$disability)==FALSE,]

disabled_turnout_2017$disability <- factor(disabled_turnout_2017$disability, levels=c("No", "Yes, limited a little", "Yes, limited a lot"))#, NA))

p_turnout_disab <- ggplot(disabled_turnout_2017, aes(x=factor(turnoutUKGeneralW12), y=percent, fill=factor(turnoutUKGeneralW12))) +
  geom_col() + 
  scale_fill_discrete(name="Voted?") + 
  scale_y_continuous(labels=scales::percent, name="Reported Likelihood of Voting", breaks = seq(0.00,1.0, by=0.1)) + 
  scale_x_discrete("Likelihood of Voting") + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom") + 
  ggtitle("2017 General Election Anticipated Turnout By Disability (British Election Study W12)") +
  facet_wrap(~disability) + 
  labs(caption = "Evan Odell | 2017 | CC-BY")

p_turnout_disab

ggsave(p_turnout_disab, filename = "p_turnout_disab_2017.png", width = 24, units = "cm")


