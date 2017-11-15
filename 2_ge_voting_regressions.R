
pacman::p_load(readr)
pacman::p_load(dplyr)

system.time(
  bes13 <- read_rds("bes13.rds")
)


# voting for labour glm ---------------------------------------------------

bes13$disability <- factor(bes13$disability, levels=c("No", "Yes, limited a little", "Yes, limited a lot"))

bes13$ageW13 <- as.numeric(bes13$ageW13)

bes13$voted_labour17 <- ifelse(bes13$generalElectionVoteW13=="Labour", TRUE, FALSE)

bes13 <- bes13[bes13$anyUniW13!="Don't know",]

bes13$anyUniW13 <- factor(bes13$anyUniW13, levels=c("Yes, I graduated from higher education", "Yes, I am currently enrolled in higher education", "Yes, but I didn’t complete higher education", "No, I have never attended higher education"))

dis_labour <- bes13[bes13$generalElectionVoteW13!="I would not vote" &
        bes13$generalElectionVoteW13!="British National Party (BNP)" &
        is.na(bes13$generalElectionVoteW13)==FALSE
        & is.na(bes13$voted_labour17)==FALSE ,]

xtabs(wt_new_W13~voted_labour17+ageW13, data=dis_labour)

glm_labour <- glm(voted_labour17 ~ disability + ageW13 + anyUniW13 + profile_socialgrade_cieW13, data=dis_labour, weights = wt_new_W13)

summary(glm_labour)

pacman::p_load(stargazer)

glm_labour_table <- stargazer(glm_labour, star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE, out = "ge_vote_labour_disability.tex")

glm_labour_table <- stargazer(glm_labour, star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE, out = "ge_labour_regression.html")
