
pacman::p_load(readr)
#pacman::p_load(nnet)
pacman::p_load(dplyr)

system.time(
  bes13 <- read_rds("bes13.rds")
)

bes13$disability <- factor(bes13$disability, levels=c("No", "Yes, limited a little", "Yes, limited a lot", NA))

bes13$ageW13 <- as.numeric(bes13$ageW13)

bes13$voted_remain <- ifelse(bes13$profile_eurefvote=="Stay/remain in the EU", TRUE, FALSE)

eu_vote_dis <- bes13[bes13$profile_eurefvote!="Don't know"
                     & is.na(bes13$profile_eurefvote)==FALSE,]

eu_vote_dis$anyUni_test <- ifelse(is.na(eu_vote_dis$anyUniW7)==TRUE, as.character(eu_vote_dis$anyUniW9), as.character(eu_vote_dis$anyUniW7))

eu_vote_dis$social_grade <- factor(ifelse(is.na(eu_vote_dis$profile_socialgrade_cieW7)==TRUE, as.character(eu_vote_dis$profile_socialgrade_cieW8), as.character(eu_vote_dis$profile_socialgrade_cieW7)))

eu_vote_dis$social_grade <- recode(eu_vote_dis$social_grade,
                                   "a" = "A",
                                   "b" = "B",
                                   "d" = "D",
                                   "e" = "E")

eu_vote_dis <- eu_vote_dis[eu_vote_dis$anyUni_test!="Don't know"
                           & is.na(eu_vote_dis$anyUni_test)==FALSE
                           & is.na(eu_vote_dis$wt_full_W7W8W9)==FALSE
                           & is.na(eu_vote_dis$disability)==FALSE,]

eu_vote_dis$anyUni_test <- factor(eu_vote_dis$anyUni_test, levels=c("Yes, I graduated from higher education", "Yes, I am currently enrolled in higher education", "Yes, but I didn’t complete higher education", "No, I have never attended higher education"))

summary(eu_vote_dis$disability)

xtabs(eu_vote_dis$wt_full_W7W8W9~eu_vote_dis$disability)

#under_30 <- eu_vote_dis[eu_vote_dis$Age <=30,]

#xtabs(under_30$wt_full_W7W8W9~under_30$disability+under_30$anyUni_test)

glm_remain<- glm(voted_remain ~ disability + Age + anyUni_test + social_grade, data = eu_vote_dis, weights = wt_full_W7W8W9)

#newdat <- data.frame(age=seq(min(eu_vote_dis$Age, na.rm = TRUE), max(eu_vote_dis$Age, na.rm = TRUE),len=100))

#newdat$vs = predict(glm_remain, newdata=newdat, type="response")

summary(glm_remain)

pacman::p_load(stargazer)

glm_remain_table <- stargazer(glm_remain, star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE, out="eu_regression.html")
