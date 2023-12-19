##### Step 1, Preproccessing #####
## CR, Summer 2023 ##

require(tidyverse)
require(qualtRics)
require(readr)

## Load in Raw Data
raw_data <- read_survey("data/S1_Confirm_12162023.csv")

############################
####### CLEAN DATA #########
############################

# filter out those who did not complete the study N = 606
data <- raw_data %>% 
  filter(!is.na(Q28)) %>% ## Over - Underestimator task. Means participant was successfully matched.
  filter(Progress == 100) %>% ## Finished the Survey
  filter(participantStatus == "ready") %>%  ## Finished the Survey
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(StartDate >= as.POSIXct("2023-11-29")) ## Removing test runs

#naming
names(data) <- make.names(names(data), unique = T)
data <- data %>% 
  rename(fairness = Q31)

#Combining the self fairness and other fairness into one column
data <- data %>% 
  mutate(fairness = ifelse(is.na(fairness), Q142, fairness)) %>% 
  mutate(guestimate = ifelse(is.na(guesstimate.new._1), Q143_1, guesstimate.new._1)) %>% 
  mutate(procedure = ifelse(is.na(fairness.new.), Q141, fairness.new.)) %>% 
  mutate(party = Q91 - 13)





## Lableing ## 
#Q75 = player 3 overestimator
#Q76 = player 4 underestimator
#Q77 = player 1 overestimator
#Q78 = player 2 underestimator 
# 1 is self, 2 is unidentified other, 3 is ingroup, 4 is outgroup. 
data <- data %>% 
  mutate(condition = ifelse(!is.na(Q20), 1, 
                            ifelse(!is.na(Q34_Click.Count), 2, 
                            ifelse(!is.na(Q75_Click.Count) & participantRole == "Player4-Underestimator", 4, 
                            ifelse(!is.na(Q75_Click.Count) & participantRole == "Player2-Underestimator", 4, 
                            ifelse(!is.na(Q75_Click.Count) & participantRole == "Player1-Overestimator", 3, 
                            ifelse(!is.na(Q76_Click.Count) & participantRole == "Player1-Overestimator", 4, 
                            ifelse(!is.na(Q76_Click.Count) & participantRole == "Player3-Overestimator", 4,
                            ifelse(!is.na(Q76_Click.Count) & participantRole == "Player2-Underestimator", 3, 
                            ifelse(!is.na(Q77_Click.Count) & participantRole == "Player2-Underestimator", 4, 
                            ifelse(!is.na(Q77_Click.Count) & participantRole == "Player4-Underestimator", 4, 
                            ifelse(!is.na(Q77_Click.Count) & participantRole == "Player3-Overestimator", 3, 
                            ifelse(!is.na(Q78_Click.Count) & participantRole == "Player1-Overestimator", 4, 
                            ifelse(!is.na(Q78_Click.Count) & participantRole == "Player3-Overestimator", 4, 
                            ifelse(!is.na(Q78_Click.Count) & participantRole == "Player4-Underestimator", 3, NA))))))))))))))) %>% 
  mutate(condition.f = as.factor(condition))

## Make levels for the condition varaible
levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

## Filter out anyone who doesn't have a condition (should do nothing, but good to check) 
data <- data %>% filter(!is.na(condition)) 

## Collective Identification questions, Calculate difference scores. 
data <- data %>% 
  mutate(CI_difference_under = ((under_ingroup1 - over_outgroup1) + (under_ingroup2 - over_outgroup2) + (under_ingroup3 - over_outgroup3)) / 3) %>% 
  mutate(CI_difference_over = ((over_ingroup1 - under_outgroup1) + (over_ingroup2 - under_outgroup2) + (over_ingroup3 - under_outgroup3)) / 3) %>% 
  mutate(CI_difference = coalesce(CI_difference_under, CI_difference_over)) ## Make Col. Iden. difference scores

## Consent for us to use their data.  (N = XXX) 
data <- data %>% 
  filter(Q102 == 1) 

## Get group ids of duplicate people
x <- data %>% 
  arrange(StartDate) %>% 
  filter(duplicated(prolific_id, fromLast = TRUE))

## remove anyone in any group of repeated person from DF
data_dup <- data %>% 
  filter(!(groupID %in% x$groupID))

# subset for supplemental analysis
data_dup <- data_dup %>% 
  select(participantRole, condition, condition.f, fairness, cond1_selection = Q20, redgreen_selection = Q84, attn_check, manip_check1 = Q95, manip_check2 = Q96, pol_or, CI_difference)

## Save for supplement
#write.csv(data_dup, "data/study1_supp_exDF.csv")

# Remove repeat people - checked chat logs and they didn't mention anything about the experiment in the chats. 
data <- data %>% arrange(StartDate) %>% 
  distinct(prolific_id, .keep_all = TRUE)

## failed manipulation check? 
xtabs(~data$group_check)
xtabs(~data$Q186)

## Age 
x <- data %>% 
  filter(attn_check==3) 
mean(x$age, na.rm = T)
sd(x$age, na.rm = T)

#Gender
xtabs(~x$gender)
mean(x$gender == 2, na.rm = T) * 100

xtabs(~x$participantRole)
xtabs(~x$pol_or)

(149+144) / 577
(140+144) / 577

# Ethnicity 
data %>% 
  count(ethnicity)


## Select variables for analysis
analysis_df <- data %>% 
  select(participantRole, condition, condition.f, fairness, cond1_selection = Q20, redgreen_selection = Q84, attn_check, manip_check1 = Q95, manip_check2 = Q96, groupcheck_over = group_check, groupcheck_under = Q186, guestimate, procedure, party, pol_or, CI_difference)

## Write CSV for analysis
write.csv(analysis_df, "data/study1_analysisDF.csv")




############ EXTRA ##################


raw_data %>% 
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(
    (format(StartDate, "%Y-%m-%d") %in% c("2023-11-29", "2023-12-06", "2023-12-15")) &
      (format(StartDate, "%H:%M:%S") >= "15:50:00" & format(StartDate, "%H:%M:%S") <= "17:00:00")) %>%
  filter(!is.na(Q28))

## SURVEY SIGN UP DATA   
signup <- read_survey("data/Study1_Signup.csv")

signup %>% 
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(StartDate <= as.POSIXct("2023-12-16")) %>% 
  count(participation)

#### CACE Subsetting 

altruists <- data %>% 
  filter(Q20==2 | Q20==2) %>% 
  mutate(complier = "alt")

cond1 <- data_subset %>% 
  filter(condition==1)

cace_data <- data_subset %>% 
  mutate(complier = "comp") %>% 
  rbind(altruists)






