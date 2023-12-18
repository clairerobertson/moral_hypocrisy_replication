##### Step 1, Preproccessing #####
## CR, Summer 2023 ##

require(tidyverse)
require(qualtRics)
require(readr)

## Load in Raw Data
raw_data <- read_survey("data/S2_Confirm_11172023.csv")

options(digits = 6)

############################
####### CLEAN DATA #########
############################

# filter out those who did not complete the study N = 606
data <- raw_data %>% 
  filter(!is.na(Q28)) %>% ## Over - Underestimator task. Means participant was successfully matched.
  filter(Progress == 100) %>% ## Finished the Survey
  filter(!prolific_id == "madison_test4") ## Removing test runs

#naming
names(data) <- make.names(names(data), unique = T)
data <- data %>% 
  rename(fairness = Q31)

#Combining the self fairness and other fairness into one column
data <- data %>% 
  mutate(fairness = ifelse(is.na(fairness), Q142, fairness))

#Political id variable creation
data <- data %>% 
  mutate(pol_id = ifelse(participantRole == "Player4-Republican" | participantRole == "Player2-Republican", "Rep", "Dem"))

## Labeling ## 
# 1 is self, 2 is unidentified other, 3 is ingroup, 4 is outgroup. 
#Q75 = player 3 Democrat
#Q76 = player 4 Republican
#Q77 = player 1 Democrat
#Q78 = player 2 Republican 
# 1 is self, 2 is unidentified other, 3 is ingroup, 4 is outgroup. 
data <- data %>% 
  mutate(condition = ifelse(!is.na(cond1_selection), 1, 
                            ifelse(!is.na(cond2_timing_Click.Count), 2, 
                            ifelse(!is.na(dem3_timing_Click.Count) & participantRole == "Player4-Republican", 4, 
                            ifelse(!is.na(dem3_timing_Click.Count) & participantRole == "Player2-Republican", 4, 
                            ifelse(!is.na(dem3_timing_Click.Count) & participantRole == "Player1-Democrat", 3, 
                            ifelse(!is.na(rep2_timing_Click.Count) & participantRole == "Player1-Democrat", 4, 
                            ifelse(!is.na(rep2_timing_Click.Count) & participantRole == "Player3-Democrat", 4,
                            ifelse(!is.na(rep2_timing_Click.Count) & participantRole == "Player4-Republican", 3, 
                            ifelse(!is.na(dem1_timing_Click.Count) & participantRole == "Player2-Republican", 4, 
                            ifelse(!is.na(dem1_timing_Click.Count) & participantRole == "Player4-Republican", 4, 
                            ifelse(!is.na(dem1_timing_Click.Count) & participantRole == "Player3-Democrat", 3, 
                            ifelse(!is.na(rep4_timing_Click.Count) & participantRole == "Player1-Democrat", 4, 
                            ifelse(!is.na(rep4_timing_Click.Count) & participantRole == "Player3-Democrat", 4, 
                            ifelse(!is.na(rep4_timing_Click.Count) & participantRole == "Player2-Republican", 3, NA))))))))))))))) %>% 
  mutate(condition.f = as.factor(condition))  

## Make levels for the condition varaible
levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

## Filter out anyone who didn't have a condition (should do nothing, but good to check)
data <- data %>% filter(!is.na(condition)) 

## Collective Identification questions, Calculate difference scores. 
data <- data %>% 
  mutate(CI_difference_dems = ((dem_ingroup1 - rep_outgroup1) + (dem_ingroup2 - rep_outgroup2) + (dem_ingroup3 - rep_outgroup3)) / 3) %>% 
  mutate(CI_difference_reps = ((rep_ingroup1 - dem_outgroup1) + (rep_ingroup2 - dem_outgroup2) + (rep_ingroup3 - dem_outgroup3)) / 3) %>% 
  mutate(CI_difference = coalesce(CI_difference_dems, CI_difference_reps)) ## Make Col. Iden. difference scores

## Consent for us to use their data.  (N = 8) 
data <- data %>% 
  filter(Q102 == 1) 

## Get group ids of duplicate people
x <- data %>% 
  arrange(StartDate) %>% 
  filter(duplicated(prolific_id, fromLast = TRUE))

## remove from DF
data_dup <- data %>% 
  filter((groupID %in% x$groupID))

# subset for supplemental analysis
data_dup <- data_dup %>% 
  select(participantRole, condition, condition.f, fairness, cond1_selection, redgreen_selection = Q84, attn_check, manip_check1 = Q95, manip_check2 = Q96, pol_id, pol_or, CI_difference)

## Save for supplement
#write.csv(data_dup, "data/study2_supp_exDF.csv")

# Remove repeat people - checked chat logs and they didn't mention anything about the experiment in the chats. (N = 13)
data <- data %>% arrange(StartDate) %>% 
  distinct(prolific_id, .keep_all = TRUE)

## Age 
x <- data %>% 
  filter(attn_check==3) 
mean(x$age, na.rm = T)
sd(x$age, na.rm = T)

#Gender
mean(x$gender == 2, na.rm = T) * 100
xtabs(~x$gender)

## Political assignment and political identification
xtabs(~x$participantRole)
xtabs(~x$pol_or)

## Select variables for analysis
analysis_df <- data %>% 
  select(participantRole, condition, condition.f, fairness, cond1_selection, redgreen_selection = Q84, attn_check, manip_check1 = Q95, manip_check2 = Q96, pol_id, pol_or, CI_difference)


## Write CSV for analysis
write.csv(analysis_df, "data/study2_analysisDF.csv")
