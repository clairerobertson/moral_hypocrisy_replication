## Pilot 3 Analyses ## 
## CR, Summer 2023 ##

######################################
#### STEP 1: Load Packages & Data ####
######################################

#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car")
ipak(packages)

raw_data <- read_survey("data/pilot3_prolific.csv")

############################
#### STEP 2: CLEAN DATA ####
############################

#filter out those who did not complete the study
data <- raw_data %>% 
  filter(RecordedDate >= as.Date("2023-05-06")) %>% 
    filter(!is.na(Q28)) %>%
  filter(Progress == 100)

#naming
names(data) <- make.names(names(data), unique = T)
data <- data %>% 
  rename(fairness = Q31)

## Labeling ## 
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

levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

data <- data %>% filter(!is.na(condition)) 

##############################
#### STEP 3: DESCRIPTIVES #### 
##############################
                
#roles and condition distribution               
xtabs(~data$participantRole)
xtabs(~data$condition)

#attention check 
xtabs(~data$attn_check)

# talking to a real person? 
xtabs(~data$Q95)
prop.test(x = 97, n = 149, p = 0.5, 
          correct = FALSE)

# watching a screen recording? 
data_234 <- data %>% filter(condition !=1)
xtabs(~data_234$Q96) 
prop.test(x = 65, n = 112, p = 0.5, 
          correct = FALSE)

#selected randomizer in self condition? 1 = choose, 2 = randomizer
data %>% filter(condition == 1) %>% 
  count(Q20)

#selected green task; 1 = green, 2 = red 
data %>% count(Q84)

########################
#### STEP 4: EXPORT ####
########################

write_csv(data, "data/pilot3_clean.csv")

## RA Coding Comments
subset <- data %>% 
  select(participantRole, condition, fairness.new., fairness, guesstimate.new._1, feedback..new., Q94, Q95, Q129, Q96, Q128, Q131, timeOutLog, chatLog)

write_csv(subset, "data/Pilot_3_Comments.csv")

