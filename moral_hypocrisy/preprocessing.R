## Pilot Analyses ## 
## CR, Spring 2022 ##

#### STEP 1: Load Packages & Data ####
#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car")
ipak(packages)

raw_data <- read_survey("data/pilot_data3.csv")

#### STEP 2: CLEAN DATA ####

data <- raw_data %>% 
  filter(RecordedDate >= as.Date("2022-05-06")) %>% 
    filter(!is.na(Q28))
names(data) <- make.names(names(data), unique = T)

data <- data %>% 
  rename(fairness = Q31)

## ADD ATTENTION CHECK. 

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
                     ifelse(!is.na(Q78_Click.Count) & participantRole == "Player4-Underestimator", 3, NA)))))))))))))))

#### STEP 3: DESCRIPTIVES #### 
                
## roles and condition distribution               
xtabs(~data$participantRole)
xtabs(~data$condition)

#Talking to a real person? 
xtabs(~data$Q95)

#Watching a screen recording? 
xtabs(~data$Q96)

#selected randomizer in self condition? 
data %>% filter(condition == 1) %>% 
  count(Q20)

## means are likely not significantly different. 
data %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))


#Subset for madison
subset <- data %>% 
  select(participantRole, condition, fairness.new., fairness, guesstimate.new._1, feedback..new., Q94, Q95, Q129, Q96, Q128, Q131, timeOutLog, chatLog)

write_csv(subset, "data/subset.csv")

