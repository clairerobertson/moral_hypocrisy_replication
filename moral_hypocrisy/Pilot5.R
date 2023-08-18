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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "lubridate")
ipak(packages)

raw_data <- read_survey("data/pilot5_july31.csv")

############################
#### STEP 2: CLEAN DATA ####
############################

my_date <- as.Date("2023-07-31")
my_date <- my_date + hours(16) + minutes(10) + seconds(0)


#filter out those who did not complete the study
data <- raw_data %>% 
  filter(StartDate <= my_date) %>%
    filter(!is.na(Q28)) %>%
  filter(Progress == 100)

#naming
names(data) <- make.names(names(data), unique = T)
data <- data %>% 
  rename(fairness = Q31)

## Labeling ## 
# 1 is self, 2 is unidentified other, 3 is ingroup, 4 is outgroup. 
#Q75 = player 3 Democrat
#Q76 = player 4 Republican
#Q77 = player 1 Democrat
#Q78 = player 2 Republican 
# 1 is self, 2 is unidentified other, 3 is ingroup, 4 is outgroup. 
data <- data %>% 
   mutate(condition = ifelse(!is.na(Q20), 1, 
                            ifelse(!is.na(Q34_Click.Count), 2, 
                             ifelse(!is.na(Q167_Click.Count) & participantRole == "Player4-Republican", 4, 
                             ifelse(!is.na(Q167_Click.Count) & participantRole == "Player2-Republican", 4, 
                             ifelse(!is.na(Q167_Click.Count) & participantRole == "Player1-Democrat", 3, 
                             ifelse(!is.na(Q76_Click.Count) & participantRole == "Player1-Democrat", 4, 
                             ifelse(!is.na(Q76_Click.Count) & participantRole == "Player3-Democrat", 4,
                             ifelse(!is.na(Q76_Click.Count) & participantRole == "Player2-Republican", 3, 
                             ifelse(!is.na(Q172_Click.Count) & participantRole == "Player2-Republican", 4, 
                             ifelse(!is.na(Q172_Click.Count) & participantRole == "Player4-Republican", 4, 
                             ifelse(!is.na(Q172_Click.Count) & participantRole == "Player3-Democrat", 3, 
                             ifelse(!is.na(Q177_Click.Count) & participantRole == "Player1-Democrat", 4, 
                             ifelse(!is.na(Q177_Click.Count) & participantRole == "Player3-Democrat", 4, 
                             ifelse(!is.na(Q177_Click.Count) & participantRole == "Player4-Republican", 3, NA))))))))))))))) %>% 
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
prop.test(x = 35, n = 44, p = 0.5, 
          correct = FALSE)

# watching Assigning tasks? 
data_234 <- data %>% filter(!condition == 1)
xtabs(~data_234$Q96) 
prop.test(x = 23, n = 32, p = 0.5, 
          correct = FALSE)


#selected randomizer in self condition? 1 = choose, 2 = randomizer
data %>% filter(condition == 1) %>% 
  count(Q20)

#selected green task; 1 = green, 2 = red 
data %>% count(Q84)

########################
#### STEP 4: EXPORT ####
########################

write_csv(data, "data/pilot4_clean.csv")

## RA Coding Comments
subset <- data %>% 
  select(participantRole, condition, fairness.new., fairness, guesstimate.new._1, feedback..new., Q94, Q95, Q129, Q96, Q128, Q131, timeOutLog, chatLog)

write_csv(subset, "data/Pilot_4_Comments.csv")

