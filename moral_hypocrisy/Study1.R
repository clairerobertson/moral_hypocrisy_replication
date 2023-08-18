## Study 1 Proposed Analyses ## 
## CR, Fall 2022 ##

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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "TOSTER", "reghelper", "stats", "multcomp", "ivreg", "ivmodel", "effects")
ipak(packages)

## Code written using Pilot 2 Data 
raw_data <- read_survey("data/pilot3_prolific.csv")

############################
#### STEP 2: CLEAN DATA ####
############################

data <- raw_data %>% 
  filter(RecordedDate >= as.Date("2023-05-06")) %>% 
  filter(!is.na(Q28)) %>%
  filter(Progress == 100)

names(data) <- make.names(names(data), unique = T)

data <- data %>% 
  rename(fairness = Q31)

## For Proposed Analyses -- Simulate Collective Identification scores
data <- data %>% 
  mutate(collective1 = sample(1:7, 149, replace = T)) %>% 
  mutate(collective2 = sample(1:7, 149, replace = T)) %>% 
  mutate(collective3 = sample(1:7, 149, replace = T)) %>% 
  mutate(collective4 = sample(1:7, 149, replace = T)) %>% 
  mutate(collective5 = sample(1:7, 149, replace = T)) %>% 
  mutate(collective6 = sample(1:7, 149, replace = T))

#Create average difference score for collective identification. 
data <- data %>% 
  mutate(CI_difference = ((collective1 - collective4) + (collective2 - collective5) + (collective3 - collective6)) / 3)

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

levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

data <- data %>% filter(!is.na(condition)) 

##############################
#### STEP 3: DESCRIPTIVES #### 
##############################
                
## roles and condition distribution               
xtabs(~data$participantRole)
xtabs(~data$condition)

## attention check
xtabs(~data$attn_check)

#Talking to a real person? 
xtabs(~data$Q95)

#Watching a screen recording? 
xtabs(~data$Q96)

#selected randomizer in self condition? 
data %>% filter(condition == 1) %>% 
  count(Q20)

#selected green task
data %>% count(Q84)

##########################
#### STEP 4: ANALYSIS ####
##########################

#Combining the self fairness and other other fairness into one column
data <- data %>% 
  mutate(full_fairness = ifelse(is.na(fairness), Q142, fairness))

#Eliminate altruists, attention check failures. 
data_subset <- data %>% 
  filter(is.na(Q20)|Q20==1) %>% 
  filter(is.na(Q84)|Q84==1) %>% 
  filter(attn_check==3)

## Means
data_subset %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

data %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

## Contrast Analysis
model1 <- lm(full_fairness ~ condition.f, data = data)
summary(model1)

data <- data %>%
  mutate(self = as.numeric(condition.f == "self"),
         other = as.numeric(condition.f == "other"),
         ingroup = as.numeric(condition.f == "ingroup"),
         outgroup = as.numeric(condition.f == "outgroup"))

## Create Contrasts
for_contrast <- emmeans(model1, ~ condition.f)

Contrasts = list(self_vs_all = c(3, -1, -1, -1),
                 ingroup_vs_outgroup = c(0, 0, 1, -1),
                 selfingroup_vs_otheroutgroup = c(1, -1, 1, -1))

## Run Contrast Analysis 
contrast(for_contrast, Contrasts)

## CACE and ATT Analysis 
data <- data %>% 
  mutate(self_vs_other = ifelse(condition.f=="self", 3, -1)) %>% 
  mutate(ingroup_vs_outgroup = ifelse(condition.f=="ingroup", 1, 
                                      ifelse(condition.f=="outgroup", -1, 0))) %>% 
  mutate(selfingroup_vs_otheroutgroup = ifelse(condition.f == "self" | condition.f=="ingroup", 1, -1))

data <- data %>% 
  mutate(compliance = ifelse(data$Q20==2, 0, 1), 
         compliance = replace_na(data$compliance, 0))

model_cace2 <- ivreg(full_fairness ~  compliance | condition.f, data = data)
summary(model_cace2)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(full_fairness ~ condition.f, data = data_subset)
summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

model_1 <- lm(full_fairness ~ condition.f, data = data_subset)
summary(model_1)

## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(14.929, 0.098, 4.760), df = c(1,1,1), df_error = c(83, 83, 83), ci = .90, alternative = "greater")

## Equivelence Test ## Mu = 4 (middle of scale)
tsum_TOST(m=0, mu=0, sd=0, n=0,low_eqbound_d=-0.2, high_eqbound_d=0.2, alpha=0.05)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification? 

CI_data <- data_subset %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, 0, 1)))

model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)
## If Interaction is significant. 0 = ingroup, 1 = outgroup.
simple_slopes(model1, levels = list(dummy_code = c(0, 1)))

##############################################################
#### STEP 5: ROBUSTNESS CHECKS AND SUPPLEMENTARY ANALYSES ####
##############################################################

## Equivalence Testing
data_subset <- data_subset %>% mutate(participantRole_Sum = ifelse(participantRole == "Player2-Underestimator" | participantRole == "Player4-Underestimator", "under", "over")) 

data_subset %>% 
  group_by(condition, participantRole_Sum) %>%
  summarize(mean = mean(full_fairness), sd = sd(full_fairness), n = n())

## Each condition is tested using code below. Using Condition 2 as an example here. NHST should not be sig, means equivelent to zero. 
tsum_TOST(m1=3.29, m2=3.1, sd1=1.20, sd2=1.60, n1=14, n2=10, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha = 0.05, var.equal = TRUE)

## Collective Identification Manipulation Check 

ci_mc <- t.test(data_subset$CI_difference)
cohens_d(data_subset$CI_difference ~ 1, mu = 0)

#### Intent to Treat (everyone included).  
data %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

contrasts(data$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data$condition.f)

fairness.contrast_itt <- aov(full_fairness ~ condition.f, data = data)
summary.aov(fairness.contrast_itt, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

CI_data2 <- data %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, 0, 1)))

model1 <- lm(full_fairness ~ dummy_code*CI_difference, data = CI_data2)
summary(model1)

# Levene's test
leveneTest(full_fairness ~ condition.f, data_subset)
plot(fairness.contrast, 1)

##########################
#### STEP 6: Plotting ####
##########################

p <- ggplot(data, aes(x = condition.f, y = full_fairness)) + 
  geom_violin() + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"))

p + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .4) + 
  stat_summary(fun = mean, geom = "point", shape = 15, size = 5, color = "red")

p <- ggplot(data_subset, aes(x = condition.f, y = full_fairness)) + 
  geom_violin() + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"))

p + geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .4) + 
  stat_summary(fun = mean, geom = "point", shape = 15, size = 5, color = "red")

########################
#### STEP 7: EXPORT ####
########################

write_csv(data, "data/study1_clean.csv")


