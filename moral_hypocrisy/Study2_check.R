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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER", "ggpubr")
ipak(packages)

## Load in Raw Data
raw_data <- read_survey("data/S2_Confirm_11172023.csv")

############################
#### STEP 2: CLEAN DATA ####
############################

# filter out those who did not complete the study N = 606
data <- raw_data %>% 
    filter(!is.na(Q28)) %>% ## Over - Underestimator task. Means participant was successfully matched.
  filter(Progress == 100) %>% ## Finished the Survey
  filter(!prolific_id == "madison_test4") ## Removing test runs

## Consent for us to use their data.   
data <- data %>% 
  filter(Q102 == 1) 

# Remove repeat people - checked chat logs and they didn't mention anything about the experiment in the chats. 
data <- data %>% arrange(StartDate) %>% 
  distinct(prolific_id, .keep_all = TRUE) 

#naming
names(data) <- make.names(names(data), unique = T)
data <- data %>% 
  rename(fairness = Q31)

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

data <- data %>% 
  mutate(full_fairness = ifelse(is.na(fairness), Q142, fairness))

analysis_df <- data %>% 
  select(participantRole, condition, condition.f, full_fairness, cond1_selection, redgreen_selection = Q84, attn_check, manip_check1 = Q95, manip_check2 = Q96, pol_id, pol_or, CI_difference)

summary(analysis_df)

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
# prop.test(x = 35, n = 44, p = 0.5, 
         # correct = FALSE)

# Others were actually  Assigning tasks? 
data_234 <- data %>% filter(!condition == 1) ## Filter out those in condition 1
xtabs(~data_234$Q96) # 1 is yes, 2 is no.

#selected randomizer in self condition? 1 = choose, 2 = randomizer
data %>% filter(condition == 1) %>% 
  count(cond1_selection)

#selected green task; 1 = green, 2 = red 
data %>% count(Q84)

#Combining the self fairness and other fairness into one column
data <- data %>% 
  mutate(full_fairness = ifelse(is.na(fairness), Q142, fairness))

## Political Identificaiton in each group
data %>% 
  group_by(pol_id) %>% 
  count(pol_or)



  
#Eliminate altruists, attention check failures. 
data_subset <- analysis_df %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) # %>% ## Eliminate attention check failures. 
  #mutate(CI_difference = coalesce(CI_difference_dems, CI_difference_reps)) ## Make Col. Iden. difference scores

###############################
##### STEP 4: MAIN ANALYSIS ###
###############################

## Means
data_subset %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

## Set Up contrasts ## 
levels(data_subset$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_subset$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_subset$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(full_fairness ~ condition.f, data = data_subset)
summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(4.531, 10.134, 1.994), df = c(1,1,1), df_error = c(482, 482, 482), ci = .90, alternative = "greater")

## Equivelence Test ## Mu = 4 (middle of scale) -- will fail currently, no data. 
tsum_TOST(m=0, mu=0, sd=0, n=0,low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

CI_data <- data_subset %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, "ingroup", "outgroup"))) %>% ## Dummy code ingroup as 0, outgroup as 1
  mutate(CI_difference = coalesce(CI_difference_dems, CI_difference_reps)) ## Combine CI difference scores for dems and reps

## Means across groups
CI_data %>% group_by(dummy_code) %>% 
  summarise(mean = mean(fairness))

## Check whether greater collective identification predicts fairness.
model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) +
  geom_point() +
  facet_grid(. ~ dummy_code) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Collective Identification",
       y = "Fairness",
       title = "Fairness ratings by collective identification and condition")
       
## If Interaction is significant. 0 = ingroup, 1 = outgroup. 
simple_slopes(model1, levels = list(dummy_code = c(0, 1)))


#### H5: Natural Groups are stronger than Minimal Groups ####

## Combine data sets (I'm simulating some data here)
study1 <- study1_data %>% select(condition, condition.f, full_fairness, attn_check, Q20, Q84) %>% 
  mutate(study = as.factor(0))

study2 <- data %>% select(condition, condition.f, full_fairness, attn_check, Q20, Q84) %>% 
  mutate(study = as.factor(1)) %>% 
  mutate(full_fairness = jitter(full_fairness))

#Filtering 
full_data <- rbind(study1, study2)

full_data <- full_data %>% 
  filter(is.na(Q20)|Q20==1) %>% 
  filter(is.na(Q84)|Q84==1) %>% 
  filter(attn_check==3)

##ingroups and outgroups only ## 
model4_data <- full_data %>% filter(condition > 2)

## Graphing the data
model4_data %>% 
  group_by(study, condition.f) %>% 
  summarise(mean = mean(full_fairness)) %>% 
  ggplot(aes(y=mean,x=condition.f,colour=study,group=study))+
  geom_point()+geom_line()

## ANOVA w. ingroups and outgroups ## 
model4 <- aov(full_fairness~condition.f*study, data = model4_data)
summary(model4)

## Post Hoc Tests ## 
model4_data %>% filter(study==0) %>% 
  t.test(full_fairness ~ condition, data = .)

model4_data %>% filter(study==1) %>% 
  t.test(full_fairness ~ condition, data = .)

model4_data %>% filter(condition==3) %>% 
  t.test(full_fairness ~ study, data = .)

model4_data %>% filter(condition==4) %>% 
  t.test(full_fairness ~ study, data = .)


##############################################################
#### STEP 5: ROBUSTNESS CHECKS AND SUPPLEMENTARY ANALYSES ####
##############################################################

#######################################################
#### STEP 5A: Intent to Treat (everyone included). ####
#######################################################

data %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

contrasts(data$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data$condition.f)

fairness.contrast_itt <- aov(full_fairness ~ condition.f, data = data)
summary.aov(fairness.contrast_itt, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

CI_data2 <- data %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, 0, 1)))

model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data2)
summary(model1)

# Levene's test
leveneTest(fairness ~ condition.f, data_subset)
plot(fairness.aov, 1)

## Equivilence testing
TOSTmeta(ES = 0.4, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.05)

#################################################################
#### STEP 5B: politically mismatched participants eliminated ####
#################################################################

## Create subset without politically mismatched paricipants
data_5b <- data_subset %>% 
  filter(CI_difference >= 0) 

data_5b %>% group_by(condition) %>% 
  summarise(mean = mean(full_fairness))

## CONTRAST RESULTS ## 
fairness.contrast <- aov(full_fairness ~ condition.f, data = data_5b)
summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(14.929, 0.098, 4.760), df = c(1,1,1), df_error = c(83, 83, 83), ci = .90, alternative = "greater")

## Equivelence Test ## Mu = 4 (middle of scale) -- will fail currently, no data. 
tsum_TOST(m=0, mu=0, sd=0, n=0,low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

CI_data <- data_5b %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, "ingroup", "outgroup"))) ## Dummy code ingroup as 0, outgroup as 1

## Means across groups
CI_data %>% group_by(dummy_code) %>% 
  summarise(mean = mean(fairness))

## Check whether greater collective identification predicts fairness.
model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) +
  geom_point() +
  facet_grid(. ~ dummy_code) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Collective Identification",
       y = "Fairness",
       title = "Fairness ratings by collective identification and condition")


##########################
#### STEP 6: Plotting ####
##########################

compare_means(full_fairness ~ condition.f, data = data_subset)
my_comparisons <- list(c("ingroup", "outgroup")) # c("self", "outgroup"), c("other", "outgroup"))


## Overeall Mean Fairnes Graph
p <- ggplot(data_subset, aes(x = condition.f, y = full_fairness, fill = condition.f)) + 
  geom_violin(alpha = .2) + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  geom_jitter(aes(colour = condition.f), position = position_jitter(width = 0.2, height = 0.4), alpha = 0.4) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 1, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2, color = "black", alpha = 0.8) + 
  stat_compare_means(comparisons = my_comparisons)
 
p
 
## Colors  
p <- p + scale_fill_brewer(palette = "Dark2", labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  scale_color_brewer(palette = "Dark2", labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  theme_bw()
 
## Graph Labeling
p + labs(title = "Average Fairness Ratings",
         subtitle = "Altruists Excluded",
         x = "Condition",
         y = "Fairness Ratings", 
         color = "Condition",
         fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back



########################
#### STEP 4: EXPORT ####
########################

write_csv(data, "data/study2_10192023_clean.csv")

## RA Coding Comments
subset <- data %>% 
  select(participantRole, condition, fairness.new., fairness, guesstimate.new._1, feedback..new., Q94, Q95, Q129, Q96, Q128, Q131, timeOutLog, chatLog)

write_csv(subset, "data/10192023_Comments.csv")



# Check kurtosis of CI variable. 
skewness(CI_data$CI_difference_dems, na.rm = T)
kurtosis(CI_data$CI_difference_dems, na.rm = T)
skewness(CI_data$CI_difference_reps, na.rm = T)
kurtosis(CI_data$CI_difference_reps, na.rm = T)

