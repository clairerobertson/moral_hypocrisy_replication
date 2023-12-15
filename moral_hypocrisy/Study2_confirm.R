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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER")
ipak(packages)

raw_data <- read_survey("data/MoHiStudy2_091223.csv")

############################
#### STEP 2: CLEAN DATA ####
############################

my_date <- as.Date("2023-09-12")
my_date <- my_date + hours(16) + minutes(20) + seconds(0)


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

levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

data <- data %>% filter(!is.na(condition)) 

data <- data %>% 
  mutate(CI_difference_dems = ((dem_ingroup1 - rep_outgroup1) + (dem_ingroup2 - rep_outgroup2) + (dem_ingroup3 - rep_outgroup3)) / 3) %>% 
  mutate(CI_difference_reps = ((rep_ingroup1 - dem_outgroup1) + (rep_ingroup2 - dem_outgroup2) + (rep_ingroup3 - dem_outgroup3)) / 3)


data$CI_difference_dems
data$CI_difference_reps
data$rep_ingroup1
data$rep_ingroup2
data$rep_ingroup3


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
  count(cond1_selection)

#selected green task; 1 = green, 2 = red 
data %>% count(Q84)

#Combining the self fairness and other other fairness into one column
data <- data %>% 
  mutate(full_fairness = ifelse(is.na(fairness), Q142, fairness))

#Eliminate altruists, attention check failures. 
data_subset <- data %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% 
  filter(is.na(Q84)|Q84==1) %>% 
  filter(attn_check==3)

###############################
##### STEP 5: ANALYSIS ########
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
F_to_eta2(f = c(14.929, 0.098, 4.760), df = c(1,1,1), df_error = c(83, 83, 83), ci = .90, alternative = "greater")

## Equivelence Test ## Mu = 4 (middle of scale) -- will fail currently, no data. 
tsum_TOST(m=0, mu=0, sd=0, n=0,low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

CI_data <- data_subset %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, 0, 1)))

# Check kurtosis of CI variable. 
skewness(CI_data$CI_difference)
kurtosis(CI_data$CI_difference)

model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)
## If Interaction is significant. 0 = ingroup, 1 = outgroup. 
simple_slopes(model1, levels = list(dummy_code = c(0, 1)))


## H5: Natural Groups are stronger than Minimal Groups 

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

#### Intent to Treat (everyone included).  
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
#### STEP 4: EXPORT ####
########################

write_csv(data, "data/study2_091223_clean.csv")

## RA Coding Comments
subset <- data %>% 
  select(participantRole, condition, fairness.new., fairness, guesstimate.new._1, feedback..new., Q94, Q95, Q129, Q96, Q128, Q131, timeOutLog, chatLog)

write_csv(subset, "data/091223_Comments.csv")

