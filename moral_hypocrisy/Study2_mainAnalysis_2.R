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
data <- read.csv("data/study2_analysisDF.csv", stringsAsFactors = T)

## Relevel data
levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

##############################
#### STEP 2: DESCRIPTIVES #### 
##############################

#roles and condition distribution               
xtabs(~data$participantRole)
xtabs(~data$condition)

#attention check 
xtabs(~data$attn_check)

#selected randomizer in self condition? 1 = choose, 2 = randomizer
data %>% filter(condition == 1) %>% 
  count(cond1_selection)

#selected green task; 1 = green, 2 = red 
data %>% count(redgreen_selection)

## Political Identificaiton 
data %>% count(pol_or)

#Eliminate altruists, attention check failures. 
data_subset <- data %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) ## Eliminate attention check failures. 


###############################
##### STEP 4: MAIN ANALYSIS ###
###############################

## Means
data_subset %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

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
fairness.contrast <- aov(fairness ~ condition.f, data = data_subset)
summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(1.441, 9.043, 1.864), df = c(1,1,1), df_error = c(528, 528, 528), ci = .90, alternative = "greater")

## Equivelence Test ## Mu = 4 (middle of scale) -- will fail currently, no data. 
tsum_TOST(m=0, mu=0, sd=0, n=0,low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

CI_data <- data_subset %>% filter(condition == 3 | condition == 4)

## Means across groups
CI_data %>% group_by(condition.f) %>% 
  summarise(mean = mean(fairness))

## Check whether greater collective identification predicts fairness.
model1 <- lm(fairness ~ condition.f*CI_difference, data = CI_data)
summary(model1)

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) + ## , fill = pol_id, colour = pol_id)) +
  geom_point() +
  facet_grid(. ~ condition.f) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Collective Identification",
       y = "Fairness",
       title = "Fairness ratings by collective identification and condition") + 
  theme_bw()
       
## If Interaction is significant. 0 = ingroup, 1 = outgroup. 
simple_slopes(model1, levels = list(condition.f = c("ingroup", "outgroup")))


#### H5: Natural Groups are stronger than Minimal Groups ####

study1 <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)

## subset Study 1 data
study1 <- study1 %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) %>% ## Eliminate attention check failures.
  mutate(study = as.factor("Study 1"))

study2 <- data_subset %>% 
  select(-pol_id) %>% 
  mutate(study = as.factor("Study 2"))

#Filtering 
full_data <- rbind(study1, study2)

##ingroups and outgroups only ## 
model4_data <- full_data %>% filter(condition == 3 | condition == 4)

## Graphing the data
model4_data %>% 
  group_by(study, condition.f) %>% 
  summarise(mean = mean(fairness)) %>% 
  ggplot(aes(y=mean,x=condition.f,colour=study,group=study))+
  geom_point()+geom_line()

## ANOVA w. ingroups and outgroups ## 
model4 <- aov(fairness~condition.f*study, data = model4_data)
summary(model4)

## Post Hoc Tests ## 
model4_data %>% filter(study=="Study 1") %>% 
  t.test(fairness ~ condition, data = .)

model4_data %>% filter(study=="Study 2") %>% 
  t.test(fairness ~ condition, data = .)

model4_data %>% filter(condition==3) %>% 
  t.test(fairness ~ study, data = .)

model4_data %>% filter(condition==4) %>% 
  t.test(fairness ~ study, data = .)


##########################
#### STEP 6: Plotting ####
##########################

compare_means(fairness ~ condition.f, data = data_subset)
my_comparisons <- list(c("ingroup", "outgroup")) # c("self", "outgroup"), c("other", "outgroup"))

## Overeall Mean Fairnes Graph
p <- ggplot(data_subset, aes(x = condition.f, y = fairness, fill = condition.f)) + 
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

