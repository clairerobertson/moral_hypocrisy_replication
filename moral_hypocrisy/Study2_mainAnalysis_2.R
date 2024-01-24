## Pilot 3 Analyses ## 
## CR, Summer 2023 ##

######################################
#### STEP 1: Load Packages & Data ####
######################################

options(digits = 8)

#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER", "ggpubr", "ggplot2")
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
x <- F_to_eta2(f = c(1.441, 9.043, 1.864), df = c(1,1,1), df_error = c(528, 528, 528), ci = .90, alternative = "greater")
x

## Equivelence Test  
equ_ftest(Fstat = 1.864, df1 = 1, df2 = 528, eqbound = 0.2)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

## T TEST FOR CI DIFFERENCE FROM CHANCE AND EFFECT SIZE
t.test(data$CI_difference, mu = 0, alternative = "two.sided")
sd(data$CI_difference, na.rm = T)

x <- cohens_d(data$CI_difference, mu = 0)
x

## T TEST FOR DEM AND REP CI DIFFERENCE AND EFFECT SIZE
t.test(subset(data,pol_id=="Dem")$CI_difference, 
       subset(data,pol_id=="Rep")$CI_difference)
sd(subset(data,pol_id=="Dem")$CI_difference, na.rm = T)
sd(subset(data,pol_id=="Rep")$CI_difference, na.rm = T)


x <- cohens_d(data$CI_difference ~ data$pol_id)
x

## Equivilence Testing
data %>% count(pol_id)

x <- tsum_TOST(m1 = mean(subset(data,pol_id=="Dem")$CI_difference, na.rm = T), 
          m2 = mean(subset(data,pol_id=="Rep")$CI_difference, na.rm = T), 
          sd1 = sd(subset(data,pol_id=="Dem")$CI_difference, na.rm = T), 
          sd2 = sd(subset(data,pol_id=="Rep")$CI_difference, na.rm = T),
          n1 = 297, n2 = 288, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)
x

## Make new dataframe 
CI_data <- data_subset %>% filter(condition == 3 | condition == 4)

## Means across groups
CI_data %>% group_by(condition.f) %>% 
  summarise(mean = mean(fairness))

## Check whether greater collective identification predicts fairness.
model1 <- lm(fairness ~ condition.f*CI_difference, data = CI_data)
summary(model1)

## Create labels
cond_labs <- c("In-group", "Out-group")
names(cond_labs) <- c("ingroup", "outgroup")

ggplot(CI_data, aes(x = CI_difference, y = fairness)) + ## , fill = pol_id, colour = pol_id)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), alpha = 0.4) +
  facet_grid(. ~ condition.f, labeller = labeller(condition.f = cond_labs)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_smooth(method = "lm", se = T) +
  labs(x = "Collective Identification",
       y = "Fairness Judgement",
       title = "Study 2 - Political Groups",
       subtitle = "Collective Identification " ) + 
  theme_bw()

ggsave("Plots/Study2_CI_all.png", width = 2000, height = 1200, units = "px", scale = 1)

## If Interaction is significant. 0 = ingroup, 1 = outgroup. 
simple_slopes(model1, levels = list(condition.f = c("ingroup", "outgroup")))

#### H5: Natural Groups are stronger than Minimal Groups ####

study1 <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)

## subset Study 1 data
study1 <- study1 %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) %>% ## Eliminate attention check failures.
  mutate(study = as.factor("Study 1")) %>% 
  select(-guestimate, -party, -procedure, -groupcheck_over, -groupcheck_under)

study2 <- data_subset %>% 
  select(-pol_id, -role) %>% 
  mutate(study = as.factor("Study 2"))

## EXPLORATORY ANALYSIS -- COLLECTIVE ID IN MINIMAL VS. NAT. GROUPS 
t.test(study1$CI_difference, study2$CI_difference)
x <- cohens_d(study1$CI_difference, study2$CI_difference)
x

#Filtering 
full_data <- rbind(study1, study2)

##ingroups and outgroups only ## 
model4_data <- full_data %>% 
  filter(condition == 3 | condition == 4)
  

## Graphing the data
model4_data %>% 
  group_by(study, condition.f) %>% 
  summarise(mean = mean(fairness)) %>% 
  ggplot(aes(y=mean,x=condition.f,colour=study,group=study))+
  geom_point()+geom_line() + 
  theme_bw()

## ANOVA w. ingroups and outgroups ## 
model4 <- aov(fairness~condition.f*study, data = model4_data)
summary(model4)

x <- F_to_eta2(f = c(11.693, 1.843, 0.590), df = c(1,1,1), df_error = c(581, 581, 581), ci = .90, alternative = "greater")
x

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
study1 <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)

study1 <- study1 %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) %>% ## Eliminate attention check failures.
  mutate(study = as.factor("Study 1")) %>% 
  select(-guestimate, -party, -procedure, -groupcheck_over, -groupcheck_under)

compare_means(fairness ~ condition.f, data = data_subset)
my_comparisons <- list(c("ingroup", "outgroup")) # c("self", "outgroup"), c("other", "outgroup"))

p1 <- ggplot(data_subset, aes(x = condition.f, y = fairness)) + 
  geom_violin(alpha = .2, fill = "navy") + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(position = position_jitter(width = 0.25, height = 0.25), alpha = 0.3) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2.5, color = "black", alpha = 0.8) + 
  geom_segment(x = 2.8, xend = 4.2, y = 7.3, yend = 7.3) + 
  annotate("text", x = 3.5, y = 7.6, label = "**", size = 16/.pt) + 
  theme_bw() +
  labs(title = "Study 2 - Political Groups",
              x = "Condition",
              y = "Fairness Ratings", 
              color = "Condition",
              fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back
p1

p2 <- ggplot(study1, aes(x = condition.f, y = fairness)) + 
  geom_violin(alpha = .2, fill = "lightblue") + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(position = position_jitter(width = 0.25, height = 0.25), alpha = 0.3) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2.5, color = "black", alpha = 0.8) + 
  geom_segment(x = .8, xend = 4.2, y = 7.3, yend = 7.3) + 
  annotate("text", x = 2.5, y = 7.6, label = "n.s.") + 
  theme_bw() +
  labs(title = "Study 1 - Minimal Groups",
       x = "Condition",
       y = "Fairness Ratings", 
       color = "Condition",
       fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back
p2

## Combine into a facet 
plot <- ggarrange(p2, p1, 
                  labels = c("A.", "B.")) 

plot

ggsave("Plots/Study1and2_fairness.png", width = 3000, height = 1500, units = "px", scale = 1)

##########################################
##### INtent to treat plotting ###########
##########################################

## Load in ITT Study 1 data 
study1 <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)

## Study 1 Altruists
study1 <- study1 %>% 
  mutate(altruists = ifelse(redgreen_selection == 2 | cond1_selection == 2, "fair", NA)) %>% 
  mutate(altruists = ifelse(is.na(altruists), "unfair", "fair"))

S1 <- ggplot(study1, aes(x = condition.f, y = fairness)) + 
  geom_violin(alpha = .2, fill = "lightblue") + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(aes(colour = altruists, fill = condition.f), position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) +
  scale_color_manual(values = c("fair" = "red", "unfair" = "black")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2.5, color = "black", alpha = 0.8) + 
  annotate("text", x = 1, y = 7.6, label = "***", size = 16/.pt) + 
  theme_bw() + 
  labs(title = "Study 1 - Minimal Groups",
       subtitle = "Altruists Included in Red",
                x = "Condition",
                y = "Fairness Ratings", 
                color = "Condition",
                fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back
S1

## Study 2
data <- data %>% 
  mutate(altruists = ifelse(redgreen_selection == 2 | cond1_selection == 2, "fair", NA)) %>% 
  mutate(altruists = ifelse(is.na(altruists), "unfair", "fair"))


S2 <- ggplot(data, aes(x = condition.f, y = fairness)) + 
  geom_violin(alpha = .2, fill = "navy") + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(aes(colour = altruists, fill = condition.f), position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) +
  scale_color_manual(values = c("fair" = "red", "unfair" = "black")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2.5, color = "black", alpha = 0.8) + 
  geom_segment(x = 2.8, xend = 4.2, y = 7.3, yend = 7.3) + 
  annotate("text", x = 1, y = 7.6, label = "***", size = 16/.pt) +
  annotate("text", x = 3.5, y = 7.6, label = "**", size = 16/.pt) + 
  scale_fill_brewer(palette = "Dark2", labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  theme_bw() + 
  labs(title = "Study 2 - Political Groups",
       subtitle = "Altruists Included in Red",
       x = "Condition",
       y = "Fairness Ratings", 
       color = "Condition",
       fill = "Condition") + 
  theme(legend.position = "none")
S2

plot2 <- ggarrange(S1, S2, 
                  labels = c("A.", "B.")) 

plot2

ggsave("Plots/Study1and2_fairness_altruists.png", width = 3000, height = 1500, units = "px", scale = 1)

