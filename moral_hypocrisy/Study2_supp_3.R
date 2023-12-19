## Pilot 3 Analyses ## 
## CR, Summer 2023 ##

######################################
#### STEP 1: Load Packages & Data ####
######################################

options(digits = 6)

#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER", "ggpubr", "texreg", "xtable")
ipak(packages)

## Load in Raw Data
data <- read.csv("data/study2_analysisDF.csv", stringsAsFactors = T)


## Relevel data
levels(data$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

#Eliminate altruists, attention check failures. 
data_subset <- data %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) ## Eliminate attention check failures. 

#############################################################
####  Altruists vs. Transgressors ###########################
#############################################################
altruists <- data %>% 
  filter(cond1_selection==2 | redgreen_selection==2) 

cond1 <- data_subset %>% 
  filter(condition==1)

mean(altruists$fairness)

## T test comparing fairness ratings of altruists and 
x <- t.test(altruists$fairness, cond1$fairness)


#############################################################
#### Intent to Treat (everyone included).  H1, H2 & H 3  ####
#############################################################
data %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

## Set Up contrasts ## 
levels(data$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))


## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(40.606, 7.461, 1.585), df = c(1,1,1), df_error = c(581, 581, 581), ci = .90, alternative = "greater")


## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "ITT", label = "ITT") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)

#######################################
##### Plotting of ITT, H1, H2, H3 #####
#######################################

compare_means(fairness ~ condition.f, data = data)
my_comparisons <- list(c("ingroup", "outgroup"), c("self", "outgroup"), c("other", "outgroup"))

## Overeall Mean Fairnes Graph
p <- ggplot(data, aes(x = condition.f, y = fairness, fill = condition.f)) + 
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
         subtitle = "Altruists Included",
         x = "Condition",
         y = "Fairness Ratings", 
         color = "Condition",
         fill = "Condition") + 
  theme(legend.position = "none") 

####################################################################
#### Repeat Survey Takers groups Excluded ####
####################################################################

## read in data with any group with dupl. person excluded
data_exclude <- read.csv("data/study2_supp_exDF.csv", stringsAsFactors = T)

## clean excluded data
data_exclude <- data_exclude %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3)

## means
data_exclude %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

data_exclude$condition.f

#relevel data
levels(data_exclude$condition.f) <- list(self = "1", other = "2", ingroup = "3", outgroup = "4")

## Set Up contrasts ## 
levels(data_exclude$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_exclude$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_exclude$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data_exclude)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## EFFECT SIZE FOR CONTRASTS ### 
F_to_eta2(f = c(40.606, 7.461, 1.585), df = c(1,1,1), df_error = c(581, 581, 581), ci = .90, alternative = "greater")

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Results with repeats excluded", label = "repeats") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)


####################################################################
#### politically mismatched participants eliminated: H1, H2, H3 ####
####################################################################

####  Mismatched partisans, political id vs. political orienation, and neg CI  ###
mismatch <- data_subset %>% 
  filter((pol_id == "Dem" & pol_or > 4) | (pol_id == "Rep" & pol_or < 4) | CI_difference < 0)

## Create subset without politically mismatched participants
data_partisans <- data_subset %>% 
  filter(!X %in% mismatch$X) 

data_partisans %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

## Set Up contrasts ## 
levels(data_partisans$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_partisans$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_partisans$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data_partisans)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Politically mismatched participants excluded.",
                       label = "mismatch") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)

###########################################################
### COLLECTVE IDENTITY W/O INCONGRUENT IDENTIFIERS, H6 ####
###########################################################

## Check whether greater collective identification predicts fairness.
CI_data <- data_partisans %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, "ingroup", "outgroup"))) %>% ## Dummy code ingroup as 0, outgroup as 1
  filter(CI_difference > 0) ## Filter out people who identified more with their other group. 

model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)

cond_labs <- c("In-group", "Out-group")
names(cond_labs) <- c("ingroup", "outgroup")

ggplot(CI_data, aes(x = CI_difference, y = fairness)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), alpha = 0.4) +
  facet_grid(. ~ condition.f, labeller = labeller(condition.f = cond_labs)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(breaks = c(0,2,4,6)) + 
  geom_smooth(method = "lm", se = T) +
  labs(x = "Collective Identification",
       y = "Fairness Judgement",
       title = "Study 2",
       subtitle = "Collective Identification " ) + 
  theme_bw() 

ggsave("Plots/Study2_CI_cong.png", width = 2000, height = 1200, units = "px", scale = 1)


## Facet graph of above model with political party included 
ggplot(CI_data, aes(x = CI_difference, y = fairness, fill = pol_id, colour = pol_id)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), alpha = 0.4) +
  facet_grid(. ~ condition.f, labeller = labeller(condition.f = cond_labs)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Collective Identification",
       y = "Fairness Judgement",
       title = "Study 1",
       subtitle = "Collective Identification " ) + 
  theme_bw()

###########################################################
### Only People who passed the manipulation check  ####
###########################################################

# talking to a real person? 
xtabs(~data$manip_check1) 

432 / (432 + 152) * 100
# prop.test(x = 35, n = 44, p = 0.5, 
# correct = FALSE)

# Others were actually  Assigning tasks? 1 is yes, 2 is no.
data %>% filter(!condition == 1) %>% 
  count(manip_check1) ## Filter out those in condition 1

324 / (324+111) * 100


data_manip <- data_subset %>% 
  filter(manip_check1 == 1 & manip_check2 == 1)

data_manip %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

## Set Up contrasts ## 
levels(data_manip$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_manip$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_manip$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data_manip)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Only those who passed the manipulation check", label = "manip") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)


###########################################################
### Study 1 v. Study 2 Collective Identification  ####
###########################################################

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

t.test(study1$CI_difference, study2$CI_difference)

####################################################################
#### OVERESTIMATOR AND UNDERESTIMATOR DIFFERENCES  ####
####################################################################

## role and political id
data %>% count(role, pol_id)

# Mean of fairness based on role and condition
data %>% 
  group_by(role, condition.f) %>%
  summarise(mean = mean(fairness), n = n())

## 4 (cond) x 2 (role) anova 
summary.aov(aov(fairness ~ condition.f*role, data = data))

equ_ftest(Fstat = 0.75, df1 = 1, df2 = 577, eqbound = 0.2)

## Pairwise Tests

## Self
data %>% filter(condition.f=="self") %>% 
  t.test(fairness ~ role, data = .)

self <- data %>% filter(condition.f=="self") 

tsum_TOST(m1 = mean(subset(self,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          m2 = mean(subset(self,role == "UNDERESTIMATOR")$fairness, na.rm = T), 
          sd1 = sd(subset(self,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          sd2 = sd(subset(self,role == "UNDERESTIMATOR")$fairness, na.rm = T),
          n1 = 78, n2 = 71, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## Other
data %>% filter(condition.f=="other") %>% 
  t.test(fairness ~ role, data = .)

other <- data %>% filter(condition.f=="other") 

tsum_TOST(m1 = mean(subset(other,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          m2 = mean(subset(other,role == "UNDERESTIMATOR")$fairness, na.rm = T), 
          sd1 = sd(subset(other,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          sd2 = sd(subset(other,role == "UNDERESTIMATOR")$fairness, na.rm = T),
          n1 = 57, n2 = 84, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

data %>% filter(condition.f=="ingroup") %>% 
  t.test(fairness ~ role, data = .)

## Ingroup
data %>% filter(condition.f=="ingroup") %>% 
  t.test(fairness ~ role, data = .)

ingroup <- data %>% filter(condition.f=="ingroup")

tsum_TOST(m1 = mean(subset(ingroup,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          m2 = mean(subset(ingroup,role == "UNDERESTIMATOR")$fairness, na.rm = T), 
          sd1 = sd(subset(ingroup,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          sd2 = sd(subset(ingroup,role == "UNDERESTIMATOR")$fairness, na.rm = T),
          n1 = 76, n2 = 73, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## Outgroup
data %>% filter(condition.f=="outgroup") %>% 
  t.test(fairness ~ role, data = .)

outgroup <- data %>% filter(condition.f=="outgroup")

tsum_TOST(m1 = mean(subset(outgroup,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          m2 = mean(subset(outgroup,role == "UNDERESTIMATOR")$fairness, na.rm = T), 
          sd1 = sd(subset(outgroup,role == "OVERESTIMATOR")$fairness, na.rm = T), 
          sd2 = sd(subset(outgroup,role == "UNDERESTIMATOR")$fairness, na.rm = T),
          n1 = 81, n2 = 65, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

#############################################################
#### Dems and Reps ####
#############################################################

data_subset %>% group_by(condition, pol_id) %>% 
  summarise(mean = mean(fairness))

#### DEMS#####
data_dem <- data_subset %>% 
  filter(pol_id == "Dem")

## Set Up contrasts ## 
levels(data_dem$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_dem$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_dem$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data_dem)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))
table

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Democrats Only", label = "dems") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)

#### REPS #####
data_rep <- data_subset %>% 
  filter(pol_id == "Rep")

## Set Up contrasts ## 
levels(data_rep$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(data_rep$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(data_rep$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = data_rep)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))
table

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Republicans Only", label = "reps") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)



#############################################################
#### Political ideology and Extremism ####
#############################################################

ideo_data <- data %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, "ingroup", "outgroup"))) %>% 
  mutate(pol_or2 = pol_or^2)

## Ingroup and outgroup models including linear and quadratic terms for political ideology
ingroup_model <- ideo_data %>% filter(dummy_code == "ingroup") %>% 
  lm(fairness ~ pol_or + pol_or2, data = .,)
summary(ingroup_model)

outgroup_model <- ideo_data %>% filter(dummy_code == "outgroup") %>% 
  lm(fairness ~ pol_or + pol_or2, data = .,)
summary(outgroup_model)

## Graph of fairness by ideology
ggplot(ideo_data, aes(x = pol_or, y = fairness)) +
  facet_grid(. ~ dummy_code) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "red", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2, color = "red", alpha = 0.8) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) + 
  labs(x = "Ideology",
       y = "Fairness",
       title = "Fairness ratings by ideology") + 
  theme_bw()





