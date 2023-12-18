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
data <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)


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
x

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
table

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
####  mismatched participants eliminated: H1, H2, H3 ####
####################################################################

####  Mismatched partisans, political id vs. political orienation, and neg CI  ###
mismatch <- data_subset %>% 
  filter(!CI_difference < 0)

mismatch %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

## Set Up contrasts ## 
levels(mismatch$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(mismatch$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(mismatch$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = mismatch)
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
CI_data <- mismatch %>% filter(condition == 3 | condition == 4) %>%
  mutate(dummy_code = as.factor(ifelse(condition==3, "ingroup", "outgroup"))) %>% ## Dummy code ingroup as 0, outgroup as 1
  filter(CI_difference > 0) ## Filter out people who identified more with their other group. 

model1 <- lm(fairness ~ dummy_code*CI_difference, data = CI_data)
summary(model1)

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) + ## , fill = pol_id, colour = pol_id)) +
  geom_point() +
  facet_grid(. ~ dummy_code) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Collective Identification",
       y = "Fairness",
       title = "Fairness ratings by collective identification and condition") + 
  theme_bw()

## Facet graph of above model with political party included 
ggplot(CI_data, aes(x = CI_difference, y = fairness)) +
  geom_point() +
  facet_grid(. ~ dummy_code) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Collective Identification",
       y = "Fairness",
       title = "Fairness ratings by collective identification and condition") + 
  theme_bw()

####################################################################
####  Only High Identifiers ####
####################################################################

####  Mismatched partisans, political id vs. political orienation, and neg CI  ###
high_ID <- data_subset %>% 
  filter(!CI_difference < 2)

high_ID %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

## Set Up contrasts ## 
levels(high_ID$condition.f)
#H1: self vs. all others. 
contrast1 = c(3, -1, -1, -1)
#H2: ingroup vs. outgroup
contrast2 = c(0,0,1,-1)
#H3: self & ingroup vs. other & outgroup
contrast3 = c(1, -1, 1, -1)

contrasts(high_ID$condition.f) = cbind(contrast1, contrast2, contrast3)
contrasts(high_ID$condition.f)

## CONTRAST RESULTS ## 
fairness.contrast <- aov(fairness ~ condition.f, data = high_ID)
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))
table
## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Politically mismatched participants excluded.",
                       label = "high_ID") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)



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


