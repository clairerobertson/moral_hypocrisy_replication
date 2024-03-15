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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER", "ggpubr", "texreg", "xtable", "AER", "ivreg")
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
  filter(cond1_selection==2 | redgreen_selection==2) %>% 
  mutate(complier = 0)

cond1 <- data_subset %>% 
  filter(condition==1)

mean(altruists$fairness)
sd(altruists$fairness)

mean(cond1$fairness)
sd(cond1$fairness)

## T test comparing fairness ratings of altruists and 
t.test(altruists$fairness, cond1$fairness)


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
x <- F_to_eta2(f = c(40.165, 2.88, 0.20), df = c(1,1,1), df_error = c(592, 592, 592), ci = .90, alternative = "greater")
x

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Study 1 Itent to Treat analysis, n = 596", label = "ITT_s1") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("Model",
                            "Contrast 1 - Self vs. Other", 
                            "Contrast 2 - Ingroup vs. Outgroup", 
                            "Contrast 3 - Self/Ingroup vs. Other/Outgroup", 
                            "Risiduals" )

## Print and copy/pase output into latex
print(out_stepwise)

####################################################################
#### OVERESTIMATOR AND UNDERESTIMATOR DIFFERENCES  ####
####################################################################

data <- data %>% 
  mutate(over_under = ifelse(participantRole == "Player2-Underestimator" | participantRole =="Player4-Underestimator", "Under", "Over"))

data %>% count(over_under)

data %>% 
  group_by(over_under, condition.f) %>%
  summarise(mean = mean(fairness), n = n())

## 4 (cond) x 2 (over_under) anova 
table <- summary.aov(aov(fairness ~ condition.f*over_under, data = data))
table 

## Effect Sizes 
x <- F_to_eta2(f = c(0.66, 0.13), df = c(1,3), df_error = c(588, 588), ci = .90, alternative = "greater")
x

## Table 
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Differences between Overestimators and Underestimators in Study 1", label = "over_underS1") 

#Relabel Row Names                     
rownames(out_stepwise) <- c("Condition",
                            "Group (Overestimator or Underestimator)", 
                            "Condition X Group", 
                            "Risiduals" )

## Print and copy/paste output into latex
print(out_stepwise)
## Equivilence test 
equ_ftest(Fstat = 0.66, df1 = 1, df2 = 588, eqbound = 0.2)

## Pairwise Tests

## Self
data %>% filter(condition.f=="self") %>% 
  t.test(fairness ~ over_under, data = .)

self <- data %>% filter(condition.f=="self") 

tsum_TOST(m1 = mean(subset(self,over_under == "Over")$fairness, na.rm = T), 
          m2 = mean(subset(self,over_under == "Under")$fairness, na.rm = T), 
          sd1 = sd(subset(self,over_under == "Over")$fairness, na.rm = T), 
          sd2 = sd(subset(self,over_under == "Under")$fairness, na.rm = T),
          n1 = 74, n2 = 78, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## Other
data %>% filter(condition.f=="other") %>% 
  t.test(fairness ~ over_under, data = .)

other <- data %>% filter(condition.f=="other") 

  tsum_TOST(m1 = mean(subset(other,over_under == "Over")$fairness, na.rm = T), 
            m2 = mean(subset(other,over_under == "Under")$fairness, na.rm = T), 
            sd1 = sd(subset(other,over_under == "Over")$fairness, na.rm = T), 
            sd2 = sd(subset(other,over_under == "Under")$fairness, na.rm = T),
            n1 = 63, n2 = 83, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## Ingroup
data %>% filter(condition.f=="ingroup") %>% 
  t.test(fairness ~ over_under, data = .)

ingroup <- data %>% filter(condition.f=="ingroup")

  tsum_TOST(m1 = mean(subset(ingroup,over_under == "Over")$fairness, na.rm = T), 
            m2 = mean(subset(ingroup,over_under == "Under")$fairness, na.rm = T), 
            sd1 = sd(subset(ingroup,over_under == "Over")$fairness, na.rm = T), 
            sd2 = sd(subset(ingroup,over_under == "Under")$fairness, na.rm = T),
            n1 = 82, n2 = 67, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

## Outgroup
data %>% filter(condition.f=="outgroup") %>% 
  t.test(fairness ~ over_under, data = .)

outgroup <- data %>% filter(condition.f=="outgroup")

  tsum_TOST(m1 = mean(subset(outgroup,over_under == "Over")$fairness, na.rm = T), 
            m2 = mean(subset(outgroup,over_under == "Under")$fairness, na.rm = T), 
            sd1 = sd(subset(outgroup,over_under == "Over")$fairness, na.rm = T), 
            sd2 = sd(subset(outgroup,over_under == "Under")$fairness, na.rm = T),
            n1 = 77, n2 = 72, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)


#### Collective Identification Differences 
t.test(subset(data,over_under == "Over")$CI_difference, 
         subset(data,over_under == "Under")$CI_difference)  

sd(subset(data,over_under == "Over")$CI_difference, na.rm = T)
sd(subset(data,over_under == "Under")$CI_difference, na.rm = T)
  
cohens_d(data$CI_difference ~ data$over_under)

tsum_TOST(m1 = mean(subset(data,over_under == "Over")$CI_difference, na.rm = T), 
           m2 = mean(subset(data,over_under == "Under")$CI_difference, na.rm = T), 
           sd1 = sd(subset(data,over_under == "Over")$CI_difference, na.rm = T), 
           sd2 = sd(subset(data,over_under == "Under")$CI_difference, na.rm = T),
           n1 = 296, n2 = 300, low_eqbound=-0.2, high_eqbound=0.2, eqbound_type = "SMD", alpha=0.05)

####################################################################
#### Repeat Survey Takers groups Excluded ####
####################################################################

## read in data with any group with dupl. person excluded
data_exclude <- read.csv("data/study1_supp_exDF.csv", stringsAsFactors = T)

## clean excluded data
data_exclude <- data_exclude %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3)

## means
data_exclude %>% group_by(condition) %>% 
  summarise(mean = mean(fairness))

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
table

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Participants who were in chatgroups with repeat participants excluded, n = 521", label = "repeats_s1") 
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
table

## Make table for Supplement
out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = ".",
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

## Annotation for graph 
cond_labs <- c("In-group", "Out-group")
names(cond_labs) <- c("ingroup", "outgroup")

anno <- data.frame(x1 = c(0.8, .8), x2 = c(5.5, 5.5), y1 = c(7.3, 7.3), y2 = c(7.3, 7.3), 
                   xlab = c(3,3), ylab = c(7.6,7.6), lab = c("*", "n.s."), 
                   condition.f = c("ingroup", "outgroup"))

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), alpha = 0.4) +
  facet_grid(. ~ condition.f, labeller = labeller(condition.f = cond_labs)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6)) +
  geom_smooth(method = "lm", se = T) +
  geom_segment(data = anno, aes(x = x1, xend = x2, y = y1, yend = y2)) + 
  geom_text(data = anno, aes(x = xlab, y = ylab, label = lab)) +
  labs(x = "Collective Identification",
       y = "Fairness Judgement",
       title = "Experiment 1 - Minimal Groups",
       subtitle = "Collective Identification " ) + 
  theme_bw()

## ggsave("Plots/Study1_CI_cong.png", width = 2000, height = 1200, units = "px", scale = 1)

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
                       booktabs = T,  no.margin = T,  caption = ".",
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

## Filter out those who failed 
data_manip <- data_subset %>% 
  filter(manip_check1 == 1)
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
                       booktabs = T,  no.margin = T,  caption = ".", label = "manip") 
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
ingroup_model <- summary(ideo_data %>% filter(dummy_code == "ingroup") %>% 
  lm(fairness ~ pol_or + pol_or2, data = .,))

out_stepwise <- xtable(ingroup_model, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Effects of poltiical ideology and extremity on on In-group Judgements ",
                       label = "ideo_ingroup1") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("(Intercept)",
                            "Political Orientation", 
                            "Political Orientation (Quadratic Term)") 
print(out_stepwise)                          

### Outgroups ###
outgroup_model <- summary(ideo_data %>% filter(dummy_code == "outgroup") %>% 
  lm(fairness ~ pol_or + pol_or2, data = .,))

out_stepwise <- xtable(outgroup_model, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "Effects of poltiical ideology and extremity on on Out-group Judgements ",
                       label = "indeo_outgroup1") 
#Relabel Row Names                     
rownames(out_stepwise) <- c("(Intercept)",
                            "Political Orientation", 
                            "Political Orientation (Quadratic Term)") 
print(out_stepwise)  


cond_labs <- c("In-group", "Out-group")
names(cond_labs) <- c("ingroup", "outgroup")

## Graph of fairness by ideology
ggplot(ideo_data, aes(x = pol_or, y = fairness)) +
  facet_grid(. ~ dummy_code, labeller = labeller(dummy_code = cond_labs)) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = .5, color = "red", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2, color = "red", alpha = 0.8) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) + 
  labs(x = "Ideology",
       y = "Fairness",
       title = "Fairness Ratings by Political Ideology and Extremity") + 
  theme_bw()

## ggsave("Plots/Study1_ideology_extremity.png", width = 2000, height = 1200, units = "px", scale = 1)


