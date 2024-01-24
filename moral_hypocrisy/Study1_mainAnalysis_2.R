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

packages <- c("tidyverse", "readr",  "lme4", "qualtRics", "car", "emmeans", "effectsize", "moments", "pequod", "reghelper", "TOSTER", "ggpubr", "ggplot2")
ipak(packages)

## Load in Raw Data
data <- read.csv("data/study1_analysisDF.csv", stringsAsFactors = T)

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

# talking to a real person? 
xtabs(~data$manip_check1)
# prop.test(x = 35, n = 44, p = 0.5, 
         # correct = FALSE)

# Others were actually  Assigning tasks? 1 is yes, 2 is no.
data %>% filter(!condition == 1) %>% 
  count(manip_check1) ## Filter out those in condition 1

#selected randomizer in self condition? 1 = choose, 2 = randomizer
data %>% filter(condition == 1) %>% 
  count(cond1_selection)

#selected green task; 1 = green, 2 = red 
data %>% count(redgreen_selection)

## Political Identificaiton in each group
data %>% 
  count(pol_or)
 
#Eliminate altruists, attention check failures. 
data_subset <- data %>% 
  filter(is.na(cond1_selection)|cond1_selection==1) %>% ## eliminate those who used randomizer
  filter(is.na(redgreen_selection)|redgreen_selection==1) %>% ## eliminate those who chose the red task
  filter(attn_check==3) ## Eliminate attention check failures. 

###############################
#####  MAIN ANALYSIS ##########
###############################

## Means
data_subset %>% group_by(condition) %>% 
  summarise(mean = mean(fairness), 
            n = n())

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
table <- summary.aov(fairness.contrast, split = list(condition.f = list("Self vs. Others" = 1, "Ingroup vs. Outgroup" = 2, "Self/Ingroup vs. Other/Outgroup" = 3)))
table

out_stepwise <- xtable(table, 
                       dcolumn = T,  stars = c(0.05, 0.01, 0.001), 
                       booktabs = T,  no.margin = T,  caption = "ITT", label = "ITT")


## EFFECT SIZE FOR CONTRASTS ### 
x <- F_to_eta2(f = c(1.27, 3.22, 0.15), df = c(1,1,1), df_error = c(536, 536, 536), ci = .90, alternative = "greater")
x
## Equiv. Test
x <- equ_ftest(Fstat = 0.15, df1 = 1, df2 = 536, eqbound = 0.2)
summary(x)

## H6: Collective Identification ## 
## does effect of ingroup or outgroup status on fairness depends on collective identification?## 

hist(data$CI_difference)

## Above chance identification? 
x <- t.test(data$CI_difference, mu = 0, alternative = "two.sided")
sd(data$CI_difference, na.rm = T)

cohens_d(data$CI_difference ~ 1, mu = 0)

## Subset just ingroup and outgroup judgements
CI_data <- data_subset %>% filter(condition == 3 | condition == 4)

## Means across groups
CI_data %>% group_by(condition.f) %>% 
  summarise(mean = mean(fairness))

## Check whether greater collective identification predicts fairness.
model1 <- lm(fairness ~ condition.f*CI_difference, data = CI_data)
summary(model1)

cond_labs <- c("In-group", "Out-group")
names(cond_labs) <- c("ingroup", "outgroup")

## Facet graph of above model
ggplot(CI_data, aes(x = CI_difference, y = fairness)) + ## , fill = pol_id, colour = pol_id)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), alpha = 0.4) +
  facet_grid(. ~ condition.f, labeller = labeller(condition.f = cond_labs)) + 
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(breaks = c(-2,0,2,4,6)) +
  geom_smooth(method = "lm", se = T) +
  labs(x = "Collective Identification",
       y = "Fairness Judgement",
       title = "Study 1 - Minimal Groups",
       subtitle = "Collective Identification " ) + 
  theme_bw()

## Saving Graphs
ggsave("Plots/Study1_CI_all.png", width = 3000, height = 1500, units = "px", scale = 1)

## If Interaction is significant. 0 = ingroup, 1 = outgroup. 
simple_slopes(model1, levels = list(condition.f = c("ingroup", "outgroup")))

##########################
#### Plotting ############
##########################

set.seed(12012023)

### ALTRUISTS EXCLUDED ####

## Overeall Mean Fairnes Graph
p <- ggplot(data_subset, aes(x = condition.f, y = fairness, fill = condition.f)) + 
  geom_violin(alpha = .2) + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 1, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2, color = "black", alpha = 0.8) + 
  geom_segment(x = .8, xend = 4.2, y = 7.3, yend = 7.3) + 
  annotate("text", x = 2.5, y = 7.6, label = "n.s.") + 
  scale_fill_brewer(palette = "Dark2", labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  theme_bw()
 
## Graph Labeling
p <- p + labs(title = "Altruists Excluded",
         x = "Condition",
         y = "Fairness Ratings", 
         color = "Condition",
         fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back
p

#### ALTRUISTS INCLUDED ####

data <- data %>% 
  mutate(altruists = ifelse(redgreen_selection == 2 | cond1_selection == 2, "fair", NA)) %>% 
  mutate(altruists = ifelse(is.na(altruists), "unfair", "fair"))

## Overeall Mean Fairnes Graph
p2 <- ggplot(data, aes(x = condition.f, y = fairness, fill = condition.f)) + 
  geom_violin(alpha = .2) + 
  scale_x_discrete(limits = c("self", "other", "ingroup", "outgroup"), 
                   labels = c("Self", "Other", "Ingroup", "Outgroup")) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) + 
  geom_jitter(aes(colour = altruists, fill = condition.f), position = position_jitter(width = 0.25, height = 0.25), alpha = 0.4) +
  scale_color_manual(values = c("fair" = "red", "unfair" = "black")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 1, color = "black", alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", size = 2, color = "black", alpha = 0.8) + 
  annotate("text", x = 1, y = 7.6, label = "***", size = 16/.pt) + 
  scale_fill_brewer(palette = "Dark2", labels = c("Self", "Other", "Ingroup", "Outgroup")) + 
  theme_bw()

## Graph Labeling
p2 <- p2 + labs(title = "Altruists Included (in Red)",
         x = "Condition",
         y = "Fairness Ratings", 
         color = "Condition",
         fill = "Condition") + 
  theme(legend.position = "none") # Can remove if you want the legend back
p2

plot <- ggarrange(p, p2, 
          labels = c("A.", "B.")) 

plot <- annotate_figure(plot, top = text_grob("Fairness Ratings in Study 1, Minimal Groups", 
                                      color = "Black", size = 14))

plot

ggsave("Plots/Study1_fairness.png", width = 3000, height = 1500, units = "px", scale = 1)






TOSTtwo(fairness.contrast, contrast3, low_eqbound_d = -0.2, high_eqbound_d = 0.2, alpha = 0.05)