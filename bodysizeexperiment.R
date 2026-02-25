library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)

theme_tess <- function () { 
  theme_cowplot()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=20))+
    theme(axis.text.y=element_text(size=20))+
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))}

data <-read.csv("./data/bodysizeexperiment.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

data <- data %>%
  mutate(dry_weight_mg = dry_weight * 1000)

data <- data%>%
  mutate(survival_twoweeks_binomial = 
           ifelse(survival_twoweeks == "s", 1, 0))

data <- data%>%
  mutate(survival_immeidate_binomial = 
           ifelse(survival_immediate == "s", 1, 0))


#### 2 WEEK SURVIVAL ####

#-------------- Females  ---------------#

female_2weeks <- data %>%
  filter(sex == "f") %>%
  select(survival_twoweeks_binomial, dry_weight_mg)


#GLM
model_1 <- glm(survival_twoweeks_binomial ~ dry_weight_mg, 
               data = female_2weeks,
               family = binomial)

anova(model_1, test = "Chisq")
#extremely significant, as expected

#Plot
p_f2w <- ggplot(female_2weeks, 
                aes(x = dry_weight_mg, y = survival_twoweeks_binomial)) +
  geom_jitter(height = 0.01, width = 0, size=3,shape=16, colour="black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se=FALSE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Died", "Survived")) +
  labs(title = "Females",
       x = "Body size (mg)", 
       y = "Survival") +
  theme_tess()

#windows();p_f2w

#-------------- Males  ---------------#

#filter original data set
male_2weeks <- data %>%
  filter(sex == "m") %>%
  select(survival_twoweeks_binomial, dry_weight_mg)

#GLM
model_2 <- glm(survival_twoweeks_binomial ~ dry_weight_mg, 
               data = male_2weeks,
               family = binomial)

anova(model_2, test = "Chisq")
#extremely significant, as expected


#Create plot
p_m2w <- ggplot(male_2weeks, aes(x = dry_weight_mg, y = survival_twoweeks_binomial)) +
  geom_jitter(height = 0.01, width = 0,colour="black",size=3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Died", "Survived")) +
  labs(title = "Males",
       x = "Body size (mg)", 
       y = "") +
  theme_tess()

#windows();p_m2w

#-------------- Combined plot ---------------#

#combine the two plots using a grid
combined <- plot_grid(p_f2w, p_m2w,
                      ncol = 2, align = "hv", axis = "tb")

#save the final plot
ggsave(filename = "./figures/Taresabodysizesurvival.pdf",
       plot = combined, width = 35, height = 19, units = "cm", dpi = 300)

# Taresa says: I'm not too happy with the size of text and thickness of the lines
# Please let me know if you think anything should be bigger or smaller and i can rework it
#TESS SAYS: i played around with the aesthetics of this plot, let me know what you think

# I also get warning messages when i plot this but I think that that's coming from my
# slope-less GLM. 

# Should I make "A" and "B" panel labels for this one too?
#TESS SAYS: yes!


#### IMMEDIATE SURVIVAL ####

# TESS SAYS: repeat everything above here for the supp mat

#-------------- Females ---------------#

female_immediate <- data %>%
  filter(sex == "f") %>%
  select(survival_immediate, dry_weight_mg)

female_immediate <- female_immediate %>%
  mutate(survival_immediate_binomial = ifelse(survival_immediate == "s", 1, 0))

#GLM
model_3 <- glm(survival_immediate_binomial ~ dry_weight_mg, 
               data = female_immediate,
               family = binomial)

#Plot
p_fi <- ggplot(female_immediate, aes(x = dry_weight_mg, y = survival_immediate_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(x = "Body size (mg)", 
       y = "Survival") + 
  theme_tess()

#-------------- Males ---------------#

male_immediate <- data %>%
  filter(sex == "m") %>%
  select(survival_immediate, dry_weight_mg)

male_immediate <- male_immediate %>%
  mutate(survival_immediate_binomial = ifelse(survival_immediate == "s", 1, 0))

#GLM
model_4 <- glm(survival_immediate_binomial ~ dry_weight_mg, 
               data = male_immediate,
               family = binomial)

#Plot
p_mi <- ggplot(male_immediate, aes(x = dry_weight_mg, y = survival_immediate_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(x = "Body size (mg)", 
       y = "Survival") +
  theme_tess_3()

# I still need to combine these two plots to make the supplementary plot but I will 
# do that later
