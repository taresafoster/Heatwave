library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)

theme_tess_3 <- function () { 
  theme_cowplot()+ #cowplot is an existing nice looking plot type thing
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))
  }

data <-read.csv("./data/bodysizeexperiment.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

#.-._.-._.-._.-._.-._.-._.- 2 week survival -._.-._.-._.-._.-._.-._.-.-#

#-------------- Females  ---------------#

female_2weeks <- data %>%
  filter(sex == "f") %>%
  select(survival_twoweeks, dry_weight)

female_2weeks <- female_2weeks %>%
  mutate(survival_twoweeks_binomial = ifelse(survival_twoweeks == "s", 1, 0))

#GLM
model_1 <- glm(survival_twoweeks_binomial ~ dry_weight, 
               data = female_2weeks,
               family = binomial)

#Plot
p_f2w <- ggplot(female_2weeks, aes(x = dry_weight, y = survival_twoweeks_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(title = "Females",
       x = "Body size (g)", 
       y = "Survival") +
  theme_tess_3()

#-------------- Males  ---------------#

#filter original data set
male_2weeks <- data %>%
  filter(sex == "m") %>%
  select(survival_twoweeks, dry_weight)

#mutate to make survival a binomial response
male_2weeks <- male_2weeks %>%
  mutate(survival_twoweeks_binomial = ifelse(survival_twoweeks == "s", 1, 0))

#GLM
model_2 <- glm(survival_twoweeks_binomial ~ dry_weight, 
               data = male_2weeks,
               family = binomial)

#Create plot
p_m2w <- ggplot(male_2weeks, aes(x = dry_weight, y = survival_twoweeks_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(title = "Males",
       x = "Body size (g)", 
       y = "Survival") +
  theme_tess_3()

#-------------- Combined plot ---------------#

#combine the two plots using a grid
combined <- plot_grid(p_f2w, p_m2w,
                      ncol = 2, align = "hv", axis = "tb")

#save the final plot
ggsave(filename = "./figures/Taresabodysizesurvival.pdf",
       plot = combined, width = 22, height = 12, units = "cm", dpi = 300)

# Taresa says: I'm not too happy with the size of text and thickness of the lines
# Please let me know if you think anything should be bigger or smaller and i can rework it

# I also get warning messages when i plot this but I think that that's coming from my
# slope-less GLM. 

# Should I make "A" and "B" panel labels for this one too?

#.-._.-._.-._.-._.-._.-._.- Immediate survival -._.-._.-._.-._.-._.-._.-.-#

#-------------- Females ---------------#

female_immediate <- data %>%
  filter(sex == "f") %>%
  select(survival_immediate, dry_weight)

female_immediate <- female_immediate %>%
  mutate(survival_immediate_binomial = ifelse(survival_immediate == "s", 1, 0))

#GLM
model_3 <- glm(survival_immediate_binomial ~ dry_weight, 
               data = female_immediate,
               family = binomial)

#Plot
p_fi <- ggplot(female_immediate, aes(x = dry_weight, y = survival_immediate_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(x = "Body size (g)", 
       y = "Survival") + 
  theme_tess_3()

#-------------- Males ---------------#

male_immediate <- data %>%
  filter(sex == "m") %>%
  select(survival_immediate, dry_weight)

male_immediate <- male_immediate %>%
  mutate(survival_immediate_binomial = ifelse(survival_immediate == "s", 1, 0))

#GLM
model_4 <- glm(survival_immediate_binomial ~ dry_weight, 
               data = male_immediate,
               family = binomial)

#Plot
p_mi <- ggplot(male_immediate, aes(x = dry_weight, y = survival_immediate_binomial)) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Dead", "Alive")) +
  labs(x = "Body size (g)", 
       y = "Survival") +
  theme_tess_3()


# I still need to combine these two plots to make the supplementary plot but I will 
# do that later

