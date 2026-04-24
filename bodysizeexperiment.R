# Load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)

# Import ggplot theme for plots
theme_tess <- function () { 
  theme_cowplot()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=20))+
    theme(axis.text.y=element_text(size=20))+
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))
}

# Import data
data <-read.csv("./data/bodysizeexperiment.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

# Convert grams to milligrams 
data <- data %>%
  mutate(weight_mg = weight * 1000)

# Create a new column with binary variables indicating survival or death two weeks after heatwave
data <- data%>%
  mutate(survival_twoweeks_binomial = 
           ifelse(survival_twoweeks == "s", 1, 0))

#### 2 WEEK SURVIVAL ####

#-------------- Females  ---------------#

# Filter data for female beetles only
female_2weeks <- data %>%
  filter(sex == "f") %>%
  select(survival_twoweeks_binomial, weight_mg)


# Construct a general linear model
model_1 <- glm(survival_twoweeks_binomial ~ weight_mg, 
               data = female_2weeks,
               family = binomial)

# Run an ANOVA
anova(model_1, test = "Chisq")
#significant

# Generate plot
p_f2w <- ggplot(female_2weeks, 
                aes(x = weight_mg, y = survival_twoweeks_binomial)) +
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

# Filter data for male beetles only
male_2weeks <- data %>%
  filter(sex == "m") %>%
  select(survival_twoweeks_binomial, weight_mg)

# Construct a general linear model
model_2 <- glm(survival_twoweeks_binomial ~ weight_mg, 
               data = male_2weeks,
               family = binomial)

# Run an ANOVA
anova(model_2, test = "Chisq")
#significant

# Generate plot
p_m2w <- ggplot(male_2weeks, aes(x = weight_mg, y = survival_twoweeks_binomial)) +
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

# Generate a combined plot
combined_2w <- plot_grid(p_f2w, p_m2w,
                      ncol = 2, align = "hv", axis = "tb")

# Save the combined plot
ggsave(filename = "./figures/bodysizesurvival_2w.pdf",
       plot = combined_2w, width = 35, height = 19, units = "cm", dpi = 300)
