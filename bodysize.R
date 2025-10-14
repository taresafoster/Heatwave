
#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)
library(car) #for running type 2 anovas
library(emmeans) #for running post-hoc tests on type 2 anovas

#Tess's ggplot theme
theme_tess <- function () { 
  theme_cowplot()+ #cowplot is an existing nice looking plot type thing
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=20))+
    theme(axis.text.y=element_text(size=20))+
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))
}

data <-read.csv("./data/preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

####MAIN PLOT

#make adapted_temp a factor
data$adapted_temp <- factor(data$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

#calculate summary stats
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight),
            n=n(),
            sd = sd(weight),
            se = sd / sqrt(n))

summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)

#create plot
p <- ggplot(summary_data, aes(x = adapted_temp, y = mean, color = sex)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  
  #jitter the points
  geom_jitter(data = data,
              aes(x = adapted_temp, y = weight, color = sex),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  
  #add error bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4),
                width = 0) +
  
  #assign colours to sex
  scale_color_manual(
    values = c("f" = "palevioletred", "m" = "steelblue3"),   # or whatever colors you like
    labels = c("f" = "Female", "m" = "Male"),
    name = "Sex")+
  
  #assign x- and y-axis labels
  xlab("Adapted temperature") +
  ylab("Body size (g)") +
  
  #assign x-axis scale
  scale_x_discrete (breaks = c("25°C", "30°C", "35°C"), labels = c("25°C", "30°C", "35°C"))+ 
  theme_tess()

windows();p #enables plot to open on a PC

print(p) #this opens the plot in a new window

#saves the plot
ggsave(file="Figures/Taresabodysizefigure.pdf", p, width = 22, height = 22, units = "cm") #close the file before you save

####ANOVA

#TWO-way ANOVA
#TESS SAYS: changed this to the way I do ANOVAS (see popsizeheatave file for explanation)

lm_bodysize <- lm(weight ~ adapted_temp * sex, data = data)
Anova(lm_bodysize, type="2")
#significant interaction
  
#Tukey test
#TESS SAYS: this is how you run a tukey test when you do your anovas as above (emmeans package)

emmeans(lm_bodysize, pairwise ~ adapted_temp | sex, adjust = "tukey")
