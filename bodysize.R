
#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)
library(car) #for running type 2 anovas
library(emmeans) #for running post-hoc tests on type 2 anovas

#Tess's ggplot theme
<<<<<<< HEAD
theme_tess_2 <- function () { 
  theme_cowplot()+ #cowplot is an existing nice looking plot type thing
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.y=element_text(size=15))+
    theme(axis.text.x=element_text(size=15))+
=======
theme_tess <- function () { 
  theme_cowplot()+ #cowplot is an existing nice looking plot type thing
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
    theme(axis.text.x=element_text(size=20))+
    theme(axis.text.y=element_text(size=20))+
>>>>>>> 990a70132cc741136d2dd1f368d96ec8ce3456ec
    theme(axis.title.x=element_text(size=20))+
    theme(axis.title.y=element_text(size=20))+
    theme(plot.title = element_text(hjust = 0.5,size=20))+
    theme(axis.title.y=element_text(size=20))
}

<<<<<<< HEAD
#.-._.-._.-._.-._.-._.-._.- Pre-heatwave -._.-._.-._.-._.-._.-._.-.-#

data <-read.csv("./data/preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

=======
data <-read.csv("./data/preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

####MAIN PLOT

>>>>>>> 990a70132cc741136d2dd1f368d96ec8ce3456ec
#make adapted_temp a factor
data$adapted_temp <- factor(data$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

<<<<<<< HEAD
#order the x axis raw data points
data <- data %>% 
  mutate(sex_temp = factor(paste(sex, adapted_temp),
      levels = c( "f 25°C", "f 30°C", "f 35°C", "m 25°C", "m 30°C", "m 35°C")))

#calculate summary stats
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight), 
=======
#calculate summary stats
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight),
>>>>>>> 990a70132cc741136d2dd1f368d96ec8ce3456ec
            n=n(),
            sd = sd(weight),
            se = sd / sqrt(n))

<<<<<<< HEAD
#order the x axis summary stats
summary_data <- summary_data %>%
  mutate(sex_temp = factor(paste(sex, adapted_temp),
      levels = c("f 25°C", "f 30°C", "f 35°C", "m 25°C", "m 30°C", "m 35°C")))

summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)

#------Plot------#

p_pre <- ggplot(summary_data, aes(x = sex, y = mean, color = adapted_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  
  #jitter the points
  geom_jitter(data = data,aes(x = sex, y = weight, color = adapted_temp),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  
  #add error bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4), width = 0) +
  
  #assign colours to sex
  scale_color_manual(values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),
                     name = "Adapted temperature") +
  
  #label the x-axis categories
  scale_x_discrete(labels = c("Female", "Male")) +
  
  #assign x- and y-axis labels
  xlab("Sex") +
  ylab("Body size (g)") +
  
  theme_tess_2()

#windows();p #enables plot to open on a PC
#print(p_pre) #this opens the plot in a new window

#------Stats------#

#Two-way ANOVA
lm_bodysize <- lm(weight ~ adapted_temp * sex, data = data)
Anova(lm_bodysize, type="2")
#significant interaction
  
#Tukey test
emmeans(lm_bodysize, pairwise ~ adapted_temp | sex, adjust = "tukey")


#.-._.-._.-._.-._.-._.-._.- Post-heatwave -._.-._.-._.-._.-._.-._.-.-#

data <-read.csv("./data/postheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

#make adapted_temp a factor
data$adapted_temp <- factor(data$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

#calculate the mean for each population/group
means <- data %>%
  drop_na(weight) %>%
  group_by(heatwave, adapted_temp, sex, group_id) %>%
  summarise(pop_mean = mean(weight))

#calculate summary stats using the means for each population
summary_data <- means %>%
  group_by(heatwave, adapted_temp, sex) %>%
  summarise(mean = mean(pop_mean),
            n=n(),
            sd = sd(pop_mean),
            se = sd / sqrt(n))

#make heatwave a factor in all data sets
summary_data$heatwave <- factor(summary_data$heatwave,
                                levels = c(0, 1),
                                labels = c("Control", "Treatment"))
data$heatwave <- factor(data$heatwave, levels = c(0, 1),
                        labels = c("Control", "Treatment"))

means$heatwave <- factor(means$heatwave, levels = c(0, 1),
                         labels = c("Control", "Treatment"))

summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)

#order the x axis is all data sets
data <- data %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                                levels = c("f Control", "f Treatment", "m Control", "m Treatment")))
means <- means %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", "m Control", "m Treatment")))
summary_data <- summary_data %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", "m Control", "m Treatment")))

#------Plot------#

p_post <- ggplot(summary_data, aes(x = sex_heatwave, y = mean, color = adapted_temp, shape = heatwave)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  
  #jitter the points
  geom_jitter(data = means,
              aes(x = sex_heatwave, y = pop_mean, color = adapted_temp, shape = heatwave),
=======
summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)

#create plot
p <- ggplot(summary_data, aes(x = adapted_temp, y = mean, color = sex)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  
  #jitter the points
  geom_jitter(data = data,
              aes(x = adapted_temp, y = weight, color = sex),
>>>>>>> 990a70132cc741136d2dd1f368d96ec8ce3456ec
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  
  #add error bars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4),
                width = 0) +
  
  #assign colours to sex
  scale_color_manual(
<<<<<<< HEAD
    values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),
    name = "Adapted temperature")+
  
  #assign shapes to heatwave
  scale_shape_manual(values = c("Control" = 16, "Treatment" = 17),  
                     name = "Heatwave") +
  
  #label the x-axis categories
  scale_x_discrete(
    breaks = c("f Control", "f Treatment", "m Control", "m Treatment"),
    labels = c("Female", "Female", "Male", "Male")) +
  
  #assign x- and y-axis labels
  xlab("Sex") +
  ylab("Body size (g)") +
  
  theme_tess_2()

#windows();p #enables plot to open on a PC
#print(p) #this opens the plot in a new window

#------Stats------#

#Two-way ANOVA
lm_bodysize <- lm(weight ~ adapted_temp * sex, data = data)
Anova(lm_bodysize, type="2")
#significant interaction

#Tukey test
emmeans(lm_bodysize, pairwise ~ adapted_temp | sex, adjust = "tukey")

#.-._.-._.-._.-._.-._.-._.- Combined figure -._.-._.-._.-._.-._.-._.-.-#

#combine the two plots using a grid
combined <- plot_grid(p_pre, p_post, labels = c("A", "B"), 
                       ncol = 2, align = "hv", axis = "tb")

#create a single set of legends that apply to both plots
legend <- get_legend(
  p_post + theme(legend.position = "right"))

#remove the individual legends from both plots
p_pre_noleg  <- p_pre  + theme(legend.position = "none")
p_post_noleg <- p_post + theme(legend.position = "none")

#combine the new legend-less plots with the new legend
combined <- plot_grid(plot_grid(p_pre_noleg, p_post_noleg, labels = c("A", "B"),
                      label_size = 20, ncol = 2, align = "hv"), legend, rel_widths = c(1, 0.23))

#save the final plot
ggsave(filename = "./figures/Taresabodysizetemp.pdf",
  plot = combined, width = 28, height = 12, units = "cm", dpi = 300)

# Taresa says: Should the plots have the same y axis scale/one scale for both panels
# Also I was having trouble making it so that female and male only appear once on the x axis of the second panel
# Should the two females and two males on panel B be closer together as well? 
=======
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
>>>>>>> 990a70132cc741136d2dd1f368d96ec8ce3456ec
