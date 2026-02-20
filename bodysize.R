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

#### PRE HEATWAVE ####

data <-read.csv("./data/preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

#make adapted_temp a factor
data$adapted_temp <- factor(data$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

#order the x axis raw data points
data <- data %>% 
  mutate(sex_temp = factor(paste(sex, adapted_temp),
      levels = c( "f 25°C", "f 30°C", "f 35°C", "m 25°C", "m 30°C", "m 35°C")))

#calculate summary stats
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight),
            n=n(),
            sd = sd(weight),
            se = sd / sqrt(n))

#order the x axis summary stats
summary_data <- summary_data %>%
  mutate(sex_temp = factor(paste(sex, adapted_temp),
      levels = c("f 25°C", "f 30°C", "f 35°C", "m 25°C", "m 30°C", "m 35°C")))

summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)

#------Plot------#

p_pre <- ggplot(summary_data, aes(x = sex, y = mean, color = adapted_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  geom_jitter(data = data,aes(x = sex, y = weight, color = adapted_temp),  #jitter the points
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), #add error bars
                position = position_dodge(width = 0.4), width = 0) +
  scale_color_manual(values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),  #assign colours to sex
                     name = "Adapted temperature") +
  scale_x_discrete(labels = c("Female", "Male")) +   #label the x-axis categories
  scale_y_continuous(limits = c(0.0010, 0.0016))+
  xlab("Sex") + #assign x- and y-axis labels
  ylab("Body size (g)") +
  labs(title = "Before experiment")+ #tess added this
  #guides(fill = "none", color = "none", shape = "none", linetype = "none")+#tess added this to suppress the legend
  theme_tess()+ 
  theme(aspect.ratio = 1)

#windows();p_pre #enables plot to open on a PC
#print(p_pre) #this opens the plot in a new window

#------Stats------#

#Two-way ANOVA
lm_bodysize <- lm(weight ~ adapted_temp * sex, data = data)
Anova(lm_bodysize, type="2")
#significant interaction
  
#Tukey test
emmeans(lm_bodysize, pairwise ~ adapted_temp | sex, adjust = "tukey")


####POST HEATWAVE BODY SIZE#### 

data2 <-read.csv("./data/postheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

#make adapted_temp a factor
data2$adapted_temp <- factor(data2$adapted_temp,
                            levels = c(25, 30, 35),
                            labels = c("25°C", "30°C", "35°C"))

#calculate the mean for each population/group
means <- data2 %>%
  drop_na(weight) %>%
  group_by(heatwave, adapted_temp, sex, group_id) %>%
  summarise(pop_mean = mean(weight))

#calculate summary stats using the means for each population
summary_data2 <- means %>%
  group_by(heatwave, adapted_temp, sex) %>%
  summarise(mean = mean(pop_mean),
            n=n(),
            sd = sd(pop_mean),
            se = sd / sqrt(n))

#make heatwave a factor in all data sets
summary_data2$heatwave <- factor(summary_data2$heatwave,
                                levels = c(0, 1),
                                labels = c("Control", "Treatment"))
data2$heatwave <- factor(data2$heatwave, levels = c(0, 1),
                        labels = c("Control", "Treatment"))

means$heatwave <- factor(means$heatwave, levels = c(0, 1),
                         labels = c("Control", "Treatment"))

summary_data2$group_id <- interaction(summary_data2$adapted_temp, summary_data2$sex)
data2$group_id <- interaction(data2$adapted_temp, data2$sex)

#order the x axis in all data sets
data2 <- data2 %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                                levels = c("f Control", 
                                           "f Treatment", "m Control", 
                                           "m Treatment")))
means <- means %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", 
                                          "m Control", "m Treatment")))
summary_data2 <- summary_data2 %>%
  mutate(sex_heatwave = factor(paste(sex, heatwave), 
                               levels = c("f Control", "f Treatment", 
                                          "m Control", "m Treatment")))

#------Plot------#

#create custom "x_pos" spacing for x axis
summary_data2 <- summary_data2 %>%
  mutate(x_pos = case_when(
    sex_heatwave == "f Control" ~ 1,
    sex_heatwave == "f Treatment" ~ 1.6,
    sex_heatwave == "m Control" ~ 2.4,
    sex_heatwave == "m Treatment" ~ 3.0))

means <- means %>%
  mutate(x_pos = case_when(
    sex_heatwave == "f Control" ~ 1,
    sex_heatwave == "f Treatment" ~ 1.6,
    sex_heatwave == "m Control" ~ 2.4,
    sex_heatwave == "m Treatment" ~ 3.0))

p_post <- ggplot(summary_data2, aes(x = x_pos, y = mean, 
                color = adapted_temp, shape = heatwave)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  geom_jitter(data = means,   aes(x = x_pos, y = pop_mean, 
                                  color = adapted_temp,shape = heatwave),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), #add error bars
                position = position_dodge(width = 0.4),
                width = 0) +
  scale_color_manual(
    values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),   #assign colours to temperatures
    name = "Adapted temperature")+
  scale_shape_manual(values = c("Control" = 16, "Treatment" = 17),  #assign shapes to heatwave
                     name = "Heatwave") +  #label the x-axis categories
  scale_y_continuous(limits = c(0.0010, 0.0016))+
  scale_x_continuous(
    breaks = c(1, 1.6, 2.4, 3.0),
    labels = c("Female", "Female", "Male", "Male")) +
  xlab("Sex") + #assign x- and y-axis labels
  ylab("") +
  labs(title = "End of experiment")+ 
  theme_tess()+ 
  theme(aspect.ratio = 1)

#------Stats------#

#Two-way ANOVA
lm_bodysize_post <- lm(weight ~ adapted_temp * sex, data = data2)
Anova(lm_bodysize_post, type="2")
#significant interaction

#Tukey test
emmeans(lm_bodysize_post, pairwise ~ adapted_temp | sex, adjust = "tukey")


####POST HEATWAVE FECUNDITY#### 

f_data <-read.csv("./data/heatwavefecundity.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

#make heatwave and adpated_temp factors
f_data$heatwave <- factor(f_data$heatwave, levels = c("c", "t"))
#make adapted_temp a factor
f_data$adapted_temp <- factor(f_data$adapted_temp,
                             levels = c(25, 30, 35),
                             labels = c("25°C", "30°C", "35°C")) #make adapted temp into a factor so we can color code by it in the plot

#filter to only the controls and remove all of the "nb" (no beetle) tubes 
f_data_filtered <- f_data %>%
  filter(
    heatwave == "c",
    notes != "nb")

#group by temp and population (heatwave is only "c" now)
#calculate the means for each population
egg_means <- f_data_filtered%>%
  group_by(adapted_temp, population_id) %>%
  summarise(mean_egg_count = mean(egg_count, na.rm = TRUE))

#calculate summary stats for each treatment
f_summary_data <- egg_means %>%
  group_by(adapted_temp) %>%
  summarise(mean = mean(mean_egg_count),
            n=n(),
            sd = sd(mean_egg_count),
            se = sd / sqrt(n))

#------Plot------#

#create custom spacing for x axis
f_summary_data <- f_summary_data %>%
  mutate(x_pos_f = case_when(
    adapted_temp == "25°C" ~ 1.1,
    adapted_temp == "30°C" ~ 1.7,
    adapted_temp == "35°C" ~ 2.3))

egg_means <- egg_means %>%
  mutate(x_pos_f = case_when(
    adapted_temp == "25°C" ~ 1.1,
    adapted_temp == "30°C" ~ 1.7,
    adapted_temp == "35°C" ~ 2.3))

f_p <- ggplot(f_summary_data, aes(x = x_pos_f, y = mean, color = adapted_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_jitter(data = egg_means, #jitter points
              aes(x = x_pos_f, y = mean_egg_count, , 
                  color = adapted_temp,
                  group = population_id),
              position = position_jitterdodge(jitter.width = 0.06, 
                                              dodge.width = 0.2), 
              size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), #add error bars
                position = position_dodge(width = 0.6),
                width = 0) +
  scale_color_manual(
    values = c("25°C" = "cornflowerblue", "30°C" = "darkorange", "35°C" = "brown3"),   #assign colours to sex
    name = "Adapted temperature")+
  scale_x_continuous(
    breaks = c(1.1, 1.7, 2.3),
    labels = c("25°C", "30°C", "35°C"),
    limits = c(0.8, 2.7)) +
  xlab("Adapted temperature") +     #add x- and y-axis labels
  ylab("Number of eggs per female") +
  labs(title = "After heatwave")+ 
  theme_tess()+ 
  theme(aspect.ratio = 1)

#Stats
lm_egg_count <- lm(mean_egg_count ~ adapted_temp, data = egg_means)
Anova(lm_egg_count)

#### COMBINED FIGURE ####

#adjust the axis text size for the combined plot
p_post <- p_post + theme(axis.text.x = element_text(size = 18))

#remove the individual legends from both plots
p_pre_noleg  <- p_pre  + theme(legend.position = "none")
p_post_noleg <- p_post + theme(legend.position = "none")
f_p_noleg <- f_p + theme(legend.position = "none")

#create a new legend 
legend <- get_legend(p_post +
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 18),
          legend.text  = element_text(size = 16),
          legend.key.size = unit(0.8, "cm"),
          legend.spacing.y = unit(0.2, "cm")))

#center the legend
legend_centered <- ggdraw() +
  draw_grob(legend, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5)

#create top half of combined figure
top_row <- plot_grid(p_pre_noleg, p_post_noleg, labels = c("A", "B"),
                     label_size = 20, ncol = 2, align = "hv")

#create bottom half of combined figure
bottom_row <- plot_grid(f_p_noleg, legend_centered, labels = c("C", ""),
                        label_size = 20, ncol = 2, align = "hv")

#create combined plot
combined <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))


ggsave(filename = "./figures/Taresabodysizebeforeafter.pdf",
         plot = combined, width = 30, height = 30, units = "cm", dpi = 300)

#------Stats------#

#Three way anova
lm_bodysize <- lm(weight ~ adapted_temp * sex*heatwave, data = data2)
Anova(lm_bodysize, type="2")

#Tukey test
emm <- emmeans(lm_bodysize, ~ heatwave | sex, data = data2)
pairs(emm, adjust = "tukey")

#significant interaction between sex and heatwave
#strong effect of sex (females larger), significant effect of adapted temp (cold adapted larger)
#let's discuss what this means


