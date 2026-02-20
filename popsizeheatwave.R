
#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)
library(car)

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

#get data 
data <-read.csv("./data/heatwavepopulation.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

 
#### MAIN PLOT

#make heatwave, adapted_temp, and weeks_since_heatwave factors
data$heatwave <- factor(data$heatwave, levels = c(0, 1))
  
data$adapted_temp<- factor(data$adapted_temp,
                           levels = c("25", "30", "35"))

data$weeks_since_heatwave <- factor(data$weeks_since_heatwave, 
                                    levels = c( "0", "2", "6", "12", "18"))
  
#calculate summary stats
  summary_data <- data %>%
    group_by(adapted_temp, heatwave, weeks_since_heatwave) %>%
    summarise(mean = mean(alive),
              n=n(),
              sd = sd(alive),
              se = sd / sqrt(n))
  summary_data$heatwave <- factor(summary_data$heatwave, levels = c(0, 1))
  summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$heatwave)
  data$group_id <- interaction(data$adapted_temp, data$heatwave)
  

#create plot
p <- ggplot(summary_data, aes(x = weeks_since_heatwave, y = mean, 
                              color = adapted_temp, shape = heatwave, 
                              group = group_id)) +
    geom_point(size = 4, position = position_dodge(width = 0.6)) +
    scale_x_discrete(expand = c(0.9, 2)) +
    geom_jitter(data = data, #jitter points
                aes(x = weeks_since_heatwave, y = alive, 
                    color = adapted_temp, shape = heatwave, group = group_id),
                position = position_jitterdodge(jitter.width = 0.05, 
                                                dodge.width = 0.6), 
                size = 3, alpha = 0.5) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), #add error bars
                  position = position_dodge(width = 0.6),
                  width = 0) +
    scale_shape_manual(values = c("0" = 16, "1" = 17),  #assign shapes to heatwave
                       name = "Heatwave",
                       labels = c("0" = "Control", "1" = "Treatment")) +
    scale_color_manual(values = c("cornflowerblue", "darkorange", "brown3"),
                       name = "Adapted temperature",
                       labels = c("25°C", "30°C", "35°C"),
                       breaks = c("25", "30", "35"),
                       guide = guide_legend(reverse = TRUE)) +
    xlab("Weeks since heatwave") +     #add x- and y-axis labels
    ylab("Population size (live adult beetles)") +
    scale_x_discrete (breaks = c("0", "2", "6", "12", "18"), 
                      labels = c("0", "2", "6", "12", "18")) +
    theme_tess() +
  theme(
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 15),
    legend.key.size = unit(0.6, "cm"),
    legend.spacing.y = unit(0.4, "cm"))

#windows();p #enables plot to open on a PC
  
quartz()         # Opens a new plotting window
plot(1:10)       # Your plot appears in that window
print(p) #this opens the plot in a new window
  

#saves the plot
ggsave(file="./figures/Taresapopsizefigure.pdf", p, 
       width = 35, height = 22, units = "cm") #close the file before you save


#### ANOVAs

#Global model
#make a new column with specific population ids called "pop_id"
data <- data%>%
  mutate(pop_id = paste(adapted_temp,heatwave,population_id, sep = "_"))

#check that n = 60 for pop_id
n_distinct(data$pop_id)

lmmglobal <- lmer(alive ~ adapted_temp * heatwave * weeks_since_heatwave + (1|pop_id),data = data)
Anova(lmmglobal, type = 2)
#significant three-way interaction

#TARESA SAYS: when I initially ran my global model during our meeting I had not converted weeks_since_heatwave to
#a factor and the three-way interaction was not significant. When I convert it to a factor it is significant. I am
#assuming it is supposed to be a factor and therefore the three-way is significant

#TWO-way ANOVAs

#Week 0
week0data <- data %>%
  filter(weeks_since_heatwave == "0")

lm_week0<-lm(alive~adapted_temp*heatwave,data=week0data)
Anova(lm_week0,type="2")
#signficant interaction

#Week 2
week2data <- data %>%
    filter(weeks_since_heatwave == "2")
  
lm_week2<-lm(alive~adapted_temp*heatwave,data=week2data)
Anova(lm_week2,type="2")
#significant interaction
  
#Week 6
week6data <- data %>%
    filter(weeks_since_heatwave == "6")
  
lm_week6<-lm(alive~adapted_temp*heatwave,data=week6data)
Anova(lm_week6,type="2")
#significant interaction
  
#Week 12
week12data <- data %>%
    filter(weeks_since_heatwave == "12")

lm_week12<-lm(alive~adapted_temp*heatwave,data=week12data)
Anova(lm_week12,type="2")
#significant interaction

#Week 18
week18data <- data %>%
  filter(weeks_since_heatwave == "18")

lm_week18<-lm(alive~adapted_temp*heatwave,data=week18data)
Anova(lm_week18,type="2")
#significant interaction


#ONE-way ANOVAs - only treatment data 

#Week 0
t_week0data <- data %>%
  filter(weeks_since_heatwave == "0",
    heatwave == 1)

lm_week0 <- lm(alive ~ adapted_temp, data = t_week0data)
Anova(lm_week0, type = "II")
#significant interaction

#Week 2
 t_week2data <- data %>%
   filter(weeks_since_heatwave == "2",
          heatwave == 1)
 
 lm_week2 <- lm(alive ~ adapted_temp, data = t_week2data)
 Anova(lm_week2, type = "II")
 #significant interaction
 
  #Week 6
 t_week6data <- data %>%
   filter(weeks_since_heatwave == "6",
          heatwave == 1)
 
 lm_week6 <- lm(alive ~ adapted_temp, data = t_week6data)
 Anova(lm_week6, type = "II")
 #not significant
 
  #Week 12
 t_week12data <- data %>%
   filter(weeks_since_heatwave == "12",
          heatwave == 1)
 
 lm_week12 <- lm(alive ~ adapted_temp, data = t_week12data)
 Anova(lm_week12, type = "II")
 #not significant
 
  #Week 18
  t_week18data <- data %>%
    filter(weeks_since_heatwave == "18",
           heatwave == 1)
  
  lm_week18 <- lm(alive ~ adapted_temp, data = t_week18data)
  Anova(lm_week18, type = "II")
  #not significant
  

#make a new column with specific population ids
data <- data%>%
  mutate(pop_id = paste(adapted_temp,heatwave,population_id, sep = "_"))

#check that n = 60 for pop_id
n_distinct(data$pop_id)


lmmglobal <- lmer(alive ~ adapted_temp * heatwave * weeks_since_heatwave + (1|pop_id),data = data)
Anova(lmmglobal, type = 2)

# because theres a significant relationship between heatwave and weeks since, we ran models for each week
#report p value chi squared value etc. in main results and put this table in supplementary results


  