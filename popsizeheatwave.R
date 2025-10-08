
#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(dplyr)

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

#get data in
data <-read.csv("./data/heatwavepopulationwk12.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))
 
#### MAIN PLOT

#make heatwave and adpated_temp factors
  data$heatwave <- factor(data$heatwave, levels = c(0, 1))
  
  data$adapted_temp<-as.factor(data$adapted_temp) #make adapted temp into a factor so we can color code by it in the plot
  
 
 # controls <- data %>%
 #   filter(heatwave == "0", weeks_since_heatwave %in% c("0", "2", "6", "12"))
  
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
  
  # Convert weeks_since_heatwave to a factor with levels
  data$weeks_since_heatwave <- factor(data$weeks_since_heatwave, 
                                      levels = c( "0", "pad2", "2", "pad3", "6", "pad6", "12"))
  
  summary_data$weeks_since_heatwave <- factor(summary_data$weeks_since_heatwave, 
                                              levels = c( "0", "pad2", "2", "pad3", "6", "pad6", "12"))
  
  #create plot
  p <- ggplot(summary_data, aes(x = weeks_since_heatwave, y = mean, color = adapted_temp, shape = heatwave, group = group_id)) +
    geom_point(size = 4, position = position_dodge(width = 0.8)) +
    scale_x_discrete(expand = c(0.9, 2)) +
    
    #jitter points 
    geom_jitter(data = data,
                aes(x = weeks_since_heatwave, y = alive, color = adapted_temp, shape = heatwave, group = group_id),
                position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8), 
                size = 3, alpha = 0.5) +
    
    #add error bars
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(width = 0.8),
                  width = 0) +
    
    #assign shapes to heatwave
    scale_shape_manual(values = c("0" = 16, "1" = 17),  
                       name = "Heatwave",
                       labels = c("0" = "Control", "1" = "Treatment")) +
    
    #assign colours to adapted_temp
    scale_color_manual(values = c("cornflowerblue", "darkorange", "brown3"),
                       name = "Adapted temperature",
                       labels = c("25°C", "30°C", "35°C"),
                       breaks = c("25", "30", "35"),
                       guide = guide_legend(reverse = TRUE)) +
    
    #add x- and y-axis labels
    xlab("Weeks since heatwave") +
    ylab("Population size") +
    scale_x_discrete (breaks = c("0", "2", "6", "12"), labels = c("0", "2", "6", "12")) +
    theme_tess()
  
  quartz()         # Opens a new plotting window
  plot(1:10)       # Your plot appears in that window
  print(p) #this opens the plot in a new window
  
  windows();p #enables plot to open on a PC

#saves the plot
ggsave(file="Figures/Taresapopsizefigure.pdf", p, width = 22, height = 22, units = "cm") #close the file before you save


#### ANOVAs

#TWO-way ANOVAs
  #Week 0
 week0data <- data %>%
  filter(weeks_since_heatwave == "0")
  
 week0anova <- aov(alive ~ adapted_temp * heatwave, data = week0data)
  
 summary(week0anova)

  #Week 2
 week2data <- data %>%
    filter(weeks_since_heatwave == "2")
  
 week2anova <- aov(alive ~ adapted_temp * heatwave, data = week2data)
  
 summary(week2anova)
  
  #Week 6
 week6data <- data %>%
    filter(weeks_since_heatwave == "6")
  
 week6anova <- aov(alive ~ adapted_temp * heatwave, data = week6data)
  
 summary(week6anova)
  
   #Week 12
 week12data <- data %>%
    filter(weeks_since_heatwave == "0")
  
 week12anova <- aov(alive ~ adapted_temp * heatwave, data = week12data)
  
 summary(week12anova)

#ONE-way ANOVAs - only treatment data 
  
  #Week 0
 t_week0data <- data %>%
    filter(weeks_since_heatwave == "0",
          heatwave == "1")
 
 t_week0anova <- aov(alive ~ adapted_temp, data = t_week0data)

 summary(t_week0anova)
 
  #Week 2
 t_week2data <- data %>%
   filter(weeks_since_heatwave == "2",
          heatwave == "0")
 
 t_week2anova <- aov(alive ~ adapted_temp, data = t_week2data)
 
 summary(t_week2anova)
 
  #Week 6
 t_week6data <- data %>%
   filter(weeks_since_heatwave == "6",
          heatwave == "0")
 
 t_week6anova <- aov(alive ~ adapted_temp, data = t_week6data)
 
 summary(t_week6anova)
 
  #Week 12
 t_week12data <- data %>%
   filter(weeks_since_heatwave == "12",
          heatwave == "0")
 
 t_week12anova <- aov(alive ~ adapted_temp, data = t_week12data)

  summary(t_week12anova)
 
  