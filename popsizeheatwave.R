#set your working directory - this will be different on your computer
setwd("/Users/taresa/Documents/R stuff/TaresaHeatwave/Data")

#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)

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
data <-read.csv("heatwavepopulationwk12.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

data$heatwave <- factor(data$heatwave, levels = c(0, 1))

data$adapted_temp<-as.factor(data$adapted_temp) #make adapted temp into a factor so we can color code by it in the plot


##MAIN PLOT

#calculate means and standard errors
filtered_data <- data %>%
  filter(heatwave == "0", weeks_since_heatwave %in% c("0", "2", "6", "12"))

# Then calculate summary stats only on the filtered data
summary_data <- data %>%
  group_by(adapted_temp, heatwave, weeks_since_heatwave) %>%
  summarise(mean = mean(alive),
            n=n(),
            sd = sd(alive),
            se = sd / sqrt(n))

summary_data$heatwave <- factor(summary_data$heatwave, levels = c(0, 1))
summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$heatwave)
data$group_id <- interaction(data$adapted_temp, data$heatwave)

# Convert weeks_since_heatwave to a factor with levels 0 and 2 only
data$weeks_since_heatwave <- factor(data$weeks_since_heatwave, 
                                    levels = c( "0", "pad2", "2", "pad3", "6", "pad6", "12"))
summary_data$weeks_since_heatwave <- factor(summary_data$weeks_since_heatwave, 
                                            levels = c( "0", "pad2", "2", "pad3", "6", "pad6", "12"))

p <- ggplot(summary_data, aes(x = weeks_since_heatwave, y = mean, color = adapted_temp, shape = heatwave, group = group_id)) +
  geom_point(size = 4, position = position_dodge(width = 0.8)) +
  scale_x_discrete(expand = c(0.9, 2)) +
  geom_jitter(data = data,
              aes(x = weeks_since_heatwave, y = alive, color = adapted_temp, shape = heatwave, group = group_id),
              position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8), 
              size = 3, alpha = 0.5) +
  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.8),
                width = 0) +
  
  scale_shape_manual(values = c("0" = 16, "1" = 17),  
                     name = "Heatwave",
                     labels = c("0" = "Control", "1" = "Treatment")) +
  
  scale_color_manual(values = c("cornflowerblue", "darkorange", "brown3"),
                     name = "Adapted temperature",
                     labels = c("25°C", "30°C", "35°C"),
                     breaks = c("25", "30", "35"),
                     guide = guide_legend(reverse = TRUE)) +
  
  xlab("Weeks since heatwave") +
  ylab("Population size") +
  scale_x_discrete (breaks = c("0", "2", "6", "12"), labels = c("0", "2", "6", "12")) +
  theme_tess()
quartz()         # Opens a new plotting window
plot(1:10)       # Your plot appears in that window
print(p) #this opens the plot in a new window

ggsave(file="Figures/Taresapopusizefigure.pdf", p, width = 22, height = 22, units = "cm") #close the file before you save
