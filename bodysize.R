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
data <-read.csv("preheatwavebodysize.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA",""))

data$adapted_temp<-as.factor(data$adapted_temp) #make adapted temp into a factor so we can color code by it in the plot


##MAIN PLOT

# Then calculate summary stats only on the filtered data
summary_data <- data %>%
  group_by(adapted_temp, sex) %>%
  summarise(mean = mean(weight),
            n=n(),
            sd = sd(weight),
            se = sd / sqrt(n))

summary_data$group_id <- interaction(summary_data$adapted_temp, summary_data$sex)
data$group_id <- interaction(data$adapted_temp, data$sex)


p <- ggplot(summary_data, aes(x = adapted_temp, y = mean, color = sex)) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  scale_x_discrete(expand = c(0.9, 2)) +
  geom_jitter(data = data,
              aes(x = adapted_temp, y = weight, color = sex),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.4), 
              size = 3, alpha = 0.5) +
  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4),
                width = 0) +
  
  scale_color_manual(
    values = c("f" = "palevioletred", "m" = "steelblue3"),   # or whatever colors you like
    labels = c("f" = "Female", "m" = "Male"),
    name = "Sex"
  ) +
  
  xlab("Adapted temperature") +
  ylab("Body size (g)") +
  scale_x_discrete (breaks = c("25", "30", "35"), labels = c("25°C", "30°C", "35°C")) 

print(p) #this opens the plot in a new window
ggsave(file="Figures/Taresabodysizefigure.pdf", p, width = 22, height = 22, units = "cm") #close the file before you save

