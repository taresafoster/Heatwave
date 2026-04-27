#load the packages that I need for this code
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(ggbeeswarm)

#ggplot theme
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
data <-read.csv("./data/mortalitywk2.csv",stringsAsFactors = FALSE,
                strip.white = TRUE, na.strings = c("NA","") )

data$prop_mortality<-data$mortality/data$starting_population

data$adapted_temp<-as.factor(data$adapted_temp) #make adapted temp into a factor so we can color code by it in the plot

data2<-data %>% #create a new dataframe
  group_by(adapted_temp,population_id) %>% #specify that the groups are 
  mutate(cumulativemortality = cumsum(mortality))%>%
  mutate(cumpropmortality = cumulativemortality/starting_population)#add a new column that is now the cumulative mortality

is.factor(data$weeks_since_heatwave)

str(data2)

##MAIN PLOT

#calculate means and standard errors
summary_data<-data2%>%
  group_by(adapted_temp,weeks_since_heatwave) %>%
  summarise(mean = mean(cumpropmortality),
            n=n(),
            sd = sd(cumpropmortality),
            se = sd / sqrt(n))

# Convert weeks_since_heatwave to a factor with levels 0 and 2 only
summary_data$weeks_since_heatwave <- factor(summary_data$weeks_since_heatwave, levels = c(0, 2))
data2$weeks_since_heatwave <- factor(data2$weeks_since_heatwave, levels = c(0, 2))

#plot
p<-ggplot(summary_data, aes(x = weeks_since_heatwave, y = mean, color=adapted_temp)) +
  geom_point(size = 4,position = position_dodge(width = 0.6)) + #mean points
  geom_jitter(data = data2,
              aes(x = weeks_since_heatwave, y = cumpropmortality, color = adapted_temp),
              position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8), 
              size = 3, alpha = 0.5) +
  #geom_beeswarm(data = data2, aes(x = weeks_since_heatwave, y = cumpropmortality, color = adapted_temp),
   #             dodge.width = 0.6, size = 3, alpha = 0.5, cex = 2.7) +
  scale_color_manual(values = c("cornflowerblue", "darkorange", "brown3"), #values tells it what actual colors to put  on
                     name = "Adapted temp", #name in the legend
                     labels=c("25C", "30C", "35C"), #labels in the legend
                     breaks = c("25","30","35"), #the labels as they are written in the dataframe
                     guide = guide_legend(reverse=TRUE))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.6),
                width = 0) +
  xlab("Weeks since heatwave") +
  ylab("Proportion mortality")+
  scale_x_discrete(breaks = c("0", "2"), labels = c("0", "2")) + #(limits = c(-1, 3), #minimum and maximum 
                  #breaks = c(0,2),
                   #labels = c("0","2"))+ 
  theme_tess()

#windows();p 

#quartz()         # Opens a new plotting window
#plot(1:10)       # Your plot appears in that window
#print(p) #this opens the plot in a new window

ggsave(file="Figures/Taresapropmortalityfigure.pdf", p, width = 22, 
       height = 22, units = "cm") 

