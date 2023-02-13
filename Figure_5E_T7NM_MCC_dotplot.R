####Load these libraries to make the figure####
library(tidyverse)
library(ggpubr)
library(extrafont)

setwd("Specify your working directory here")

####Read the csv containing your data into R ####
EcoNatMod_data <- read.csv("Eco_T7NM_MCC_v2.csv", header = F, na.strings = c(""), skip = 1)
colnames(EcoNatMod_data) <- c("Sample", "MCC", "Condition", "A", "D", "F")

#### Tell R what order to put the data in, in the plot ####
EcoNatMod_data$Condition=factor(EcoNatMod_data$Condition, 
                                levels = c("T7 Condition a", "NM Condition a", "T7 Condition d", "NM Condition d", "T7 Condition f", "NM Condition f"))
####Make Vectors from the columns #####
A <- EcoNatMod_data$A
D <- EcoNatMod_data$D
F <- EcoNatMod_data$F


#### Make the plot ####
T7NM_dotplot <- ggplot(data = EcoNatMod_data, mapping = aes(x = Condition, y = MCC, fill = Condition))+
  geom_dotplot(aes(fill = Condition), binaxis = "y", binwidth = 2.3, stackdir = "center")+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black")+
  geom_line(aes(group = A), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = D), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = F), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  theme_classic()+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02", "#A6761D"))+
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02", "#A6761D"))+
  theme(text = element_text(size = 25, family = "Arial", color = "black"))+
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  xlab("Condition")+
  ylab("Prediction accuracy (%)")
T7NM_dotplot

#### Save the plot as a .svg ####
ggsave("T7NM_MCC_dotplot.svg", width = 15, height = 7,  dpi = 600)