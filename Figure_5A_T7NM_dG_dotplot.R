####Load these libraries to make the figure####
library(tidyverse)
library(ggpubr)
library(extrafont)

setwd("Specify your working directory here")

####Read the csv containing your data into R ####
dG.data <- read.csv("Eco_T7NM_dG.csv", header = F, na.strings = c(""), skip = 1)
colnames(dG.data) <- c("Sample", "dG", "Condition", "A", "D", "F")

####Make Vectors from the columns #####
Condition <- dG.data$Condition
dG <- dG.data$dG
A <- dG.data$A
D <- dG.data$D
F <- dG.data$F

#### Tell R what order to put the data in, in the plot ####
dG.data$Condition=factor(dG.data$Condition, 
                                levels = c("T7 10mers / 1:1 / 0.5 (A)", "NM 10mers / 1:1 / 0.5 (A)", "T7 10mers / 1:2 / 10 (D)", "NM 10mers / 1:2 / 10 (D)", "T7 30mers / 1:2 / 10 (F)", "NM 30mers / 1:2 / 10 (F)"))
#### Make the plot ####
dG_plot <-ggplot(mapping = aes(x = Condition, y = dG, fill(Condition)), data = dG.data)+
  geom_dotplot(aes(fill = Condition), binaxis = "y", binwidth = 1, stackdir = "center")+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black")+
  geom_line(aes(group = A), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = D), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = F), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  theme_classic()+
  theme(text = element_text(size = 45, family = "Arial", color = "black"))+
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  ylab("G")
dG_plot

#### Save the plot as a .svg ####
ggsave("Eco_T7NM_dG_dotplot.svg", width = 22.5, height = 10.5,  dpi = 600)