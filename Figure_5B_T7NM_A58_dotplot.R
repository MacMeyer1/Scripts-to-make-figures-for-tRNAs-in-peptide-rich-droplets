####Load these libraries to make the figure####
library(tidyverse)
library(ggpubr)
library(extrafont)

setwd("Specify your working directory here")

####Read the csv containing your data into R ####
A58.data <- read.csv("eco_T7NM_A58.csv", header = F, na.strings = c(""), skip = 1)
colnames(A58.data) <- c("Sample", "A58R", "Condition", "A", "D", "F")

#### For columns with 0 in the data, replace it with NaN ####
A58.data[A58.data == 0] <- NaN
A58.data

####Make Vectors from the columns #####
Condition <- A58.data$Condition
A58 <- A58.data$A58R
A <- A58.data$A
D <- A58.data$D
F <- A58.data$F

#### Tell R what order to put the data in, in the plot ####
A58.data$Condition=factor(A58.data$Condition, 
                          levels = c("T7 10mers / 1:1 / 0.5 (A)", "NM 10mers / 1:1 / 0.5 (A)", "T7 10mers / 1:2 / 10 (D)", "NM 10mers / 1:2 / 10 (D)", "T7 30mers / 1:2 / 10 (F)", "NM 30mers / 1:2 / 10 (F)"))


#### Make the plot ####
A58_dotplot <-ggplot(mapping = aes(x = Condition, y = A58, fill(Condition)), data = A58.data)+
  geom_dotplot(aes(fill = Condition), binaxis = "y", binwidth = 0.075, stackdir = "center")+
  geom_line(aes(group = A), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = D), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  geom_line(aes(group = F), alpha = 0.5, lineend = "round", linejoin = "round", color = "grey")+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black")+
  scale_y_continuous(trans = "log10", limits = c(0.1,100))+
  theme_classic()+
  theme(text = element_text(size = 45, family = "Arial", color = "black"))+
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  ylab("Normalized A58 Reactivity")
A58_dotplot

#### Save the plot as a .svg ####
ggsave("Eco_T7NM_A58_dotplot.svg", width = 22.5, height = 10.5,  dpi = 600)