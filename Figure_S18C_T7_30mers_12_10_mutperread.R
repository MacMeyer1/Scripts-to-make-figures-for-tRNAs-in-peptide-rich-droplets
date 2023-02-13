####Load these libraries to make the figure####
library(viridis)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(extrafont)
library(scales)
library(ggridges)

setwd("Specify your working directory here")

####Read the csv file into R to make your histogram####
data <- read.csv("T7_30mers_12_10_mutperread.csv", header = TRUE)

####Making your histogram####
Figure_S18C <- ggplot(data, aes(x=Mutation, y=Normalized, fill=Sample))+
  geom_bar(stat = "identity",
           position = "dodge")+
  geom_line(aes(x=Mutation, y=Cumulative, col=Sample), size=1.5)+
  theme_classic()+
  theme(text = element_text(size = 30, family = "Arial", color = "black"))+
  xlab("Number of mutations/read")+
  ylab("Density")+
  theme(legend.position = c(0.85, 0.7))+
  theme(legend.title = element_blank())+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6))
Figure_S18C

###Save your histogram as a png###
ggsave(filename = 'Figure_S18C_T7_30mers_12_10_mutperread.png', path = getwd(), plot = Figure_S18C, scale = 2.5, width = 7.5, height = 5, units = "cm", dpi = 300)

