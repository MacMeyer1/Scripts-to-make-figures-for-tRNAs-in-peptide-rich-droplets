####Load these libraries to make the figure####
library(viridis)
library(tidyverse)
library(ggpmisc)

setwd("Specify your working directory here")

####Read  the csv files containing the data from the replicate experiments into R to make your correlation plots####
rep1_rep2 <- read.csv("NM_10mers_11_05_rep1_rep2.csv")
colnames(rep1_rep2) <- c("Rep1", "Rep2")
rep2_rep3 <- read.csv("NM_10mers_11_05_rep2_rep3.csv")
colnames(rep2_rep3) <- c("Rep2", "Rep3")
rep3_rep1 <- read.csv("NM_10mers_11_05_rep3_rep1.csv")
colnames(rep3_rep1) <- c("Rep3", "Rep1")

#### Plot the line x = y to make it easy to see the correlation ####
line.formula <- y ~ x

####Make the correlation plot of Rep1 vs Rep2 ####
NM_10mers_11_05_Rep1_vs_Rep2 <- ggplot(data = rep1_rep2, aes(x = Rep1, y = Rep2)) +
  geom_point()+
  scale_x_continuous(limits = c(-1, 20))+
  scale_y_continuous(limits = c(-1, 20))+
  geom_abline(intercept = 0, slope = 1)+
  geom_hex(bins = 50)+
  scale_fill_continuous(type = "viridis") +
  theme_classic(base_size = 30)+
  xlab("Rep1 DMS reactivity")+
  ylab("Rep2 DMS reactivity")+
  stat_poly_eq(formula = line.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 5)+
  theme(legend.key.size = unit(3, 'cm'),
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))
NM_10mers_11_05_Rep1_vs_Rep2

###Save the Rep1 vs Rep2 correlation plot as a .png####
ggsave(filename = 'NM_10mers_11_05_Rep1_vs_Rep2.png', path = getwd(), plot = NM_10mers_11_05_Rep1_vs_Rep2, scale = 2.5, width = 7.5, height = 5, units = "cm", dpi = 400)

####Make the correlation plot of Rep2 vs Rep3 ####
NM_10mers_11_05_Rep2_vs_Rep3 <- ggplot(data = rep2_rep3, aes(x = Rep2, y = Rep3)) +
  geom_point()+
  scale_x_continuous(limits = c(-1, 20))+
  scale_y_continuous(limits = c(-1, 20))+
  geom_abline(intercept = 0, slope = 1)+
  geom_hex(bins = 50)+
  scale_fill_continuous(type = "viridis") +
  theme_classic(base_size = 30)+
  xlab("Rep2 DMS reactivity")+
  ylab("Rep3 DMS reactivity")+
  stat_poly_eq(formula = line.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 5)+
  theme(legend.key.size = unit(3, 'cm'),
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))
NM_10mers_11_05_Rep2_vs_Rep3

###Save the Rep2 vs Rep3 correlation plot as a .png####
ggsave(filename = 'NM_10mers_11_05_Rep2_vs_Rep3.png', path = getwd(), plot = NM_10mers_11_05_Rep2_vs_Rep3, scale = 2.5, width = 7.5, height = 5, units = "cm", dpi = 400)

####Make the correlation plot of Rep3 vs Rep1 ####
NM_10mers_11_05_Rep3_vs_Rep1 <- ggplot(data = rep3_rep1, aes(x = Rep3, y = Rep1)) +
  geom_point()+
  scale_x_continuous(limits = c(-1, 20))+
  scale_y_continuous(limits = c(-1, 20))+
  geom_abline(intercept = 0, slope = 1)+
  geom_hex(bins = 50)+
  scale_fill_continuous(type = "viridis") +
  theme_classic(base_size = 30)+
  xlab("Rep3 DMS reactivity")+
  ylab("Rep1 DMS reactivity")+
  stat_poly_eq(formula = line.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 5)+
  theme(legend.key.size = unit(3, 'cm'),
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))
NM_10mers_11_05_Rep3_vs_Rep1

###Save the Rep3 vs Rep1 correlation plot as a .png####
ggsave(filename = 'NM_10mers_11_05_Rep3_vs_Rep1.png', path = getwd(), plot = NM_10mers_11_05_Rep3_vs_Rep1, scale = 2.5, width = 7.5, height = 5, units = "cm", dpi = 400)