#### Load these libraries to make the figure ####
library(plyr)
library(tidyverse)
library(reshape2)
library(svglite)
library(extrafont)
library(unpivotr)
library(ggpubr)
library(RColorBrewer)

setwd("Specify your working directory here")

#### Read your alignment file into R ####
Alignment <- read.csv("tRNA_alignment.csv", header = T, check.names = F)

#### Make sure that column 1 is labeled Position ####
colnames(Alignment)[1] = "Position"
colnames(Alignment)[1]

#### Remove Missing values from your dataframe ####
unique(Alignment$Position)
which(Alignment$Position =="")
Alignment[-which(Alignment$Position ==""),]
Alignment=Alignment[-which(Alignment$Position ==""),]

#### Make a new dataframe containing the information from the Position and name columns ####
Alignment_melt <- melt(Alignment, id = c("Position", "name"))

#### Name the columns in the new dataframe Position, Region, tRNA and nucleotide ####
colnames(Alignment_melt) <- c("Position", "Region", "tRNA", "nucleotide")

#### For nucleotides with NaN for their reactivity replace that with blank because there is no data there not a reactivity of 0 ####
Alignment_melt[, "reactivity"] <- NaN

#### Sort the dataframe by the tRNA column and make a new sorted dataframe ####
Alignment_melt_sort <- Alignment_melt[order(Alignment_melt$tRNA),]
head(Alignment_melt_sort)

#### Format the data so that you can make the heatmap ####
path <- getwd()
files <- list.files(path, pattern = "profile.txt$")
files
for (var in files)
{
  Reac <- read.table(var, header = T, sep = "")
  tRNA_name <- strsplit(var, "merged_")[[1]][2]
  tRNA_name <- strsplit(tRNA_name, "_profile")[[1]][1]
  print(tRNA_name)
  if ("Norm_profile" %in% names(Reac)) {
    Reac[is.na(Reac)] <- -999
  } else {
    Reac[, "Norm_profile"] <- 0
  }
  
  position_length <- 1:NROW(Reac)
  target_cells <- which(grepl(tRNA_name, (Alignment_melt_sort$tRNA)))
  
  for (pos in position_length)
  {
    Reactivity <- Reac$Norm_profile[pos]
    print(pos)
    print(Reactivity)
    
    for (target in target_cells)
    {
      if (Alignment_melt_sort$nucleotide[target] != "-") {
        Alignment_melt_sort$reactivity[target] <- Reactivity
        print(target)
        target_cells <- target_cells[-1]
        break
      } else {
        count_factor <- target
        while (Alignment_melt_sort$nucleotide[target] == "-") {
          target <- target + 1
        }
        count <- target - count_factor
        print(target)
        print(count)
        Alignment_melt_sort$reactivity[target] <- Reactivity
        target_cells <- target_cells[-1:-(count+1)]
        break
      }
    }
    print(target_cells)
  } 
}

#### Make a new dataframe where the DMS reactivity at each nucleotide is separated into bins based on how reactive each nucleotide was ####
Alignment_melt_sort2 <- mutate(Alignment_melt_sort, countfactor=cut(reactivity, breaks=c(min(reactivity, na.rm = T), 0.3, 0.6, 1, 2, 3, 10, max(reactivity, na.rm=T)),
                                                                    labels = c("<0.3", "0.6~0.3", "1~0.6", "2~1", "3~2", "10~3", ">10")))

#### Make annotations for native modifications and those nucleotides that show no DMS data ####
Alignment_melt_sort2$countfactor <- as.character(Alignment_melt_sort2$countfactor)
Alignment_melt_sort2$countfactor[Alignment_melt_sort2$reactivity == -999] <- "native modification"
Alignment_melt_sort2$countfactor[Alignment_melt_sort2$reactivity == 0] <- "no data"
Alignment_melt_sort2 <- transform(Alignment_melt_sort2, countfactor = factor(countfactor, levels = c("no data", "native modification", ">10", "10~3", "3~2", "2~1", "1~0.6", "0.6~0.3", "<0.3")))
Alignment_melt_sort2 <- filter(Alignment_melt_sort2, Position != 76)
head(Alignment_melt_sort2)


#### Define a custom heatmap palette, ignore if you'd rather use viridis etc ####
my_pal <- c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "blue", "grey90", "white")

#### Make the heatmap ####
NM_30mers_12_10_heatmap <- ggplot(Alignment_melt_sort2, aes(x = Position, y = tRNA, fill = countfactor))+
  geom_tile(color = "grey")+
  theme_minimal() + 
  theme(text = element_text(size = 20, family = "Arial", color = "black"))+
  theme(axis.text = element_text(color = "black"))+
  xlab("Position")+
  scale_y_discrete()+
  scale_x_discrete(position = "top", limit = c("1", "2","3", "4", "5", "6", "7", '8', '9', '10',
                                               '11', '12', '13', '14', '15','16', '17','18','19','20a','20b',
                                               '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',
                                               '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',
                                               '41', '42', '43', '44', '45','45a', '45b', '45c', '45d', '45e', '45f', '45g', '45h', '45i', '45j', '45k', '45l',
                                               '45m', '45n', '45o', '45p', '46', '47', '48', '49', '50',
                                               '51', '52', '53', '54', '55', '56', '57', '58', '59', '60', '61', '62', '63', '64', '65', '66', '67', '68',
                                               '69', '70', '71', '72', '73', '74', '75'))+
  scale_fill_manual(aesthetics = "fill", values=rev(my_pal), na.value="white", name = "Reactivity", limits = c("no nts", "no data", "native modification", ">10", "10~3", "3~2", "2~1", "1~0.6", "0.6~0.3", "<0.3"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  theme(legend.position="right", legend.direction="vertical")
NM_30mers_12_10_heatmap

#### Save your heatmap as a .svg ####
ggsave(filename = 'NM_cond_f_heatmap.svg', path = getwd(), plot = NM_30mers_12_10_heatmap, scale = 10, width = 5, height = 5, units = "cm", dpi = 300)