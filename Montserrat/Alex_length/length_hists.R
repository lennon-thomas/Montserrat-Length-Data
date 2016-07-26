setwd('./Montserrat/Alex_length')

library(dplyr)
library(readr)

#Data Summary Length Data

data <- read.csv("Montserrat_Species_Length_Composition_Data_July_2016_AS.csv")
names(data)
days <- length(unique(data$Date))
boats <- length(unique(data$Vessel.ID..Length))
area <- length(unique(data$Area.fished))
#unique(df[c("data$Date","data$Vessel.ID..Length")])
trips <- data.frame(data$Date,data$Vessel.ID..Length)
unique_trips <- unique(trips)
nrow(unique_trips)
nrow(trips)
gears<-unique(data$Gear.Type)
species_CN <- unique(lapply(data$Species.common.name, tolower))

# Exclude species with count less than 30 individuals from length data

species <- table(data$Species.ID)

# Create sample size labels for plot
n_df = data.frame(Species.ID=rownames(species[species > 30]), n=species[species > 30])
n_df$xpos = c(24, 21, 38, 26, 30, 30, 40, 36)
n_df$ypos = c(15, 20, 7.5, 10, 4, 6, 5, 20)

data_threshold = data[data$Species.ID %in% names(species[species > 30]), ]
  
#####
#SA individual lengths data (generated with script 'extract_lenghts_SA.R')
####
SA_lengths <- read.csv('SA_individ_lengths.csv')

SA_species <- table(SA_lengths$Species.ID)
SA_n_df = data.frame(Species.ID=rownames(SA_species), n = SA_species)
SA_n_df$xpos = c(0, 0, 0, 5, 25, 15, 10)
SA_n_df$ypos = c(15, 150, 3, 10, 20, 3, 6)

# Plot hists for all species n>30 from length data
library(ggplot2)

length_h1 = ggplot(data = data_threshold) +
              geom_histogram(aes(x = Length..cm.),binwidth = 1) +
              geom_text(data=n_df,aes(x=xpos,y=ypos, label=paste("n=", n)), hjust=0) +
              theme_minimal()

length_h2 = length_h1 + facet_wrap(~Species.ID, scales = "free") 
 


# Plot hists for SA data
library(ggplot2)

SA_h1 = ggplot(data = SA_lengths) +
  geom_histogram(aes(x = length),binwidth = 1) +
  geom_text(data=SA_n_df,aes(x=xpos,y=ypos, label=paste("n=", n.Freq)), hjust=0) +
  theme_minimal()

SA_h2 = SA_h1 + facet_wrap(~Species.ID, scales = "free") 
  




geom_text(aes(x=Inf,y=Inf, label=paste("n =", length(data_threshold$Length..cm.)))) 


#blue tang spaitial length dist

ACANCO_h1 = ggplot(data = data_threshold[data_threshold$Species.ID=="ACANCO",]) +
  geom_histogram(aes(x = Length..cm.),binwidth = 1) +
  #geom_text(data=n_df,aes(x=xpos,y=ypos, label=paste("n=", n)), hjust=0) +
  theme_minimal()

ACANCO_h2 = ACANCO_h1 + facet_wrap(~Area.fished, scales = "free") 


#doctorfish spaitial length dist

ACANCH_h1 = ggplot(data = data_threshold[data_threshold$Species.ID=="ACANCH",]) +
  geom_histogram(aes(x = Length..cm.),binwidth = 1) +
  #geom_text(data=n_df,aes(x=xpos,y=ypos, label=paste("n=", n)), hjust=0) +
  theme_minimal()

ACANCH_h2 = ACANCH_h1 + facet_wrap(~Area.fished, scales = "free") 
              
#aes(x=Inf,y=Inf, label=paste("n =", length(data_threshold$Length..cm.)))) +


SERRGU_h1 = ggplot(data = data_threshold[data_threshold$Species.ID=="SERRGU",]) +
  geom_histogram(aes(x = Length..cm.),binwidth = 1) +
  #geom_text(data=n_df,aes(x=xpos,y=ypos, label=paste("n=", n)), hjust=0) +
  theme_minimal()

SERRGU_h2 = SERRGU_h1 + facet_wrap(~Area.fished, scales = "free") 

