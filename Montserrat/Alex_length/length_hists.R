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

data_threshold = data[data$Species.ID %in% names(species[species > 30]), ]
  

#SA individual lengths data (generated with script 'extract_lenghts_SA.R')

SA_lengths <- read.csv('SA_individ_lengths.csv')


# Plot hists for length data
library(ggplot2)

length_h1 = ggplot(data = data_threshold, aes(x = Length..cm.)) +
              geom_histogram(binwidth = 1) +
              theme_minimal()

length_h2 = length_h1 + facet_wrap(~Species.ID, scales = "free") +
  geom_text(aes(x=Inf,y=Inf, label=paste("n =", length(data_threshold$Length..cm.)))) 
    
    


# Plot hists for SA data
library(ggplot2)

SA_h1 = ggplot(data = SA_lengths, aes(x = length)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

SA_h2 = SA_h1 + facet_wrap(~Species.ID, scales = "free") 
  #geom_text(aes(x=Inf,y=Inf, label=paste("n =", length(data_threshold$Length..cm.)))) 



    
    #data=n_df, aes(x=30,y=25, label=paste("n =", n), hjust=-.1))
              
#aes(x=Inf,y=Inf, label=paste("n =", length(data_threshold$Length..cm.)))) +


#n_df = data.frame(n=species[species > 30])