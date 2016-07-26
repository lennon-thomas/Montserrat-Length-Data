#to make spp code look up table

data <- read.csv("Montserrat_Species_Length_Composition_Data_July_2016_AS.csv",strip.white=TRUE) %>%
        mutate(common.name = unlist(lapply(data$Species.common.name, tolower)))

SA_data <- read.csv('Montserrat_fish_data_FINAL_SA.csv', strip.white=TRUE) 

length_list <- unique(data[c("common.name", "Species.ID")])

SA_list <-unique(SA_data[c("COMMON_NAME", "SPECIES_CODE")])


lookup_spp_code =  left_join(length_list, SA_list, by =c("common.name" = "COMMON_NAME")) 


write.csv(lookup_spp_code, file = 'lookup_spp_code.csv')
