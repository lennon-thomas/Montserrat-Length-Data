data<-read.csv("Sept_2016/Data/speciesname.csv")
names(data)
data$scientific<-tolower(data$scientific)
data$scientific<-paste0(toupper(substr(data$scientific, 1, 1)), substr(data$scientific, 2, nchar(data$scientific)))
table<-read.csv("Sept_2016/Data/spcommon.csv")

all_sp_fam<-merge(data, table, by.x = "common", all.x = TRUE)

write.csv(all_sp_fam,"Sept_2016/Data/speciesname2.csv")
