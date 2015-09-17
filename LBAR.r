#### LBAR ####

library(dplyr)

### Read in life history parameter data and length frequency data######


#lifehistory<-read.csv("Montserrat Life History Parms1.csv")
lifehistory<-read.csv("Montserrat Life History Parms2.csv")
lifehistory<- lifehistory[order(lifehistory$Species),] 
length_data<-read.csv("Montserrat Species Length Composition Data_July2.csv")
length_data<- length_data[order(length_data$Species.ID),] 
allspecies<-unique(lifehistory$Species)


allspecies2<-unique(length_data$Species.ID)
commonname<-unique(length_data$Species.common.name)
commonname<-sort(commonname)


lifehistory[is.na(lifehistory)] <- 0
results <- data.frame(matrix(0, nrow=length(allspecies)),Species=NA, Linf=NA, k=NA, M=NA,LBAR=NA, Lc=NA,Z=NA, F=NA, n=NA, Fcurr=NA)
i=12
for (i in 1:length(allspecies))
{
temp<-lifehistory %>%
  filter(lifehistory$Species==allspecies[i]) 

Linf<- temp$Value[temp$Parameter=="Linf"]
Linf<-as.numeric(Linf)
k<-temp$Value[temp$Parameter=="K"]
k<-as.numeric(k)
t0<-temp$Value[temp$Parameter=="t0"]
t0<-as.numeric(t0)
M<-temp$Value[temp$Parameter=="M"]
M<-as.numeric(M)

filter_length<-length_data%>%
  filter(length_data$Species.ID==allspecies2[i])
  lengths<-filter_length$Length..cm.
  length_freq<-data.frame(table(lengths))
  length<-length_freq$lengths
  length<-as.numeric(as.character(length))

## Solve for Lc ##
# Find the index of the mode
x <- seq(along=length_freq$Freq)[length_freq$Freq==max(length_freq$Freq)]
x<-x[1]
#Find the value associated with the index above
Lc<-length_freq$lengths[1]
#Lc <- length_freq[x, 1]
Lc <-as.numeric(as.character(Lc))
## Find lengths that are between Lc and Linf ##
# Set the boundaries
mn <- Lc
mn<-as.numeric(as.character(mn))
mx <- Linf
mx<-as.numeric(as.character(mx))
# Function that states T/F if value is between boundaries
is.between <- function(x, mn, mx) {
x >= mn & x <= mx
}
# Run is.between function using lengths, Lc, and Linf as variables and store in temporary variable
temp1 <- is.between(length, mn, mx)
# Find indices of values that are T from above function
temp2 <- which(temp1, arr.ind=TRUE)
# Find the length value from the indices above
l_between <- length_freq[temp2,1]
l_between <-as.numeric(as.character(l_between))

## Write the frequency of each of the above lengths ##
freq2 <- length_freq[temp2,2]



## Calculate the proportion of each length between Lc and Linf ##
#prop <- l_between/freq2
prop <- freq2/sum(freq2)

## LBAR = sum of product of lengths between Lc and Linf divided by sum of Length proportions ##
LBAR <- sum(l_between*prop)/sum(prop)

##Solve for Z (total mortality) and F (fishing mortality) ##
Z <- k*(Linf-LBAR)/(LBAR-Lc)
F <- Z-M
Fcurr<-F/M

## Export results in table ##
# Create blank dataframe and then fill with appropriate results
results$Linf[i]<-Linf
results$k[i]<-k
results$M [i] <- M
results$LBAR [i] <- LBAR
results$Lc[i]<-Lc
results$Z [i]<- Z
results$F [i]<- F
results$Species[i]<-as.character(allspecies[i])
results$n[i]<-length(lengths)
results$Fcurr[i]<-Fcurr
# Print table into a csv

## Print histogram ##

n<-length(lengths)


species<-unique(lifehistory$Sci.Name)
sp<-species[i]
sp<-as.character(sp)
hist(lengths, xlab="Length", ylab="Frequency", main=substitute(expr=paste(italic(sp)),env=list(sp=sp)),freq=TRUE)
mtext(paste("n =",n),side=3)
print(i)
i=i+1
}

#write.csv(results, file="final_results_lc.csv")

final<-read.csv("final_results_lc.csv")
final<-final[,-1]
final_results_lc<-rbind(final,results)
final_results_lc<- final_results_lc[order(final_results_lc$Species),] 

write.csv(final_results_lc,"final_results_lc.csv")




#write.csv(results, file="final_results.csv")

# final<-read.csv("final_results.csv")
# final<-final[,-1]
# final_results<-rbind(final,results)
# final_results<- final_results[order(final_results$Species),] 
# 
# write.csv(final_results,"final_results.csv")

