#### LBAR ####

### Read in life history parameter data and length frequency data######

lifehistory<-read.csv("Montserrat Life History Parms.csv")
length_data<-read.csv("Montserrat Species Length Composition Data_July.csv")
allspecies<-unique(lifehistory$Species)
allspecies2<-unique(length_data$Species.ID)
lifehistory[is.na(lifehistory)] <- 0

for i in 1:length(allspecies)
{
temp<-lifehistory %>%
  filter(lifehistory$Species==allspecies[i]) 

Linf<- temp$Value[temp$Parameter=="Linf"]
k<-temp$Value[temp$Parameter=="K"]
t0<-temp$Value[temp$Parameter=="t0"]
M<-temp$Value[temp$Parameter=="M"]

filter_length<-length_data%>%
  filter(length_data$Species.ID==allspecies2[i])
  lengths<-filter_length$Length..cm.
  length_freq<-data.frame(table(lengths))
  length<-length_freq$lengths
  length<-as.numeric(as.character(length))

## Solve for Lc ##
# Find the index of the mode
x <- seq(along=length_freq$Freq)[length_freq$Freq==max(length_freq$Freq)]
#Find the value associated with the index above
Lc <- length_freq[x, 1]
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

## Export results in table ##
# Create blank dataframe and then fill with appropriate results
results <- data.frame(matrix(0, nrow=length(allspecies)),Species=NA, LBAR=NA, Z=NA, M=NA, F=NA)
results$LBAR <- LBAR
results$Z <- Z
results$M <- M
results$F <- F
results$Species<-allspecies[i]
# Print table into a csv
write.table(results, file="results_baseline.csv")


## Print histogram ##
H <- unlist(lapply(seq_along(lengths$frequency), 
       function(x)rep(lengths[x,1], lengths[x,2])))
hist(H, xlab="Length", ylab="Frequency", main="Length Distribution")

}
