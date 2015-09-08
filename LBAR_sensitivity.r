#### LBAR ####

# Read in parameter data
data <- read.csv("input.csv", header=T)

# Rename variables
input_Linf <- data$Linf
input_Lc <- data$Lc
input_k <- data$k
input_M <- data$M

# Create blank dataframe
results <- data.frame(Linf=NA, Lc=NA, LBAR=NA, Z=NA, M=NA, F=NA)

for(i in 1:length(input_Linf)){
# Name Linf and Lc
Linf <- input_Linf[i]
Lc <- input_Lc[i]
k <- input_k[i]
M <- input_M[i]


# Read in Length Frequency Data (ONLY USE EITHER OPTION 1 OR OPTION 2, NOT BOTH) ##
lengths <- read.csv("test3.csv", header=T)
len <- lengths$length
freq1 <- lengths$frequency


## Solve for Lc ##
# Find the index of the mode
x <- seq(along=freq1)[freq1==max(freq1)]
#Find the value associated with the index above

## Find lengths that are between Lc and Linf ##
# Set the boundaries
mn <- Lc
mx <- Linf
# Function that states T/F if value is between boundaries
is.between <- function(x, mn, mx) {
x >= mn & x <= mx
}
# Run is.between function using lengths, Lc, and Linf as variables and store in temporary variable
temp1 <- is.between(len, mn, mx)
# Find indices of values that are T from above function
temp2 <- which(temp1, arr.ind=TRUE)
# Find the length value from the indices above
l_between <- lengths[temp2,1]


## Write the frequency of each of the above lengths ##
freq2 <- lengths[temp2,2]



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
results[i,1] <- Linf
results[i,2] <- Lc
results[i,3] <- LBAR
results[i,4] <- Z
results[i,5] <- M
results[i,6] <- F

}

# Print table into a csv
write.table(results, file="results_sensitivity.csv")


## Print histogram ##
H <- unlist(lapply(seq_along(lengths$frequency), 
       function(x)rep(lengths[x,1], lengths[x,2])))
hist(H, xlab="Length", ylab="Frequency", main="Length Distribution")


