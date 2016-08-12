#Some t -tests to compare mean length in captured fish with Scientific Assessment (SA) length
#

setwd('./Montserrat/Alex_length')

capture.data <- read.csv("Montserrat_Species_Length_Composition_Data_July_2016_AS.csv")

capture.data$caught = rep(1, length(nrow(capture.data)))

SA.data <- read.csv('SA_individ_lengths.csv')

SA.data$caught = rep(0, length(nrow(SA.data)))

all.data = rbind(capture.data[, c(15, 9, 10, 12) ], SA.data[, c(12, 11, 5, 3)])


interaction.plot(all.data$caught, all.data$Species.ID, all.data$Length)


allspecies <- as.character(unique(SA.data$Species.ID))

results <- data.frame(matrix(vector(), 0, 10,
                             dimnames=list(c(), c("statistic","p.value","mean.uncaught","mean.caught","conf.int1", "conf.int2", "nobs","dof","alternative","null.value"))),
                      stringsAsFactors=F)

row.names(results) <- allspecies

for (i in 1:length(allspecies)) {

  x <- all.data[all.data$Species.ID == allspecies[i] ,]
  

  y <- rep(NA,10)
  
  y[7] <- nrow(x)[1]              # count observations
  #if(nrow(x) < 2) return(y)       # exits if too less observations
  res <- t.test(x[,4]~x[,1])  # doing the test
  
  y[1] <- res$statistic           # extract values of interest
  y[2] <- res$p.value      
  y[3] <- res$estimate[1]
  y[4] <- res$estimate[2]
  y[5] <- res$conf.int[1]  
  y[6] <- res$conf.int[2]  
  y[8] <- res$parameter    
  y[9] <- res$alternative  
  y[10] <- res$null.value 
  
print(res$estimate)
  results[i,] = y


}




#Two-way ANOVA

ANOVA1 <- aov(length ~ group*Species.ID, data = all.data)

tukey1 <- TukeyHSD(ANOVA1)
