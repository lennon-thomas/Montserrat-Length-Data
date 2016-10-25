#### ControlFile ###

AvailableData<- c("CatchData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBSPR','LBAR')
 
 Fish<- 'gar'
 
 Fish$SciName<- 'T. crocodilus'
 
 Fish$CommName<- 'Gar'
### Life History ###
 
 DefaultSD<- 
 
 Fish$LHITol<-  # The average % deviation from LHI allowed
 
 Fish$vbk<-                      ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<-
 
 Fish$Linf<-                        ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- 
 
 Fish$WeightA <-
 
 Fish$WeightB<- 
 
 
 Fish$MaxAge<- 
 
 
 Fish$M<-   
 
 Fish$MvK<-#Ratio of M versus K
 
 Fish$MaxAge<- #Max age is age at thich only 1% of population are left. Can also be set manually if needed
 
 Fish$MortalityError<-                 ## test error, can change to accomodate more error
 
 Fish$Mat50<-                   ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<-                             ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<-
 
 Fish$VBSD<- 
 
 
 Fish$res<- 'Low'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- NULL                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
