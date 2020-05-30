library(DAAG)
head(ais, n=10)

# packages 

library(e1071)
library(plyr)
library(ggplot2)
ais2 <- subset(ais, sex=="m") # only male athletes
ais3 = ais2[,c(3,4)] # subset column number that correspond to "hg" and "hc"
newdata <- rename(ais3, c("hg"="HEMAGLOBIN", "hc"="HEMATOCRIT"))
str(newdata) 
# new dataset now includes 102 observations

colSums(is.na(newdata)) # report how many missing values are per column
summary(newdata) # overview of the two selected variables




