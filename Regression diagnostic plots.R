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

# Residuals vs Fitted values

plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=1)

# Normal Q-Q

plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=2)


# Scale-location

plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=3)


# cook's distance 

plot(mod1, pch=16, col="blue", lty=1, lwd=2, which=4)



