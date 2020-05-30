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
mod1 = lm(HEMATOCRIT ~ HEMAGLOBIN_CENT, data = newdata)
summary(mod1)

# t statistic  

modSummary <- summary(mod1)  # capture model summary as an object
modCoeff <- modSummary$coefficients  # model coefficients
beta.estimate <- modCoeff["HEMAGLOBIN_CENT", "Estimate"]  # get beta coefficient estimate
std.error <- modCoeff["HEMAGLOBIN_CENT", "Std. Error"]  # get standard error
t_value <- beta.estimate/std.error  # calculate t statistic
print(t_value)# print t-value 

# f statistic 

f_statistic <- mod1$fstatistic[1]  # calculate F statistic
f <- summary(mod1)$fstatistic  # parameters for model p-value calculation
print(f) # print F value

