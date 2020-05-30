library(DAAG)
head(ais, n=10)

# R Libraries 
library(e1071)
library(plyr)
library(ggplot2)

ais2 <- subset(ais, sex=="m") # only male athletes
ais3 = ais2[,c(3,4)] # subset column number that correspond to "hg" and "hc"
newdata <- rename(ais3, c("hg"="HEMAGLOBIN", "hc"="HEMATOCRIT"))
str(newdata) # new dataset now includes 102 observations


# Show the relationship creating a regression line
qplot(HEMAGLOBIN, HEMATOCRIT, data = newdata,
      main = "HEMAGLOBIN and HEMATOCRIT relationship") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method="lm", col="red", size=1) +
  geom_point(colour = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(30:65), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(10:25), minor_breaks = NULL)



set.seed(123) # setting seed to reproduce results of random sampling
HEMAGLOBIN_CENT = scale(newdata$HEMAGLOBIN, center=TRUE, scale=FALSE) # center the variable
# Show the relationship with new variable centered, creating a regression line
qplot(HEMAGLOBIN_CENT, HEMATOCRIT, data = newdata,
      main = "HEMAGLOBIN_CENT and HEMATOCRIT relationship") +
      theme(plot.title = element_text(hjust = 0.5)) +
      stat_smooth(method="lm", col="red", size=1) +
      geom_point(colour = "blue", size = 1.5) +
      scale_y_continuous(breaks = c(30:65), minor_breaks = NULL) +
      scale_x_continuous(breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4), minor_breaks = NULL)

