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


# Graphical Analysis 


# 1.Box plot 

par(mfrow=c(1, 2))  # it divides graph area in two parts

boxplot(newdata$HEMAGLOBIN, col = "yellow", border="blue",
        main = "HEMAGLOBIN boxplot",
        ylab = "g per decaliter")

boxplot(newdata$HEMATOCRIT, col = "orange", border="blue",
        main = "HEMATROCRIT boxplot",
        ylab = "percent values")
boxplot.stats(newdata$HEMAGLOBIN)$out # HEMAGLOBIN outliers
boxplot.stats(newdata$HEMATOCRIT)$out #HEMATOCRIT outliers

# 2.Histogram 

# Histogram for HEMAGLOBIN

qplot(HEMAGLOBIN, data = newdata, geom="histogram", binwidth=0.5, 
      fill=I("azure4"), col=I("azure3")) +
  labs(title = "HEMAGLOBIN") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x ="Concentration (in g per decaliter)") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(10:25), minor_breaks = NULL) +
  geom_vline(xintercept = mean(newdata$HEMAGLOBIN), show_guide=TRUE, color
             ="red", labels="Average") +
  geom_vline(xintercept = median(newdata$HEMAGLOBIN), show_guide=TRUE, color
             ="blue", labels="Median")


  # Histogram for HEMATOCRIT

qplot(HEMATOCRIT, data = newdata, geom="histogram", binwidth=1, 
      fill=I("azure4"), col=I("azure3")) +
  labs(title = "HEMATOCRIT") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x ="percent values") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(30:65), minor_breaks = NULL) +
  geom_vline(xintercept = mean(newdata$HEMATOCRIT), show_guide=TRUE, color
             ="red", labels="Average") +
  geom_vline(xintercept = median(newdata$HEMATOCRIT), show_guide=TRUE, color
             ="blue", labels="Median")

  

  # 3. Density plot 


  par(mfrow=c(1, 2))     # it divides graph area in two parts

plot(density(newdata$HEMAGLOBIN), main="Density: HEMAGLOBIN", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(newdata$HEMAGLOBIN), 2)))
     polygon(density(newdata$HEMAGLOBIN), col="yellow")

plot(density(newdata$HEMATOCRIT), main="Density: HEMATOCRIT", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(newdata$HEMATOCRIT), 2)))
     polygon(density(newdata$HEMATOCRIT), col="orange")




  # 4.  scatter plot
  

  qplot(HEMAGLOBIN, HEMATOCRIT, data = newdata,
      main = "HEMAGLOBIN and HEMATOCRIT relationship") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(colour = "blue", size = 1.5) +
      scale_y_continuous(breaks = c(30:65), minor_breaks = NULL) + scale_x_continuous(breaks = c(10:25), minor_breaks = NULL)




