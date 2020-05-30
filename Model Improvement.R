library(DAAG)
head(ais, n=10)

# packages 


library(e1071)
library(plyr)
library(ggplot2)
ais2 <- subset(ais, sex=="m") # only male athletes
ais3 = ais2[,c(3,4)] # subset column number that correspond to "hg" and "hc"
newdata <- rename(ais3, c("hg"="HEMAGLOBIN", "hc"="HEMATOCRIT"))
str(newdata) # new dataset now includes 102 observations


# MODEL IMPROVEMENT 


newdata1 <- setNames(cbind(rownames(newdata), newdata, row.names = NULL), 
                     c("OBS", "HEMAGLOBIN", "HEMATOCRIT"))
newdata1$OUTLIER = ifelse(newdata1$OBS %in% c(159,166,169),"Y","N") # create condition Yes/No if outlier

qplot(HEMATOCRIT, HEMAGLOBIN, data = newdata1, colour = OUTLIER,
      main = "HEMAGLOBIN and HEMATOCRIT relationship") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(30:65), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(10:25), minor_breaks = NULL)
  # Blue points which will be display they  represent as  three outliers which is identified.


  newdata2 <- subset(newdata1, OBS != 159 & OBS != 166 & OBS != 169,
                  select=c(HEMAGLOBIN, HEMATOCRIT))
HEMAGLOBIN_CENT = scale(newdata2$HEMAGLOBIN, center=TRUE, scale=FALSE) # center the variable


# new Model 

mod2 = lm(HEMATOCRIT ~ HEMAGLOBIN_CENT, data = newdata2)
summary(mod2)


# Diagnostic plots are summarized in the graph below:

par(mfrow = c(2,2)) # display a unique layout for all graphs
plot(mod2)

# AIC and BIC values of mod2 are lower than mod1 ones. Generally, small values correspond to models with a low test error, so it’ is’s the confirmation that mod2 fits better than mod1.

AIC(mod1)   
AIC(mod2)
BIC(mod1)
BIC(mod1)