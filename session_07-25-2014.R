library(memisc)
library(rstan)
library(dplyr)

##  Import data

NATSAP <- as.data.set(spss.system.file('NATSAP_PRN_DATABASE.sav'))
NATSAP <- as.data.frame(NATSAP)

##  Subset data for OQ total scores.

NATSAP_OQ <- NATSAP[, grep("OQ.*total|total.*OQ", names(NATSAP), ignore.case = T)]

##  Calculuate difference between YOUTHOQ Admission Total Score and 
##  YOUTHOQ Discharge Total Score

diff <-  NATSAP_OQ$YOUTHOQAdmissionTotalScore - NATSAP_OQ$YOUTHOQDischargeTotalScore

##  Remove NAs

diff <- diff[!is.na(diff)]
N <- length(diff)

##  Run basic t-test

print(t.test(diff))
truehist(diff)

##  Setup Stan data

fit_data <- c("N", "diff")

fit <- stan(file = "YouthOQ_AD.stan", data = fit_data)
print(fit)
plot(fit)
    