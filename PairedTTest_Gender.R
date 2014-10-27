library(rstan)
library(memisc)
library(dplyr)
library(ggplot2)

## Import Data
NATSAP <- as.data.set(spss.system.file("NewNATSAP.sav"))
NATSAP <- as.data.frame(NATSAP)

## Subset Data for OQ scores
## NATSAP_OQ <- NATSAP[, grep("NatsapId|total.*OQ|OQ.*total", names(NATSAP), ignore.case = T)]

## Calculate diff variable
whichDiff <- function(x1, x2){
  NATSAP_G[x1] - NATSAP_G[x2]
}


## Decide which variables to use for the Paired T-Test
YAdd <- "AdmissionTotalScore"
YDis <- "DischargeTotalScore"
YPD6 <- "PD6TotalScore"
YPD12 <- "PD12TotalScore"
MAdd <- "MOTHEROQInitialYOQ20TotalScore"
MDis <- "MOTHEROQDischargeOQ20TotalScore"
MPD6 <- "MOTHEROQPD6YOQ20TotalScore"
MPD12 <- "MOTHEROQPD12YOQ20TotalScore"
FAdd <- "FATHEROQInitiaLOQ20TotalScore"
FDis <- "FATHEROQDischargeYOQ20TotalScore"
FPD6 <- "FATHEROQPD6YOQ20TotalScore"
FPD12 <- "FATHEROQPD12OQ20TotalScore"

NATSAP_G <- NATSAP[, c("GenderNumeric", YAdd, YDis)]
NATSAP_G <- NATSAP[complete.cases(NATSAP_G),]

diff <- whichDiff(YAdd, YDis)
diff <- diff[!is.na(diff)]
nSubj <- length(diff)
nGender <- length(unique(NATSAP_G$GenderNumeric))
Gender <- NATSAP_G$GenderNumeric

#GenderId is a vector that replaces Gender IDs with new IDs numbered 1-2
#So it will work in the loop in the sampler
GenderId <- as.factor(Gender)
levels(GenderId) <- 1:length(levels(GenderId))
GenderId <- as.numeric(GenderId)

#Put data in a list for stan

dataList <- list( 
  nGender = nGender ,
  nSubj = nSubj ,
  GenderId = GenderId,
  diff = diff)

#fitData <- c(dataList)
fit <- stan(file = "YouthOQ_AD_Gender.stan", data = dataList)

#---------------------------------------------------------------------
#Examine the Results
samplesSet = extract(fit, pars =  c("mu"))
print(fit, digits_summary = 3)
traceplot(fit, pars = c("mu"))

# Extract parameter values and save them.

mu = t(samplesSet$mu)

chainLength = NCOL(mu)

# Histograms of mu differences:
source("plotPost.R")
pdf("GenderDiffs.pdf")
layout( matrix(1:2, nrow=2, byrow = TRUE) )
for(i in 1:2){
  plotPost( mu[i,] , xlab=paste("mu", i, sep="") , main="" ,
            breaks=20, xlim = range(20:45))
}

source("plotPost.R")
pdf("GenderDiff.pdf")
plotPost( mu[2,]-mu[1,] , xlab=expression(mu[female]-mu[male]) , main="" ,
            breaks=20, compVal = 0)
graphics.off()


sampleSet <- as.data.frame(samplesSet)
#ggplot(sampleSet, aes(mu.1,mu.2)) +
#  geom_histogram() + facet_grid(mu.1 ~ mu.2)

## Run T-Test
print(t.test(diff ~ GenderNumeric, data = NATSAP_G))
