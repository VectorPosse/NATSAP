library(rstan)
library(memisc)

## Import Data
NATSAP <- as.data.set(spss.system.file("NATSAP_PRN_DATABASE.sav"))
NATSAP <- as.data.frame(NATSAP)

## Subset Data for OQ scores
NATSAP_OQ <- NATSAP[, grep("NatsapId|total.*OQ|OQ.*total", names(NATSAP), ignore.case = T)]

## Calculate diff variable
whichDiff <- function(x1, x2){
  NATSAP[x1] - NATSAP[x2]
}


## Decide which variables to use for the Paired T-Test
YAdd <- "YOUTHOQAdmissionTotalScore"
YDis <- "YOUTHOQDischargeTotalScore"
YPD6 <- "YOUTHOQPD6TotalScore"
YPD12 <- "YOUTHOQPD12TotalScore"
MAdd <- "MOTHEROQInitialYOQ20TotalScore"
MDis <- "MOTHEROQDischargeOQ20TotalScore"
MPD6 <- "MOTHEROQPD6YOQ20TotalScore"
MPD12 <- "MOTHEROQPD12YOQ20TotalScore"
FAdd <- "FATHEROQInitiaLOQ20TotalScore"
FDis <- "FATHEROQDischargeYOQ20TotalScore"
FPD6 <- "FATHEROQPD6YOQ20TotalScore"
FPD12 <- "FATHEROQPD12OQ20TotalScore"

#Youth Specific Test---------------------------------

diff <- whichDiff(YAdd, YDis)
diff <- diff[!is.na(diff)]
nSubj <- length(diff)

#Put data in a list for stan

dataList <- list( 
  nSubj = nSubj ,
  diff = diff)

#fitData <- c(dataList)
fit <- stan(file = "YouthOQ_AD.stan", data = dataList)

#---------------------------------------------------------------------
#Examine the Results
samplesSet = extract(fit, pars =  c("mu"))
print(fit, digits_summary = 3)
traceplot(fit, pars = c("mu"))

# Extract parameter values and save them.

mu = t(samplesSet$mu)

chainLength = NCOL(mu)

pdf("ProgDiffs.pdf")
# Histograms of mu differences:
pdf("TTestDiff.pdf")
windows(10,10)
layout( matrix(1:1,nrow=1) ) #This was originally matrix(1:3)
source("plotPost.R")
plotPost( mu , xlab=expression(mu[diff]) , main="" ,
          breaks=20)
dev.off()
#dev.copy2eps(file=paste(fileNameRoot,"MuDiffs.eps",sep=""))

## Run T-Test
print(t.test(diff))
