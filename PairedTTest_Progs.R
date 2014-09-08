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

NATSAP_Y <- NATSAP_OQ[, c("NatsapId", YAdd, YDis)]
NATSAP_Y <- NATSAP_Y[complete.cases(NATSAP_Y),]

diff <- whichDiff(YAdd, YDis)
diff <- diff[!is.na(diff)]
nSubj <- length(diff)
nProg <- length(unique(NATSAP_Y$NatsapId))
Prog <- NATSAP_Y$NatsapId

#ProgId is a vector that replaces Program IDs with new IDs numbered 1-30
#So it will work in the loop in the sampler
ProgId <- as.factor(Prog)
levels(ProgId) <- 1:length(levels(ProgId))
ProgId <- as.numeric(ProgId)

#Put data in a list for stan

dataList <- list( 
  nProg = nProg ,
  nSubj = nSubj ,
  ProgId = ProgId,
  diff = diff)

#fitData <- c(dataList)
fit <- stan(file = "YouthOQ_AD_H.stan", data = dataList)

#---------------------------------------------------------------------
#Examine the Results
samplesSet = extract(fit, pars =  c("mu"))
print(fit, digits_summary = 3)
traceplot(fit, pars = c("mu"))

# Extract parameter values and save them.

mu = t(samplesSet$mu)

chainLength = NCOL(mu)

# Histograms of mu differences:
windows(19,10)
layout( matrix(1:10,nrow=2) )
source("plotPost.R")
for(i in 1:10){
plotPost( mu[i,] , xlab=paste("mu", i, sep="") , main="" ,
          breaks=20)
}
windows(19,10)
layout( matrix(1:10,nrow=2) )
source("plotPost.R")
for(i in 11:20){
  plotPost( mu[i,] , xlab=paste("mu", i, sep="") , main="" ,
            breaks=20)
}
windows(19,10)
layout( matrix(1:10,nrow=2) )
source("plotPost.R")
for(i in 21:30){
  plotPost( mu[i,] , xlab=paste("mu", i, sep="") , main="" ,
            breaks=20)
}
#dev.copy2eps(file=paste(fileNameRoot,"MuDiffs.eps",sep=""))

## Run T-Test
print(t.test(diff))
