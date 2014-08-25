library(rstan)
library(memisc)

## Import Data
NATSAP <- as.data.set(spss.system.file("NATSAP_PRN_DATABASE.sav"))
NATSAP <- as.data.frame(NATSAP)

## Subset Data for OQ scores
## NATSAP_OQ <- NATSAP[, grep("total.*OQ|OQ.*total", names(NATSAP), ignore.case = T)]

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
MDis <- "MOTHEROQDischarge20TotalScore"
MPD6 <- "MOTHEROQPD6YOQ20TotalScore"
MPD12 <- "MOTHEROQPD12YOQ20TotalScore"
FAdd <- "FATHEROQInitiaLOQ20TotalScore"
FDis <- "FATHEROQDischargeYOQ20TotalScore"
FPD6 <- "FATHEROQPD6YOQ20TotalScore"
FPD12 <- "FATHEROQPD12OQ20TotalScore"

diff <- whichDiff(YAdd, YDis)
diff <- diff[,1]
diff <- diff[!is.na(diff)]
N <- length(diff)

## Run T-Test
print(t.test(diff))

fitData <- c("N","diff")
fit <- stan(file = "YouthOQ_AD.stan", data = fitData)


