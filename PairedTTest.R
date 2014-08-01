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
diff <- whichDiff("YOUTHOQAdmissionTotalScore", "YOUTHOQDischargeTotalScore")
diff <- diff[,1]
diff <- diff[!is.na(diff)]
N <- length(diff)

## Run T-Test
print(t.test(diff))

fitData <- c("N","diff")
fit <- stan(file = "YouthOQ_AD.stan", data = fitData)

#Pull Request test
pull_test <- "test"