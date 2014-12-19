library(rstan)
library(memisc)
library(dplyr)
library(ggplot2)

## Import Data
NATSAP <- as.data.set(spss.system.file("NewNATSAP.sav"))
NATSAP <- as.data.frame(NATSAP)
DoseData <- read.csv("NATSAPDoseData.csv")
DoseData <- DoseData[!is.na(DoseData$NatsapId),]

## Subset Data for OQ scores
## NATSAP_P <- NATSAP[, grep("NatsapId|total.*OQ|OQ.*total", names(NATSAP), ignore.case = T)]

## Calculate diff variable
whichDiff <- function(x1, x2){
  NATSAP_P[[x1]] - NATSAP_P[[x2]]
}


## Decide which variables to use for the Paired T-Test
YAdd <- "AdmissionTotalScore"
YDis <- "DischargeTotalScore"

NATSAP_P <- NATSAP[, c("NatsapId", "GenderNumeric", YAdd, YDis)]
NATSAP_P <- NATSAP_P[complete.cases(NATSAP_P),]
NATSAP_P$NatsapId <- as.integer(NATSAP_P$NatsapId)

Natsap_P_Ids <- select(NATSAP_P, NatsapId)
DoseData_Ids <- select(DoseData, NatsapId)
Lookup <- semi_join(DoseData_Ids, Natsap_P_Ids)
Lookup <- cbind(Lookup, newNatsapId = 1:length(Lookup$NatsapId))

NATSAP_P <- inner_join(NATSAP_P, Lookup, by = "NatsapId")
DoseData <- inner_join(DoseData, Lookup, by = "NatsapId")

nSubj <- nrow(NATSAP_P)
nProg <- nrow(DoseData)

diff <- whichDiff(YAdd, YDis)

IPred <- cbind(rep.int(1,nSubj),NATSAP_P[,"GenderNumeric"])
GPred <- cbind(rep.int(1,nProg),DoseData[,c("Mode.minutes.of.Inidividual.Therapy",
                                            "Mode.minutes.of.Group.Therapy")])
groupId = NATSAP_P[,"newNatsapId"]

## This is like 8 schools (we're using a different model, but keeping this in case.)
# ProgMeans <- NATSAP_P %>%
#   mutate(YOQDiff = AdmissionTotalScore - DischargeTotalScore) %>%
#   group_by(NatsapId) %>%
#   summarize(mean_YOQDiff = mean(YOQDiff), sd_YOQDiff = sd(YOQDiff),
#             n_YOQDiff = n(), se_YOQDiff = sd(YOQDiff)/sqrt(n()))
# 
# Dose_and_Prog <- left_join(DoseData, ProgMeans, by = "NatsapId")

#Put data in a list for stan

dataList <- list(nSubj = nSubj, nGroup = nProg, nIPred = NCOL(IPred),
                 nGPred = NCOL(GPred), diff = diff, groupId = groupId,
                 IPred = IPred, GPred = GPred)

#fitData <- c(dataList)
fit <- stan(file = "H_Lin_Reg.stan", data = dataList)

#---------------------------------------------------------------------
#Examine the Results
samplesSet <- extract(fit, pars =  c("beta", "gamma", "sigma"))
print(fit, digits_summary = 3)
traceplot(fit, pars = c("beta", "gamma", "sigma"))

# Extract parameter values and save them.

beta <- samplesSet$beta
gamma <- samplesSet$gamma
sigma <- samplesSet$sigma

hist(beta[,4,2])
hist(sigma)
