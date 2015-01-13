library(rstan)
library(memisc)
library(dplyr)
library(ggplot2)

##Import Data
NATSAP <- read.csv("NewNATSAP.csv")
DoseData <- read.csv("NATSAPDoseData.csv")
DoseData <- DoseData[!is.na(DoseData$NatsapId),]


##Calculate diff variable
whichDiff <- function(x1, x2){
  NATSAP_P[[x1]] - NATSAP_P[[x2]]
}


##Variables to store long names in the original data file
YAdd <- "AdmissionTotalScore"
YDis <- "DischargeTotalScore"

NATSAP_P <- NATSAP[, c("NatsapId", "GenderNumeric", YAdd, YDis)]
NATSAP_P <- NATSAP_P[complete.cases(NATSAP_P),]
NATSAP_P$NatsapId <- as.integer(NATSAP_P$NatsapId)

##Creates new program IDs incrementing from 1 for loops in the sampler
#Lookup is the intersection of NatsapIds from DoseData and NATSAP_P
Natsap_P_Ids <- select(NATSAP_P, NatsapId)
DoseData_Ids <- select(DoseData, NatsapId)
Lookup <- semi_join(DoseData_Ids, Natsap_P_Ids)
Lookup <- cbind(Lookup, newNatsapId = 1:length(Lookup$NatsapId))

#Selects only the cases in the dataframes that have NatsapIds in Lookup
#and adds a column including the new indices for the NatsapIds
NATSAP_P <- inner_join(NATSAP_P, Lookup, by = "NatsapId")
DoseData <- inner_join(DoseData, Lookup, by = "NatsapId")

##Defines Variables to be passed to the sampler
#IPred and GPred have a row of 1's representing a constant variable
nSubj <- nrow(NATSAP_P)
nProg <- nrow(DoseData)
diff <- whichDiff(YAdd, YDis)
IPred <- cbind(rep.int(1,nSubj),NATSAP_P[,"GenderNumeric"])
GPred <- cbind(DoseData[,c("Mode.minutes.of.Inidividual.Therapy",
                           "Mode.minutes.of.Group.Therapy")])
groupId = NATSAP_P[,"newNatsapId"]


#Put data in a list for stan
dataList <- list(nSubj = nSubj, nGroup = nProg, nIPred = NCOL(IPred),
                 nGPred = NCOL(GPred), diff = diff, groupId = groupId,
                 IPred = IPred, GPred = GPred)

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

hist(beta[,1,1]+beta[,1,2])
hist(beta[,,1]) #histogram for females
hist(beta[,,1]+beta[,,2]) #histogram for males
for(i in 1:nProg) #histograms for each program
  hist(beta[,i,])
hist(sigma) #histogram of the error
