library(arm)
library(rstan)
library(dplyr)
library(ggplot2)


#### THIS FILE IS SUPERCEDED BY THE RMD FILE ####

## Import Data
natsap <- read.csv("NewNATSAP.csv")
dose <- read.csv("NATSAPDoseData.csv")

## Get rid of program with no NatsapID
dose <- dose[!is.na(dose$NatsapId),]

## Select only wanted variables and create diff
natsap_tidy <- natsap %>%
    select(ID = NatsapId,
           sex = GenderNumeric, 
           admission_OQ = AdmissionTotalScore,
           discharge_OQ = DischargeTotalScore) %>%
    mutate(diff = admission_OQ - discharge_OQ)

natsap_tidy <- natsap_tidy[complete.cases(natsap_tidy),]

dose_tidy <- dose %>%
    select(rtc_vs_OBH = RTCvsOBH,
           ID = NatsapId,
           minutes_ind_therapy = Mode.minutes.of.Inidividual.Therapy,
           minutes_group_therapy = Mode.minutes.of.Group.Therapy)

## Creates new program IDs incrementing from 1 for loops in Stan
## lookup is the intersection of ID from dose_tidy and natsap_tidy
natsap_tidy_ID <- select(natsap_tidy, ID)
dose_tidy_ID <- select(dose_tidy, ID)
lookup <- semi_join(dose_tidy_ID, natsap_tidy_ID)
lookup <- cbind(lookup, new_ID = 1:length(lookup$ID))

## Selects only the cases in the dataframes that have IDs in Lookup
## and adds a column including the new indices for the NatsapIds
natsap_tidy <- inner_join(natsap_tidy, lookup, by = "ID")
dose_tidy <- inner_join(dose_tidy, lookup, by = "ID")

## Defines Variables to be passed to Stan
## IPred and GPred have a column of 1's representing the constant term
n_subj <- nrow(natsap_tidy)
n_prog <- nrow(dose_tidy)
sex <- select(natsap_tidy, sex)
ind_pred <- cbind(rep(1, n_subj), sex)
minutes_ind_therapy <- select(dose_tidy, minutes_ind_therapy)
minutes_group_therapy <- select(dose_tidy, minutes_group_therapy)
group_pred <- cbind(rep(1, n_prog), minutes_ind_therapy, minutes_group_therapy)
diff <- natsap_tidy$diff
ID = select(natsap_tidy, ID)


## Put data in a list for Stan
data_list <- list(n_subj = n_subj,
                  n_prog = n_prog,
                  n_ind_pred = ncol(ind_pred),
                  n_group_pred = ncol(group_pred),
                  diff = diff,
                  ID = ID,
                  ind_pred = ind_pred,
                  group_pred = group_pred)
