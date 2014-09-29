library(rstan)
library(memisc)
library(dplyr)
library(ggplot2)

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

# Indicator variable for Mother_OQ missing data

NATSAP_OQ_MI <- NATSAP_OQ %>%
  mutate(MI = ifelse(is.na(MOTHEROQInitialYOQ20TotalScore - 
                             MOTHEROQDischargeOQ20TotalScore), 0, 1))

NATSAP_OQ_MI_Prog <- NATSAP_OQ_MI %>%
  group_by(NatsapId) %>%
  summarize(mean(MI))

ggplot(NATSAP_OQ_MI, aes(x = YOUTHOQAdmissionTotalScore - YOUTHOQDischargeTotalScore)) +
  geom_histogram() + facet_grid(MI ~ .) 
ggplot(NATSAP_OQ_MI, aes(x = YOUTHOQDischargeTotalScore, fill = MI)) +
  geom_histogram() + facet_grid(MI ~ .) 