# Omar Masood
# Task 3: Variable Creation

library(dplyr)
library(tidyr)
load("PubPol477477Task3F22.Rdata")

dim(AIRBAGS)
names(AIRBAGS)
str(AIRBAGS)

AIRBAGS$carage <- AIRBAGS$yearacc - AIRBAGS$yearVeh

AIRBAGS <- AIRBAGS %>% mutate(
  seatbeltmand = case_when(yearVeh <= 1973 ~ 0,
                           yearVeh > 1973 ~ 1),
  airbagmand = case_when(yearVeh <= 1988 ~ 0,
                        yearVeh > 1988 ~ 1)
)

AIRBAGS <- separate(AIRBAGS, col=caseid, into=c("sample", "case","vehicleNum"),sep=":")
###

FUELECON <- FUELECON %>% mutate(
  comb = (cty*0.45) + (hwy*0.55)
)

FUELECON <- FUELECON %>% group_by(year,class) %>% mutate(
    classavghwy = mean(hwy),
    classavghwy = mean(cty)
)

FUELECON <- FUELECON %>%group_by(year) %>% mutate(
  combavg = mean(comb)
)
###

WAGES$L_LWAGE <- log(WAGES$LWAGE)

WAGES <- WAGES %>% mutate(
  TOTEARN = cumsum(WKS * LWAGE)
)

WAGES <- WAGES %>% mutate(
  WAGEDIFF = WAGES$LWAGE - lag(WAGES$LWAGE)
)

