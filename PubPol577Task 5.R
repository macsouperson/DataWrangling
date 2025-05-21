##Omar Masood | PubPol 577 | Task 5

library(dplyr)
library(haven)
library(estimatr)

load("PubPol477577Task5W23.RData")

#Q1a
cor(OECD$longir,OECD$unemp, use="complete.obs")
cor(OECD$longir,OECD$gdpdef, use="complete.obs")
cor(OECD$curacct,OECD$labprod, use="complete.obs")

#Q2b
cor(subset(OECD,select=-c(cty)))

#Q3
SUBSET2003 <- subset(OECD, year == 2003)
SUBSET2009 <- subset(OECD, year == 2009)
SUBSET2016 <- subset(OECD, year == 2016)

t.test(SUBSET2003$unemp, SUBSET2009$unemp)
t.test(SUBSET2009$unemp, SUBSET2016$unemp)

#Q4
chisq.test(DANVILLE$daysafe,DANVILLE$nitesafe)
chisq.test(DANVILLE$danville,DANVILLE$nitesafe)
chisq.test(DANVILLE$owncell,DANVILLE$haveline)
table(is.na(DANVILLE$owncell), is.na(DANVILLE$haveline))

#Q5
ELECTRIC <- read_dta("ReplicationDataedit.dta")
summary(ELECTRIC)

#Q6
density <- lm(ahu_rural_w99 ~ wb_popdensityK, data = ELECTRIC)
urban <- lm(ahu_rural_w99 ~ pop_urban_rate, data = ELECTRIC)
hydro <- lm(ahu_rural_w99 ~ loghydropowercapitaK, data = ELECTRIC)
oil <- lm(ahu_rural_w99 ~ rents_ff, data = ELECTRIC)
natural <- lm(ahu_rural_w99 ~ wb_natres_rents, data = ELECTRIC)
dem <- lm(ahu_rural_w99 ~ polity2, data = ELECTRIC)

pooled <- lm(ahu_rural_w99 ~ wb_popdensityK + pop_urban_rate + 
               loghydropowercapitaK + rents_ff +polity2, data = ELECTRIC,
             method = "qr")
summary(pooled)

rbstpool <- lm_robust(ahu_rural_w99 ~ wb_popdensityK + pop_urban_rate + 
               loghydropowercapitaK + rents_ff +polity2, data = ELECTRIC)

#Q7
DIET <- read_dta("TPPedit.dta")
summary(DIET)

#Q8a
vote <- glm(signed ~ b_per_farm + b_GPP + b_GAGG + b_PAG + b_col +
              b_percap + b_unemp + b_nen + DPJ + new + SMD + b_farmxSMD, 
            data = DIET, family = binomial)
summary(vote)

#Q8b
voteprbt <- glm(signed ~ b_per_farm + b_GPP + b_GAGG + b_PAG + b_col +
              b_percap + b_unemp + b_nen + DPJ + new + SMD + b_farmxSMD, 
            data = DIET, family = binomial(link = probit))
summary(voteprbt)

#Q8c
votelm <- lm_robust(signed ~ b_per_farm + b_GPP + b_GAGG + b_PAG + b_col +
              b_percap + b_unemp + b_nen + DPJ + new + SMD + b_farmxSMD, 
            data = DIET)
summary(votelm)
