#Omar Masood | PubPol577
#Task 4

library(dplyr)
library(tidyr)
load("prisondata.Rdata")

#Question 1
LEFTJOIN <- left_join(x=EMPLOY,y=GDPDATA, by=c('Year','Country'))
RIGHTJOIN <- right_join(x=EMPLOY,y=GDPDATA, by=c('Year','Country'))
FULLJOIN <- full_join(x=EMPLOY,y=GDPDATA, by=c('Year','Country'))
INNERJOIN <- inner_join(x=EMPLOY,y=GDPDATA, by=c('Year','Country'))

str(LEFTJOIN) # 480 obs.
str(RIGHTJOIN) #779 obs. 
str(FULLJOIN) #791 obs. 
str(INNERJOIN) #468 obs. 

#Question 2
str(MIENROLL)
str(MITEACHERS)
str(MIDEMO)

MISCHOOL <- full_join(x=MIENROLL,y=MITEACHERS,by='DistrictCode',copy=FALSE)
MISCHLALL <- full_join(x=MISCHOOL,y=MIDEMO, by = c('CtyCode.x'='CountyCode'))

#Question 3
PRISONWIDER <- PRISONLONG %>% pivot_wider(names_from = legal,
                                           values_from = count)

PRISONGATHER <- gather(data = PRISONWIDE,key="legal",value='value',2:33)
PRISONSEPARATE <- separate(data=PRISONGATHER,
                           col=legal,
                           into=c('state','gender','legal'),
                           sep=c(3,4))
PRISONLONGER <- PRISONSEPARATE %>% pivot_wider(names_from = legal,values_from = legal)

#Question 4
table(DANVILLE$owncell)
table(DANVILLE$haveline)
table(DANVILLE$danville)

table(DANVILLE$neighbor,DANVILLE$danville <= 3, useNA = "ifany")