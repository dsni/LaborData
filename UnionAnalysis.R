
#####################################################
#####################################################
##
##   Environment Setup
##
#####################################################
#####################################################

library(car)          # regression tools, recoding
library(dplyr)        # data cleaning tools
library(ggplot2)      # plotting commands
library(reshape2)     # wide to tall reshaping
library(xtable)       # nice table formatting
library(knitr)        # kable table formatting
library(grid)         # units function for ggplot
library(foreign)      # gives input from STATA
library(lubridate)
library(quantmod)

saveDir <- getwd()  # get the current working directory
saveDir             # show me the saved directory

wd <- saveDir
setwd(wd)           # set this path as my work directory

#####################################################
#####################################################
##
##   Import Data
##
#####################################################
#####################################################

#Documentation available for extraction commands, from IPUMS-CPS

#census <- read.dta("/Users/jakecarlson/Desktop/UnionData62_14/Dataset/cps_00003.dta",
#                   convert.dates=TRUE, missing.type=TRUE
#                   )

#str(census)

#save(census,file="CPSCensus.Rda")

load("CPSCensus.Rda")

#####################################################
#####################################################
##
##   Data Cleaning
##
#####################################################
#####################################################

census$sex <- recode(census$sex, "9=NA")
census$race <- recode(census$race, "999=NA")
census$white <- recode(census$race, "'White'='White';
                       else='Non-white'")
census$citizen <- recode(census$citizen, "c('niu',9)=NA")
census$hispan <- recode(census$hispan, "c(901, 902)=NA")
census$classwkr <- recode(census$classwkr, "c('niu','Unpaid family worker', 99)=NA") 
census$classwkrR <- recode(census$classwkr, "c('Armed forces', 'Federal government employee', 'Local government employee', 'State government employee', 'Wage/salary, government')='Public Sector';
                           NA=NA;
                           else='Private Sector'")
census$inctot <- recode(census$inctot, "c('99999998', '99999999')=NA")  
census$incwage <- recode(census$incwage, "c('9999998', '9999999')=NA")
census$paidgh <- recode(census$paidgh, "'niu'=NA;
                        'No'='No';
                        NA=NA;
                        else='Yes'")
census$pension <- recode(census$pension, "'niu'=NA")
census$cleaners <- recode(census$OCC1990, "c('Housekeepers, maids, butlers, stewards, and lodging quarters cleaners', 
                          'Private household cleaners and servants', 'Private household workers allocated',
                          'Supervisors of cleaning and building service', 'Janitors', 'Laundry workers',
                          'Vehicle washers and equipment cleaners')='Cleaners';
                          NA=NA;
                          'Garbage and recyclable material collectors'='Sanitation workers';
                          c('Nursing aides, orderlies, and attendants', 'Other health and therapy', 'Registered nurses', 'Licensed practical nurses')='Healthcare workers';
                          else='Other'")


###CPI adjustment
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
#[1] "CPIAUCSL"

avg.cpi <- apply.yearly(CPIAUCSL, mean)

cf <- as.numeric(avg.cpi['2008'])/avg.cpi #using 2008 as the base year
cdf <- as.data.frame(cf)
cdf$LastDayOfYear <- rownames(cdf)
cdf$year <- sapply(cdf$LastDayOfYear, FUN=function(x) as.numeric(substr(x, 1, 4)))

#####################################################
#####################################################
##
##   Tabulations
##
#####################################################
#####################################################

###Median Income All
incAll <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0) %>%
  group_by(year) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE),
            count=n())

incAll2 <- merge(incAll, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incAll2 <- mutate(incAll2, 
                  medianIncAdj = medianInc*CPIAUCSL)
##Plots
plotIncAll <- ggplot(data=incAll2, aes(x=year, y=medianIncAdj)) +
  geom_line()+
  ggtitle("Median Income") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())

############
###Median Income All
incWK <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0, labforce=="Yes, in the labor force") %>%
  group_by(year) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE),
            count=n())

incWK2 <- merge(incWK, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incWK2 <- mutate(incWK2, 
                  medianIncAdj = medianInc*CPIAUCSL)
##Plots
plotIncAll <- ggplot(data=incAll2, aes(x=year, y=medianIncAdj)) +
  geom_line()+
  ggtitle("Median Income for All in the Labor Force") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


###Black/White median income
##With all respondents

#Tabluation
incBlackWhiteAll <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0) %>%
  group_by(year, race) %>%
  summarize(SampleCount=n(),
            medianInc=median(as.numeric(inctot), na.rm=TRUE))

#Add CPI
incBlackWhiteAll2 <- merge(incBlackWhiteAll, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incBlackWhiteAll2 <- mutate(incBlackWhiteAll2, 
                            medianIncAdj = medianInc*CPIAUCSL)

incBlackWhiteAll2 <- incBlackWhiteAll2 %>%
  filter(race=="Black/Negro" | race=="White")

#Plots
plotMedIncBWAll <- ggplot(data=incBlackWhiteAll2, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=race)) +
  geom_line(data=incAll2, aes(x=year, y=medianIncAdj)) +
  ggtitle("Median Income for Black and White Individuals") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


##With those in labor force
incBlackWhite3 <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0, labforce=="Yes, in the labor force") %>%
  group_by(year, race) %>%
  summarize(SampleCount=n(),
            medianInc=median(as.numeric(inctot), na.rm=TRUE),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Add CPI
incBlackWhite4 <- merge(incBlackWhite3, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incBlackWhite4 <- mutate(incBlackWhite4, 
                         medianIncAdj = medianInc*CPIAUCSL)

incBlackWhite4 <- incBlackWhite4 %>%
  filter(race=="Black/Negro" | race=="White")

#Plots
plotMedIncBWWk <- ggplot(data=incBlackWhite4, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=race)) +
  geom_line(data=incWK2, aes(x=year, y=medianIncAdj)) +
  ggtitle("Median Income for Black and White Individuals, \n In the Labor Force") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


###White/Non-white median income
##With all respondents

#Tabluation
incWhiteNon <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0) %>%
  group_by(year, white) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE))

#Add CPI
incWhiteNon2 <- merge(incWhiteNon, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incWhiteNon2 <- mutate(incWhiteNon2, 
                       medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotIncWNAll <- ggplot(data=incWhiteNon2, aes(x=year, y=medianIncAdj, group=white)) +
  geom_line(aes(color=white)) +
  ggtitle("Median Income for White and Non-white Individuals") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


##With those in labor force
incWhiteNon3 <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0, labforce=="Yes, in the labor force") %>%
  group_by(year, white) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE))

#Add CPI
incWhiteNon4 <- merge(incWhiteNon3, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incWhiteNon4 <- mutate(incWhiteNon4, 
                       medianIncAdj = medianInc*CPIAUCSL)
#Plots
plotIncWNWk <- ggplot(data=incWhiteNon4, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=white)) +
  geom_line(data=incWK2, aes(x=year, y=medianIncAdj)) +
  ggtitle("Median Income for White and Non-white Individuals, \n In the Labor Force") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


###Male/Female median income
##With all respndents

#Tabulation
incGender <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0) %>%
  group_by(year, sex) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE))

#Add CPI
incGender2 <- merge(incGender, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incGender2 <- mutate(incGender2, 
                     medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotIncGenderAll <- ggplot(data=incGender2, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=sex)) +
  geom_line(data=incAll2, aes(x=year, y=medianIncAdj)) +
  ggtitle("Median Income for Women and Men, \n All Workers") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


##With those in labor force
incGender3 <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0, labforce=="Yes, in the labor force") %>%
  group_by(year, sex) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE))

#Add CPI
incGender3 <- merge(incGender3, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incGender4 <- mutate(incGender3, 
                     medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotIncGenderWk <- ggplot(data=incGender4, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=sex)) +
  geom_line(data=incWK2, aes(x=year, y=medianIncAdj)) +
  ggtitle("Median Income for Women and Men, \n In the Labor Force") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


###Public/Private Median Income
##Total

#Tabulation
incPubPriv <- census %>%
  filter(as.numeric(age)>=15, as.numeric(inctot)>0, !is.na(classwkrR)) %>%
  group_by(year, classwkrR) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE),
            SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdproportion=totper/sum(totper))

#Add CPI
incPubPriv2 <- merge(incPubPriv, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incPubPriv2 <- mutate(incPubPriv2, 
                      medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotMedIncPubPriv <- ggplot(data=incPubPriv2, aes(x=year, y=medianIncAdj, group=classwkrR)) +
  geom_line(aes(color=classwkrR)) +
  ggtitle("Median Income for Public and Private Sector Workers") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


#Proportion line, put next to above plot
plotPropPubPriv <- ggplot(data=incPubPriv2, aes(x=year, y=wtdproportion)) +
  geom_line(data=subset(incPubPriv2, classwkrR=="Public Sector")) +
  ggtitle("Share of employed workers in the Public Sector") +
  xlab("Year") +
  ylab("Proportion") + 
  theme(legend.title=element_blank())  



##Public/Private:Black/White Median Income

#Tabulation
incBWPubPriv <- census %>%
  filter(!is.na(classwkrR)) %>%
  group_by(year, classwkrR, race) %>%
  summarize(SampleCount=n(),
            medianInc=median(as.numeric(inctot), na.rm=TRUE),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Add CPI
incBWPubPriv2 <- merge(incBWPubPriv, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incBWPubPriv2 <- mutate(incBWPubPriv2,
                        medianIncAdj = medianInc*CPIAUCSL)

incBWPubPriv2 <- incBWPubPriv2 %>%
  filter(race=="Black/Negro" | race=="White")

#Plots
plotMedIncPubPrivBW <- ggplot(data=incBWPubPriv2, aes(x=year, y=medianIncAdj)) +
  geom_line(aes(color=interaction(classwkrR, race))) +
  ggtitle("Median incomes in the Public and Private Sector \n for Black and White Workers") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


#Proportion line, put next to above plot 
plotPropPubPrivBW <- ggplot(data=incBWPubPriv2, aes(x=year, y=wtdProportion)) +
  geom_line(data=subset(incBWPubPriv2, race=="Black/Negro"), aes(color=interaction(race, classwkrR))) +
  ggtitle("Share of Black workers in the Public and Private Sectors") +
  xlab("Year") +
  ylab("Proportion") + 
  theme(legend.title=element_blank()) +
  scale_color_discrete(name="Sector",
                       breaks=c("Black/Negro.Private Sector", "Black/Negro.Public Sector"),
                       labels=c("Private Sector", "Public Sector"))


##Public/Private:White/Non-white Median Income

#Tabulation
incWNPubPriv <- census %>%
  filter(!is.na(classwkrR)) %>%
  group_by(year, classwkrR, white) %>%
  summarize(SampleCount=n(),
            medianInc=median(as.numeric(inctot), na.rm=TRUE),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Add CPI
incWNPubPriv2 <- merge(incWNPubPriv, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incWNPubPriv2 <- mutate(incWNPubPriv2, 
                        medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotMedIncPubPrivWN <- ggplot(data=incWNPubPriv2, aes(x=year, y=medianIncAdj, group=interaction(classwkrR, white))) +
  geom_line(aes(color=interaction(classwkrR, white))) +
  ggtitle("Median incomes in the Public and Private Sector \n for White and Non-white Workers") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


##Public/Private:Male/Female Median Income

#Tabulation
incMFPubPriv <- census %>%
  filter(!is.na(classwkrR)) %>%
  group_by(year, classwkrR, sex) %>%
  summarize(SampleCount=n(),
            medianInc=median(as.numeric(inctot), na.rm=TRUE),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Add CPI
incMFPubPriv2 <- merge(incMFPubPriv, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incMFPubPriv2 <- mutate(incMFPubPriv2, 
                        medianIncAdj = medianInc*CPIAUCSL)

#Plots
plotMedIncPubPrivMF <- ggplot(data=incMFPubPriv2, aes(x=year, y=medianIncAdj, group=interaction(classwkrR, sex))) +
  geom_line(aes(color=interaction(classwkrR, sex))) +
  ggtitle("Median incomes in the Public and Private Sector \n for Female and Male Workers") +
  xlab("Year") +
  ylab("Median income (in 2008 $s)") + 
  theme(legend.title=element_blank())


###Health Insurance coverage, All

healthIns <- census %>%
  filter(!is.na(labforce), !is.na(paidgh)) %>%
  group_by(year, paidgh) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotHealthIns <- ggplot(data=healthIns, aes(x=year, y=wtdProportion)) +
  ylim(.85,1) +
  geom_line(data=subset(healthIns, paidgh=="Yes")) +
  ggtitle("Covered by Employer's Group Healthcare plan") +
  xlab("Year") +
  ylab("Proportion covered") + 
  theme(legend.title=element_blank())


###Health Insurance coverage, Public/Private
healthInsPubPriv <- census %>%
  filter(!is.na(classwkrR), !is.na(paidgh)) %>%
  group_by(year, classwkrR, paidgh) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotHealthInsPubPriv <- ggplot(data=healthInsPubPriv, aes(x=year, y=wtdProportion)) +
  ylim(.85,1) +
  geom_line(data=subset(healthInsPubPriv, paidgh=="Yes"), aes(color=classwkrR)) +
  ggtitle("Covered by Employer's Group Healthcare Plan, \n for Public and Private Sector Workers") +
  xlab("Year") +
  ylab("Proportion covered") + 
  theme(legend.title=element_blank())


###Health Insurance coverage, White/Non-white
healthInsWN <- census %>%
  filter(!is.na(white), !is.na(paidgh)) %>%
  group_by(year, white, paidgh) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotHealthInsWN <- ggplot(data=healthInsWN, aes(x=year, y=wtdProportion)) +
  ylim(.85,1) +
  geom_line(data=subset(healthInsWN, paidgh=="Yes"), aes(color=white)) +
  ggtitle("Covered by Employer's Group Healthcare Plan,\n for White and Non-white Workers") +
  xlab("Year") +
  ylab("Proportion covered") + 
  theme(legend.title=element_blank())


###Pension through employment 
##Total
pensionCov <- census %>%
  filter(!is.na(pension)) %>%
  group_by(year, pension) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotPensionCov <- ggplot(data=pensionCov, aes(x=year, y=wtdProportion)) +
  geom_line(aes(color=pension)) +
  ggtitle("Receive Pension from employer, \n by Public and Private Sector Workers") +
  xlab("Year") +
  ylab("Proportion receiving pensions") + 
  theme(legend.title=element_blank())

##Public/Private
pensionPubPriv <- census %>%
  filter(!is.na(classwkrR), !is.na(pension)) %>%
  group_by(year, classwkrR, pension) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotPensionPubPriv <- ggplot(data=pensionPubPriv, aes(x=year, y=wtdProportion)) +
  geom_line(aes(color=interaction(classwkrR, pension))) +
  ggtitle("Receive Pension from employer, \n by Public and Private Sector Workers") +
  xlab("Year") +
  ylab("Proportion receiving pensions") + 
  theme(legend.title=element_blank())


##White/Non-white
pensionWN <- census %>%
  filter(!is.na(pension)) %>%
  group_by(year, white, pension) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plot
plotPensionWN <-  ggplot(data=pensionWN, aes(x=year, y=wtdProportion)) +
  geom_line(aes(color=interaction(white, pension))) +
  ggtitle("Receive Pension from employer, \n by White and Non-white Workers") +
  xlab("Year") +
  ylab("Proportion receiving pensions") + 
  theme(legend.title=element_blank())


###Cleaners/Sanitation/Healthcare workers
##Median Income Tabulation
incCleaners <- census %>%
  filter(cleaners=="Cleaners" | cleaners=="Healthcare workers" | cleaners=="Sanitation workers") %>%
  group_by(year, cleaners) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE),
            count=n())

#Add CPI
incCleaners2 <- merge(incCleaners, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incCleaners2 <- mutate(incCleaners2, 
                       medianIncAdj = medianInc*CPIAUCSL)
#Plots
plotIncCleaners <- ggplot(data=incCleaners2, aes(x=year, y=medianIncAdj, group=cleaners)) +
  geom_line(data=subset(incCleaners2, cleaners=="Cleaners" | cleaners=="Healthcare workers"), aes(color=cleaners)) +
  ggtitle("Median Incomes for Jobs") +
  xlab("Year") +
  ylab("Median Income, in 2008 $s") + 
  theme(legend.title=element_blank())


## Median Income Tabulation:Public/Private
incCleaners3 <- census %>%
  filter(!is.na(classwkrR), cleaners=="Cleaners" | cleaners=="Healthcare workers" | cleaners=="Sanitation workers") %>%
  group_by(year, cleaners, classwkrR) %>%
  summarize(medianInc=median(as.numeric(inctot), na.rm=TRUE),
            count=n())

#Add CPI
incCleaners4 <- merge(incCleaners3, cdf, by.x = "year", by.y = "year", all.x=T, all.y=F)

incCleaners4 <- mutate(incCleaners4, 
                       medianIncAdj = medianInc*CPIAUCSL)
#Plots
plotIncCleanerPubPriv <- ggplot(data=incCleaners4, aes(x=year, y=medianIncAdj)) +
  geom_line(data=subset(incCleaners4, cleaners=="Cleaners" | cleaners=="Healthcare workers"), aes(color=interaction(classwkrR, cleaners))) +
  ggtitle("Median Incomes for Jobs \n in the Public and Private Sectors") +
  xlab("Year") +
  ylab("Median Income, in 2008 $s") + 
  theme(legend.title=element_blank())

## Proportion White/Non-white
#Tabulation
cleanersWN <- census %>%
  filter(cleaners=="Cleaners" | cleaners=="Healthcare workers" | cleaners=="Sanitation workers") %>%
  group_by(year, cleaners, white) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))


#Plots
plotCleanersWN <- ggplot(data=cleanersWN, aes(x=year, y=wtdProportion)) +
  geom_line(data=subset(cleanersWN, white=="Non-white" & c(cleaners=="Cleaners" | cleaners=="Healthcare workers")), aes(color=cleaners)) +
  ggtitle("Share of Non-white for Jobs") +
  xlab("Year") +
  ylab("Proportion") + 
  theme(legend.title=element_blank())


###Health Insurance coverage, Cleaners
healthInsCleaners <- census %>%
  filter(!is.na(paidgh), cleaners=="Cleaners" | cleaners=="Healthcare workers" | cleaners=="Sanitation workers") %>%
  group_by(year, cleaners, paidgh) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotHealthInsCleaners <- ggplot(data=healthInsCleaners, aes(x=year, y=wtdProportion)) +
  ylim(.85,1) +
  geom_line(data=subset(healthInsCleaners, paidgh=="Yes" & c(cleaners=="Cleaners" | cleaners=="Healthcare workers")), aes(color=cleaners)) +
  ggtitle("Covered by Employer's Group Healthcare Plan,\n for Job Categories") +
  xlab("Year") +
  ylab("Proportion covered") + 
  theme(legend.title=element_blank())

##Receive Pensions from employer
pensionCleaners <- census %>%
  filter(!is.na(pension), cleaners=="Cleaners" | cleaners=="Healthcare workers" | cleaners=="Sanitation workers") %>%
  group_by(year, cleaners, pension) %>%
  summarize(SampleCount=n(),
            totper=sum(wtsupp)) %>%
  mutate(wtdProportion=totper/sum(totper))

#Plots
plotPensionCleaners <- ggplot(data=pensionCleaners, aes(x=year, y=wtdProportion)) +
  geom_line(data=subset(pensionCleaners, cleaners=="Cleaners" | cleaners=="Healthcare workers"), aes(color=interaction(cleaners, pension))) +
  ggtitle("Receive Pension from employer, \n by Job Categories") +
  xlab("Year") +
  ylab("Proportion receiving pensions") + 
  theme(legend.title=element_blank())