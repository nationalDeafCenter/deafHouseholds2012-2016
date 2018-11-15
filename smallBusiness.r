library(readr) ## read in the csvs faster
library(dplyr)
library(survey)

## Median Annual Salary STEM
## Number/Percent in STEM field

## load in data: 5-year ACS



pVars <- c('SERIALNO','DEAR','ST','AGEP','COW','ADJINC','SCHL','WKW','WKHP','SCH','WAGP', 'PERNP', 'PINCP', 'SSIP', 'OCCP','COW','ESR','PWGTP',paste0('PWGTP',1:80))

    #hVars <- c('SERIALNO','TYPE','LNGI','HUPAC','HUPAOC','HUPARC','FPARC','FES','HHT','PARTNER','WKEXREL','WIF','HINCP')

## need: DEAR, attain, employment,PERNP, fulltime

firstTry <- read_csv('../../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(names(firstTry)%in%pVars,
              ifelse(names(firstTry)=='OCCP','c','i'),'-')
ccc <- paste(ccc,collapse='')

sdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv',col_types=ccc) ### CHANGE TO APPROPRIATE LOCATION


for(pp in c('b','c','d')){
    sdat2 <- read_csv(paste0('../../data/acs5yr2016/ss16pus',pp,'.csv'),col_types=ccc) ## CHANGE TO APPROPRIATE LOCATION
    sdat <- rbind(sdat[,pVars],sdat2[,pVars])
}


rm(sdat2)
gc()

names(sdat) <- tolower(names(sdat))

sdat$state <- states$abb[match(sdat$ST,states$x)]

sdat$stem <- sdat$occp%in%stemCodes$code
sdat$stemrel <- sdat$occp%in%c(stemRelatedCodes$code,stemCodes$code)

sdat$adj <- sdat$adjinc/1e6

sdat <- sdat%>%filter(agep>24,agep<65)%>%
    mutate(hs = schl>=16,
           ba = schl>=21,
           employed = esr%in%c(1,2,4,5),
           unemployed = esr==3,
           fulltime=(wkw==1 & wkhp>=35),
           earn=pernp*adj,
           inc=pincp*adj,

           raceEth=ifelse(hisp>1,"Hispanic",
                   ifelse(rac1p==2,"African American",
                   ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                   ifelse(rac1p%in%c(3,4,5),'American Indian',
                   ifelse(rac1p==1,"White","Other"))))))

sdat <- sdat%>%filter(fulltime)%>%mutate(stemJob=factor(ifelse(stem,'stem',
                                                        ifelse(stemrel,'stem-related','non-stem')),
                                                        levels=c('stem','stem-related','non-stem')),
                                         BA=factor(ifelse(ba&(sciengp==1),'stem',
                                                   ifelse(ba&(sciengrlp==1),'stem-related',
                                                   ifelse(ba,'non-stem','none'))),
                                                   levels=c('stem','stem-related','non-stem','none')))
