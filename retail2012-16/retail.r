library(readr) ## read in the csvs faster
library(dplyr)
library(survey)

source('../generalCode/estimationFunctions.r')

## 411011         .SAL-First-Line Supervisors Of Retail Sales Workers
## 411012         .SAL-First-Line Supervisors Of Non-Retail Sales Workers
## 412010         .SAL-Cashiers
## 412021         .SAL-Counter And Rental Clerks
## 412022         .SAL-Parts Salespersons
## 412031         .SAL-Retail Salespersons


pVars <- c('SERIALNO','DEAR','ST','AGEP','COW','SCHL','WKW','WKHP','SCH', 'OCCP','NAICSP','INDP','OCCP', 'SOCP','ESR','RELP','PWGTP',paste0('PWGTP',1:80))


firstTry <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(tolower(names(firstTry))%in%tolower(pVars),
              ifelse(names(firstTry)%in%c('SOCP','OCCP','NAICSP','INDP'),'c','i'),'-')
missingVars <- setdiff(tolower(pVars),tolower(names(firstTry)[ccc!='-']))
if(length(missingVars)) cat('WARNING: Missing these variables:\n',missingVars,'\n')
ccc <- paste(ccc,collapse='')


dat <- lapply(c('a','b','c','d'),function(part)
  read_csv(paste0('../../../data/acs5yr2016/ss16pus',part,'.csv'),col_types=ccc)
)

varNames <- lapply(dat,names)
sameVarNames <- sapply(varNames,identical,varNames[[1]])
stopifnot(all(sameVarNames))

dat <- do.call('rbind',dat)

gc()

4661257+3408501+3672720+3939449

names(dat) <- tolower(names(dat))


edlevs <- c(
    '<Grade 10',
    'Grade 10',
    'Grade 11',
    '12th grade - no diploma',
    'Regular high school diploma',
    'GED or alternative credential',
    'Some college, but less than 1 year',
    '1 or more years of college credit, no degree',
    'Associates degree',
    'Bachelors degree',
    'Masters degree',
    'Professional degree beyond a bachelors degree',
    'Doctorate degree')

dat$attain <- ifelse(dat$schl<13,1,dat$schl-11)
dat$attain <- factor(edlevs[dat$attain],levels=edlevs,ordered=TRUE)

gc()

dat$indpInt <- parse_integer(dat$indp)

dat <- dat%>%filter(agep>17,agep<65,relp!=16,esr%in%c(1,2,4,5))%>% ## relp==16 for institutionalized, limit to employed
  mutate(
      deaf=factor(ifelse(dear==1,'deaf','hearing')),
      Age=ordered(ifelse(agep<35,'25-34',
        ifelse(agep<45,'35-44',
          ifelse(agep<55,'45-54','55-64')))),
      attainCum=ordered(
        ifelse(attain<'Regular high school diploma','No HS',
          ifelse(attain<'Some college, but less than 1 year','HS Diploma',
            ifelse(attain<'Associates degree','Some College',
              ifelse(attain<'Bachelors degree','Associates',
                ifelse(attain<'Masters degree','Bachelors','Post-Graduate'))))),
        levels=c('No HS','HS Diploma','Some College','Associates','Bachelors','Post-Graduate')),
      retailINDP= (indp>=4670)&(indp<=5790),
      retailINDPint= (indpInt>=4670)&(indpInt<=5790),
      naicspPrefix=substr(naicsp,1,2),
      retailNAICSP=naicspPrefix%in%c('44','45','4M'),
      ## retail1= (socp>=411012) & (socp <= 412031),
      ## retailCat=factor(
      ##   ifelse(socp==411011,"First-Line Supervisors Of Retail Sales Workers",
      ##   ifelse(socp==411012,"First-Line Supervisors Of Non-Retail Sales Workers",
      ##     ifelse(socp==412010,"Cashiers",
      ##       ifelse(socp==412021,"Counter And Rental Clerks",
      ##         ifelse(412022,"Parts Salespersons",
      ##           ifelse(412031,"Retail Salespersons","Not Retail"))))))),
      ## retail=retailCat!="Not Retail",
      fulltime=(wkw==1 & wkhp>=35))
gc()

## get 3-letter categories and full labels for OCCP codes
## file "occp.tab" copied and pasted from data dictionary
## with some edting to make it more machine-readable
occp <- read.table('occp.tab',header=FALSE,sep='&',colClasses='character')
occp$cat <- substr(occp$V2,1,3)
dat$occpCat <- occp$cat[match(dat$occp,occp$V1)]
occp$lab <- substr(occp$V2,5,500)
dat$occpLab <- occp$lab[match(dat$occp,occp$V1)]

## top 20 occupations for people working in retail industry (in dataset, not in population)
head(names(sort(table(dat$occpLab[dat$retailINDP]),decreasing=TRUE)),20)
