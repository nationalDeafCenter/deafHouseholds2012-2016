library(readr) ## read in the csvs faster
library(dplyr)
library(survey)

source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')
## Median Annual Salary STEM
## Number/Percent in STEM field

## load in data: 5-year ACS



pVars <- c('SERIALNO','DEAR','ST','AGEP','COW','ADJINC','SCHL','WKW','WKHP','SCH','WAGP', 'PERNP', 'PINCP', 'SSIP', 'OCCP','ESR','RELP','PWGTP',paste0('PWGTP',1:80))

    #hVars <- c('SERIALNO','TYPE','LNGI','HUPAC','HUPAOC','HUPARC','FPARC','FES','HHT','PARTNER','WKEXREL','WIF','HINCP')

## need: DEAR, attain, employment,PERNP, fulltime

firstTry <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(names(firstTry)%in%pVars,
              ifelse(names(firstTry)=='OCCP','c','i'),'-')
ccc <- paste(ccc,collapse='')

sdat <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',col_types=ccc) ### CHANGE TO APPROPRIATE LOCATION


for(pp in c('b','c','d')){
    sdat2 <- read_csv(paste0('../../../data/acs5yr2016/ss16pus',pp,'.csv'),col_types=ccc) ## CHANGE TO APPROPRIATE LOCATION
    sdat <- rbind(sdat[,pVars],sdat2[,pVars])
}


rm(sdat2)
gc()

names(sdat) <- tolower(names(sdat))

### limit to non-institutional, ages 25-64
sdat <- filter(sdat,relp!=16,agep>24,agep<65)

sdat$adj <- sdat$adjinc/1e6

sdat <- mutate(sdat,
           employed = esr%in%c(1,2,4,5),
           unemployed = esr==3,
           fulltime=(wkw==1 & wkhp>=35),
           earn=pernp*adj,
           inc=pincp*adj,
           selfEmp=cow%in%(6:7),
           smallBiz=cow==7
           )

sdat$cow[is.na(sdat$cow)] <- 0 ## ppl not in labor force. give them separate category

## percent own small biz
ownSmallBiz <- list(
    overall= rbind(deaf=estExpr(smallBiz,dear==1,sdat),
                   hearing=estExpr(smallBiz,dear==2,sdat)),
    employed=rbind(deaf=estExpr(smallBiz,(dear==1)&employed,sdat),
                   hearing=estExpr(smallBiz,(dear==2)&employed,sdat)),
    under40=rbind(deaf=estExpr(smallBiz,(dear==1)&(agep<40),sdat),
                  hearing=estExpr(smallBiz,(dear==2)&(agep<40),sdat)),
    under40Employed=rbind(deaf=estExpr(smallBiz,(dear==1)&employed&(agep<40),sdat),
                   hearing=estExpr(smallBiz,(dear==2)&employed&(agep<40),sdat)))


selfEmp <- list(
    overall= rbind(deaf=estExpr(selfEmp,dear==1,sdat),
                   hearing=estExpr(selfEmp,dear==2,sdat)),
    employed=rbind(deaf=estExpr(selfEmp,(dear==1)&employed,sdat),
                   hearing=estExpr(selfEmp,(dear==2)&employed,sdat)),
    under40=rbind(deaf=estExpr(selfEmp,(dear==1)&(agep<40),sdat),
                  hearing=estExpr(selfEmp,(dear==2)&(agep<40),sdat)),
    under40Employed=rbind(deaf=estExpr(selfEmp,(dear==1)&employed&(agep<40),sdat),
                   hearing=estExpr(selfEmp,(dear==2)&employed&(agep<40),sdat)))

deafOwners <- list(
    percentDeaf=estExpr(dear==1,sdat=sdat),
    numDeaf=svTot(sdat,'dear==1'),
    percentDeafUnder40=estExpr(dear==1,agep<40,sdat=sdat),
    numDeafUnder40=svTot(sdat,'(dear==1)&(agep<40)'),
    percentSB=estExpr(dear==1,smallBiz,sdat),
    percentSE=estExpr(dear==1,selfEmp,sdat),
    percentSBunder40=estExpr((dear==1),smallBiz&(agep<40),sdat),
    percentSEunder40=estExpr((dear==1),selfEmp&(agep<40),sdat),
    numSB=svTot(sdat,'(dear==1)&smallBiz'),
    numSE=svTot(sdat,'(dear==1)&selfEmp'),
    numSBu40=svTot(sdat,'(dear==1)&smallBiz&(agep<40)'),
    numSEu40=svTot(sdat,'(dear==1)&selfEmp&(agep<40)'))



medInc <- list(
 smallBiz=rbind(
     deaf=medStr('inc',subst='(dear==1)&smallBiz&employed',sdat=sdat),
     hearing=medStr('inc',subst='(dear==2)&smallBiz&employed',sdat=sdat)),
 smallBizUnder40=rbind(
     deaf=medStr('inc',subst='(dear==1)&smallBiz&employed&(agep<40)',sdat=sdat),
     hearing=medStr('inc',subst='(dear==2)&smallBiz&employed&(agep<40)',sdat=sdat)),
 selfEmp=rbind(
     deaf=medStr('inc',subst='(dear==1)&selfEmp&employed',sdat=sdat),
     hearing=medStr('inc',subst='(dear==2)&selfEmp&employed',sdat=sdat)),
 selfEmpUnder40=rbind(
     deaf=medStr('inc',subst='(dear==1)&selfEmp&employed&(agep<40)',sdat=sdat),
     hearing=medStr('inc',subst='(dear==2)&selfEmp&employed&(agep<40)',sdat=sdat)))


save(ownSmallBiz,selfEmp,deafOwners,medInc,file='smallBizResults.RData')

trans <- function(res,cn){
 res <- lapply(names(res),
  function(x){
   if(NCOL(res[[x]])>1) return(
    rbind(
     c(x,rep('',ncol(res[[x]])+1)),
     cbind(rep('',nrow(res[[x]])),rownames(res[[x]]),round(res[[x]],1))
    )
   )
  rbind(c(x,round(res[[x]],1)))
 })

 mlen <- max(sapply(res,NCOL))
 fff <- function(x) if(ncol(x)<mlen) cbind(x,matrix('',nrow(x),mlen-ncol(x))) else x
 res <- lapply(res,fff)

 mat <- do.call('rbind',res)
 if(!missing(cn))
     mat <- rbind(c(rep('',ncol(mat)-length(cn)),cn),mat)
 mat <- rbind(c('ACS 2012-2016',rep('',ncol(mat)-1)),mat)
 mat <- rbind(mat,
     c('Ages 25-64 unless otherwise specified; non-institutionalized',
                rep('',ncol(mat)-1)))

 mat
}


library(openxlsx)

write.xlsx(list(
    smallBusinessOwners=trans(ownSmallBiz,c('% Own Small Biz','SE','n')),
    selfEmployed=trans(selfEmp,c('% Self-Employed','SE','n')),
    deafOwners=trans(deafOwners,c('% or Number','SE','n')),
    medianIncome=trans(medInc,c('Median Income','SE','n'))),
    file='SmallBiz.xlsx',row.names=FALSE,col.names=FALSE)
