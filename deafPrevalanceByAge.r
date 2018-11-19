library(readr)
library(dplyr)
library(ggplot2)
source('generalCode/estimationFunctions.r')

###In households with a deaf child between the age of 0-22(?),
###how many of them have a head of household (or actually, a parent) who are also deaf?

pVars <- c('agep','dear','sex','pwgtp',paste0('pwgtp',1:80))

firstTry <- read_csv('../../data/acs5yr2016/ss16pusa.csv',n_max=10)

ct <- ifelse(tolower(names(firstTry))%in%pVars,'i','-')
print(table(ct))
print(names(firstTry)[ct!='-'])
ct <- paste(ct,collapse='')

pdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv',col_types=ct)
str(pdat)

pdat <- rbind(
    pdat,
    read_csv('../../data/acs5yr2016/ss16pusb.csv',col_types=ct),
    read_csv('../../data/acs5yr2016/ss16pusc.csv',col_types=ct),
    read_csv('../../data/acs5yr2016/ss16pusd.csv',col_types=ct))

names(pdat) <- tolower(names(pdat))

pdat$agep <- as.factor(pdat$agep)

ests <- svby('dear==1','agep',FUN=estSEstr,sdat=pdat,prop=FALSE)

estsSex <- pdat%>%group_by(sex,agep)%>%do(propDeaf=estSEstr('dear==1',sdat=.))

save(ests,estsSex,file='deafPropByAge.RData')


