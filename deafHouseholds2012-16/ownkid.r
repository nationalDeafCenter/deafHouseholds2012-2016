#### use census bureau definition
#### "householder" is ref person--i.e. head of HH, i.e. don't worry if they're actually a kid
#### "own kid" "own kid"= any(relp%in%c(2,3,4)) i.e. biological, adopted, step-kid AND never married AND age<18

library(tidyverse)
source('../generalCode/estimationFunctions.r')


pVars <- c('serialno','agep','dear','relp','sex','msp','pwgtp',paste0('pwgtp',1:80))

firstTry <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',n_max=10)

ct <- ifelse(names(firstTry)=='SERIALNO','n',ifelse(tolower(names(firstTry))%in%pVars,'i','-'))
print(table(ct))
print(names(firstTry)[ct!='-'])
print(setdiff(pVars,tolower(names(firstTry)[ct!='-'])))
ct <- paste(ct,collapse='')

dat <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',col_types=ct)
#str(dat)

dat <- rbind(
    dat,
    read_csv('../../../data/acs5yr2017/psam_pusb.csv',col_types=ct),
    read_csv('../../../data/acs5yr2017/psam_pusc.csv',col_types=ct),
    read_csv('../../../data/acs5yr2017/psam_pusd.csv',col_types=ct))

names(dat) <- tolower(names(dat))

dat$id <- 1:nrow(dat)
weights <- dat%>%select(id,starts_with('pwgtp'))
dat <- dat%>%select(-starts_with('pwgtp'))

gc()

dat <- filter(dat,relp<16)%>%
  mutate(
    kid=(agep<18) & (!relp%in%c(0,1,6,13,8)), #not householder, married/partner to householder or parent
    ownkid=kid & relp%in%c(2:4)&(is.na(msp)|msp==6)&(agep<18),
    relkid=kid & relp%in%c(2:5,7,10)
  )%>%
  group_by(serialno)%>%
  filter(any(kid)&any(dear==1))%>% ## we're only interested in HH with kids & with a deaf person
  mutate(
    nkid=sum(kid),
    nDeafKid=sum(kid&(dear==1)),
    nOwnKid=sum(ownkid),
    nDeafOwnKid=sum(ownkid&(dear==1)),
    nRelKid=sum(relkid),
    nDeafRelKid=sum(relkid&(dear==1)),
    parent=relp%in%c(0,1)&nOwnKid>0, ## official ACS definition (vis a vis own child)
    reladult=(agep>17)&(relp%in%c(0,1,5,6,8,9,10,13) )& nRelKid>0, ## any family older than 17
    adult=agep>17,
    nParent=sum(parent),
    nDeafParent=sum(parent&(dear==1)),
    nRelAdult=sum(reladult),
    nDeafRelAdult=sum(reladult&(dear==1)),
    nAdult=sum(adult),
    nDeafAdult=sum(adult&(dear==1))
  )%>%
  ungroup()

save(dat,file='data.RData')

print(nrow(dat))
dat <- inner_join(dat,weights,by='id')
print(nrow(dat))

dat$anyDeafOwnKid <- dat$nDeafOwnKid>0
dat$anyDeafParent <- dat$nDeafParent>0

save(dat,file='data.RData')

print('proportion deaf kids living w/ at least 1 deaf parent (official definitions)')
print(estExpr(anyDeafParent,ownkid&(dear==1),dat,FALSE))

print('proportion deaf parents living w/ at least 1 own deaf kid (official definitions)')
print(estExpr(anyDeafOwnKid,parent&(dear==1),dat,FALSE))

