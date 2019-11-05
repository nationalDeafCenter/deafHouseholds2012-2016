library(readr)
library(dplyr)
source('../generalCode/estimationFunctions.r')

###In households with a deaf child between the age of 0-22(?),
###how many of them have a head of household (or actually, a parent) who are also deaf?

pVars <- c('serialno','agep','dear','relp','sex','msp','pwgtp',paste0('pwgtp',1:80))

firstTry <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',n_max=10)

ct <- ifelse(names(firstTry)=='SERIALNO','n',ifelse(tolower(names(firstTry))%in%pVars,'i','-'))
print(table(ct))
print(names(firstTry)[ct!='-'])
ct <- paste(ct,collapse='')

pdat <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',col_types=ct)
str(pdat)

pdat <- rbind(
    pdat,
    read_csv('../../../data/acs5yr2017/psam_pusb.csv',col_types=ct),
    read_csv('../../../data/acs5yr2017/psam_pusc.csv',col_types=ct),
    read_csv('../../../data/acs5yr2017/psam_pusd.csv',col_types=ct))

names(pdat) <- tolower(names(pdat))

### only keep HHs with deaf person < 19
### for now:
### grandkids are kids, grandparents are parents
### parents CAN be kids

### gallaudet paper:
### " these young people were 6 to 19 years of age"

pdatO <- pdat

pdat <- pdat%>%mutate(deafKid= (dear==1)&(agep<19))%>%
    filter(relp<16)%>% # take out group living quarters
    group_by(serialno)%>%filter(any(deafKid))

pdat <- pdat%>%
    mutate(
        fakeAge= agep+14,
        unmarriedPartnerParent=(relp==13)&(agep>suppressWarnings(min(fakeAge[deafKid&(relp==15)]))),
                                        #unmarried partner
                                        #may be parent of deaf kid. logic: at least 15 years older than
                                        # some deaf kid in house who is unrelated to ref.
                                        # when there is no deafKid&(relp==15), min returns Inf
        adult=agep>18 & agep>min(fakeAge[deafKid]), #at least 15 years older than some deaf kid and at least 19
        kid1=agep<19 & ((relp==0)&any(relp==6)), # ego is ref, parent in HH
        kid2=agep<19 & (relp%in%c(2:4,14)),  # ego is ref's kid, ref has no grandkids in house
        kid3=agep<19 & ((relp==15)&(fakeAge<suppressWarnings(max(agep[relp==13])))),
                                        # ego is kid of unmarried partner of ref?
                                        #logic: ref is unrelated to ego;
                                        # if ref has unmarried partner, ego is at least 15 years younger;
                                        # if there is no umarried partner, max(agep[relp==13]) returns -Inf
        kid4=agep<19 & (relp==7), #ego is ref's grandkid
        kid=kid1|kid2|kid3|kid4,
        parent1=(any(relp%in%c(2:4,14,7))&(relp==0)), #ego is ref; kid or grandkid in HH
        parent2=(any(relp%in%c(2:4,7,14))&(relp%in%c(1,13))), # ego married/partner to ref; ref's (grand)kid in HH
        parent3=(relp==6), # ego is parent of ref
        parent4= (relp%in%c(2:4,9,14)&any(relp==7)&(agep>min(fakeAge[relp==7]))), #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
        parent5= unmarriedPartnerParent,
        parent6=  ((relp==0)&any(unmarriedPartnerParent)), # ego's unmarried partner is a parent (?)
        parent=parent1|parent2|parent3|parent4|parent5|parent6,
        deafCharge=deafKid&(fakeAge<suppressWarnings(max(agep[agep>17]))), # there's at least one adult 15 years
                                        #older than this deaf kid
        numDeafKids=sum(deafKid),
        numKids=sum(kid),
        numDeafParents=sum(parent&(dear==1)),
        numParents=sum(parent))%>%
    ungroup()

## worry: multigenerational HH with young deaf parent(s). middle generation are both "parents" and "kids"
## someone cannot be their own deaf parent! example:
## grampa bob (hearing, age 60), bob's daughter alice (deaf, age 18), alice's partner beth (deaf, age 19),
## alice & beth's son joe (deaf, age 1).
## number of deaf parents in HH: 2. But alice shouldn't be counted as living with deaf parents--she is the deaf
## parent!
## instead, for each person, count number of parents in HH 15 years older then _them_ who are deaf.
## then joe has two deaf parents, alice has 0 deaf parents. (beth is >18 so we don't count her as kid)


countDeafParents <- function(i){
     rowi <- pdat[i,]
     hh <- filter(pdat[-i,],serialno==rowi$serialno)
     with(hh, sum(parent&(agep>rowi$fakeAge)&(dear==1)))
}

deafkidparentrows <- which(pdat$kid&(pdat$dear==1)&pdat$parent)
ndp <- vapply(deafkidparentrows,countDeafParents,1)

pdat$numDeafParents[deafkidparentrows] <- ndp

countDeafAdults <- function(deafCharge,adult,agep,fakeAge,dear){
    out <- rep(NA,length(adult))
    if(!any(deafCharge)) return(out)
    dc <- which(deafCharge)
    cc <- vapply(dc,function(i) sum(adult&(agep> fakeAge[i])&(dear==1)),integer(1))
    out[dc] <- cc
    out
}

pdat <- pdat%>%group_by(serialno)%>%
    mutate(numDeafAdults=countDeafAdults(deafCharge,adult,agep,fakeAge,dear))%>%
    ungroup()


pdat <- within(pdat,{
    anyDeafParents=numDeafParents>0
    twoPlusDeafParents=numDeafParents>1
    anyDeafAdults=numDeafAdults>0
    twoPlusDeafAdults=numDeafAdults>1
})

results <- list(
    anyPar=estExpr(anyDeafParents,kid&(dear==1),pdat),
    twoPar=estExpr(twoPlusDeafParents,kid&(dear==1),pdat),
    anyAd=estExpr(anyDeafAdults,deafCharge,pdat),
    twoAd=estExpr(twoPlusDeafAdults,deafCharge,pdat))

parentKidCategories <- list(
    kid=sapply(1:4,function(i) estSEstr(paste0('kid',i),subst='kid&(dear==1)',sdat=pdat)),
    parent=sapply(1:6,function(i) estSEstr(paste0('parent',i),subst='parent&(dear==1)',sdat=pdat)))


save(pdat,file='deafHHdata.RData')

save(parentKidCategories,results,file='deafHHresults.RData')

sink('deafParents.txt')
print('% deaf kids living w/ parent/guardian with ANY deaf parent/guardian (proportion, SE, n):')
print(results$anyPar)
print('% deaf kids living w/ parent/guardian with 2+ deaf parent/guardian (proportion, SE, n):')
print(results$twoPar)

print('% deaf kids living w/ older adults with ANY deaf adults (proportion, SE, n):')
print(results$anyAd)
print('% deaf kids living w/ older adults with ANY deaf adults (proportion, SE, n):')
print(results$twoAd)
sink()



############# append ACS "own child" analysis from "ownkid.r"
pdat <-
  pdat%>%
  ungroup()%>%
  filter(relp<16)%>%
  mutate(
    acskid=(agep<18) & (!relp%in%c(0,1,6,13,8)), #not householder, married/partner to householder or parent
    ownkid=acskid & relp%in%c(2:4)&(is.na(msp)|msp==6)&(agep<18),
    relkid=acskid & relp%in%c(2:5,7,10)
  )%>%
  group_by(serialno)%>%
  filter(any(acskid)&any(dear==1))%>% ## we're only interested in HH with kids & with a deaf person
  mutate(
    nacskid=sum(acskid),
    nDeafAcskid=sum(acskid&(dear==1)),
    nOwnKid=sum(ownkid),
    nDeafOwnkid=sum(ownkid&(dear==1)),
    nRelKid=sum(relkid),
    nDeafRelKid=sum(relkid&(dear==1)),
    acsparent=relp%in%c(0,1)&nOwnKid>0, ## official ACS definition (vis a vis own child)
    reladult=(agep>17)&(relp%in%c(0,1,5,6,8,9,10,13) )& nRelKid>0, ## any family older than 17
    acsadult=agep>17,
    nParent=sum(acsparent),
    nDeafParent=sum(acsparent&(dear==1)),
    nRelAdult=sum(reladult),
    nDeafRelAdult=sum(reladult&(dear==1)),
    nAdult=sum(acsadult),
    nDeafAdult=sum(acsadult&(dear==1))
  )%>%
  ungroup()

pdat$anyDeafOwnKid <- pdat$nDeafOwnkid>0
pdat$anyDeafParent <- pdat$nDeafParent>0
pdat$twoDeafParents <- pdat$nDeafParent==2

sink('acsDefinitionEstimates.txt')
print('proportion deaf kids living w/ at least 1 deaf parent (official definitions)')
print(estExpr(anyDeafParent,ownkid&(dear==1),pdat,FALSE))

print('proportion deaf kids living w/ 2 deaf parents (official definitions)')
print(estExpr(twoDeafParents,ownkid&(dear==1),pdat,FALSE))

print('proportion deaf parents living w/ at least 1 own deaf kid (official definitions)')
print(estExpr(anyDeafOwnKid,acsparent&(dear==1),pdat,FALSE))

sink()


### comparing
pdat%>%
  filter(dear==1,agep<18)%>%
  xtabs(~kid+ownkid,.)
# 2148 kids (adam's def) not acs "own kids"

## OK so they are mutually exclusive
table(with(pdat,kid1+kid2+kid3+kid4))
table(with(pdat,kid1*kid2*kid3*kid4))
pdat <- mutate(pdat,kidType=ifelse(kid1,1,ifelse(kid2,2,ifelse(kid3,3,ifelse(kid4,4,0)))))

xtabs(~kidType,filter(pdat,dear==1))
xtabs(~kidType+ownkid,filter(pdat,dear==1))
with(filter(pdat,dear==1),mean(ownkid[kidType==2]))
with(filter(pdat,dear==1,(kidType==2)&!ownkid),c(sum(agep==18),mean(agep==18)))
with(filter(pdat,dear==1,(kidType==2)&!ownkid),c(sum(relp==14),mean(relp==14)))
with(filter(pdat,dear==1,(kidType==2)&!ownkid),c(sum(msp!=6,na.rm=TRUE),mean(!is.na(msp)&msp!=6)))

##### parents
table(with(pdat,parent1+parent2+parent3+parent4+parent5+parent6))
table(with(pdat,parent1+parent2+parent3+parent4))
## 1-4 are mutually exclusive; 5 and 6 are not (106 cases of overlap)
xtabs(~parent1+parent6,pdat)
## parent6 and parent1 overlap in 53 cases
xtabs(~parent2+parent5,pdat)
## parent2 and parent5 overlap in 53 cases

table(with(filter(pdat,dear==1),parent1+parent2+parent3+parent4+parent5+parent6))
xtabs(~parent1+parent6,filter(pdat,dear==1))
## parent6 and parent1 overlap in 53 cases
xtabs(~parent2+parent5,filter(pdat,dear==1))

pdat <- mutate(pdat,parentType=ifelse(parent1,1,ifelse(parent2,2,ifelse(parent3,3,ifelse(parent4,4,ifelse(parent5,5,ifelse(parent6,6,0)))))))

addmargins(xtabs(~parentType+acsparent,pdat))

