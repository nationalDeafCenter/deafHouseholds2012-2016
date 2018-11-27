library(readr)
library(dplyr)
source('generalCode/estimationFunctions.r')

###In households with a deaf child between the age of 0-22(?),
###how many of them have a head of household (or actually, a parent) who are also deaf?

pVars <- c('serialno','agep','dear','relp','sex','pwgtp',paste0('pwgtp',1:80))

firstTry <- read_csv('../../data/acs5yr2016/ss16pusa.csv',n_max=10)

ct <- ifelse(names(firstTry)=='SERIALNO','n',ifelse(tolower(names(firstTry))%in%pVars,'i','-'))
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

