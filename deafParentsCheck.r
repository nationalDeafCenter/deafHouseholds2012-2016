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
        kid=agep<19 & (
            ((relp==0)&any(relp==6))| # ego is ref, parent in HH
            (relp%in%c(2:4,14))|  # ego is ref's kid, ref has no grandkids in house
            ((relp==15)&(fakeAge<suppressWarnings(max(agep[relp==13]))))| # ego is kid of unmarried partner of ref?
                                        #logic: ref is unrelated to ego;
                                        # if ref has unmarried partner, ego is at least 15 years younger;
                                        # if there is no umarried partner, max(agep[relp==13]) returns -Inf
            (relp==7)), #ego is ref's grandkid
        kid2=agep<19 &
            (((relp==0)&any(relp==6)&!any(relp%in%c(2:4,7,14)))| # ego is ref, parent in HH, ego is not parent
             ((relp%in%c(2:4,14))&!any(relp==7))|  # ego is ref's kid, ref has no grandkids in house
                                         # (excludes ppl living with parents and with
                                         #   siblings who are themselves parents)
             (relp==7)), #ego is ref's grandkid
        parent=
            (any(relp%in%c(2:4,14,7))&(relp==0))| #ego is ref; kid or grandkid in HH
            (any(relp%in%c(2:4,7,14))&(relp%in%c(1,13)))| # ego married/partner to ref; ref's (grand)kid in HH
            (relp==6)| # ego is parent of ref
            (relp%in%c(2:4,9,14)&any(relp==7)&(agep>max(fakeAge[relp==7])))| #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
            unmarriedPartnerParent|
            ((relp==0)&any(unmarriedPartnerParent)), # ego's unmarried partner is a parent (?)
        parent2=!kid2&
            ((any(relp%in%c(2:4,14,7))&(relp==0))| #ego is ref; kid or grandkid in HH
             (any(relp%in%c(2:4,7,14))&(relp%in%c(1,13)))| # ego married/partner to ref; ref's (grand)kid in HH
             (relp==6)&!any(relp%in%c(2:4,7,14))| # ego is parent of ref, ref doesn't have kids
             (relp%in%c(2:4,9,14)&any(relp==7))), #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
        numDeafParents=sum(parent&(dear==1)),
        numDeafParents2=sum(parent2&(dear==1))
        )%>%
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

kidDat <- pdat%>%
    mutate(
        fakeAge= agep+14,
        unmarriedPartnerParent=(relp==13)&(agep>suppressWarnings(min(fakeAge[deafKid&(relp==15)]))),
                                        #unmarried partner
                                        #may be parent of deaf kid. logic: at least 15 years older than
                                        # some deaf kid in house who is unrelated to ref.
                                        # when there is no deafKid&(relp==15), min returns Inf
        kidCat=ifelse(agep>18,'na',
               ifelse((relp==0)&any(relp==6),'ref',
               ifelse((relp%in%c(2:4,14)),'kid',
               ifelse(((relp==15)&(fakeAge<suppressWarnings(max(agep[relp==13])))),'unMarPar',
               ifelse(relp==7,'gran','na'))))),
        kidCat2=ifelse(agep>18,'na',
                ifelse((relp==0)&any(relp==6)&!any(relp%in%c(2:4,7,14)),'ref',
                ifelse(((relp%in%c(2:4,14))&!any(relp==7)),'kid',
                ifelse(relp==7,'gran','na')))))


pdat2 <- pdat%>%
       mutate(
        kid2=agep<19 &
            (((relp==0)&any(relp==6)&!any(relp%in%c(2:4,7,14)))| # ego is ref, parent in HH, ego is not parent
             ((relp%in%c(2:4,14))&!any(relp==7))|  # ego is ref's kid, ref has no grandkids in house
                                         # (excludes ppl living with parents and with
                                         #   siblings who are themselves parents)
             (relp==7)), #ego is ref's grandkid
         parent2=!kid2&
             ((any(relp%in%c(2:4,14,7))&(relp==0))| #ego is ref; kid or grandkid in HH
              (any(relp%in%c(2:4,7,14))&(relp%in%c(1,13)))| # ego married/partner to ref; ref's (grand)kid in HH
              (relp==6)&!any(relp%in%c(2:4,7,14))| # ego is parent of ref, ref doesn't have kids
             (relp%in%c(2:4,9,14)&any(relp==7))), #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
        deafKidAge2=max(agep[deafKid &!parent2]),
        adult2=agep>17 & agep>deafKidAge2+14,
        olderRelative2=!parent2&
            (agep>(deafKidAge2+5))& # 5 years older than oldest deaf kid
            agep>18 & #"adult"
            ((relp%in%c(5,10))|(relp==0 & any(relp[deafKid]%in%c(5,10)))),
        strictParent2=((relp==0)&any(relp%in%c(2:4)))|
            ((relp==1)&any(relp%in%c(2:4)))|relp==6,
        strictKid2=((relp==0)&any(relp==6))|relp%in%c(2:4),
        numDeafStrictParents2=sum((strictParent2)&(dear==1)),
        numDeafKids2=sum(deafKid),
        numKids2=sum(kid2),
        numDeafParents2=sum(parent2&(dear==1)),
        numParents2=sum(parent2),
        numDeafRel2=sum(olderRelative2&(dear==1)),
        numRel2=sum(olderRelative2),
        numAdult2=sum(adult2),
        numDeafAdult2=sum(adult2&(dear==1)))%>%
    ungroup()


pdat <- within(pdat,{
    numAdults=vapply(1:nrow(pdat),function(i) sum(adult&(serialno==serialno[i])&(agep>fakeAge[i])),1)
    numDeafAdults=vapply(1:nrow(pdat),function(i) sum(adult&(serialno==serialno[i])&(agep>fakeAge[i])&(dear==1)),1)
})

pdat <- within(pdat,{
    anyDeafParents=numDeafParents>0
    twoPlusDeafParents=numDeafParents>1
    anyDeafParents2=numDeafParents2>0
    twoPlusDeafParents2=numDeafParents2>1

   # anyDeafAdults=numDeafAdults>0
   # twoPlusDeafAdults=numDeafAdults>1
})


sink('deafParents.txt')
print('% deaf kids living w/ parent/guardian with ANY deaf parent/guardian (proportion, SE, n):')
print(estExpr(anyDeafParents,kid&(dear==1),pdat))
print('% deaf kids living w/ parent/guardian with 2+ deaf parent/guardian (proportion, SE, n):')
print(estExpr(twoPlusDeafParents,kid&(dear==1),pdat))

print('% deaf kids living w/ older adults with ANY deaf adults (proportion, SE, n):')
print(estExpr(anyDeafAdults,deafCharge,pdat))
print('% deaf kids living w/ older adults with ANY deaf adults (proportion, SE, n):')
print(estExpr(twoPlusDeafAdults,deafCharge,pdat))
sink()



### checking stuff
### deaf kid & not deafcharge
pdat$dkid <- pdat$kid&(pdat$dear==1)
 pdat%>%group_by(serialno)%>%filter(any(dkid&!deafCharge))%>%select(-starts_with('pwg'))%>%summarize(oldest=max(agep),deafkidage=fakeAge[dkid&!deafCharge])
## # A tibble: 18 x 3
##         serialno oldest deafkidage
##            <dbl>  <dbl>      <dbl>
##  1 2012000442519   18.0       18.0
##  2 2012000566738   26.0       26.0
##  3 2012000702903   31.0       31.0
##  4 2013000600649   20.0       28.0
##  5 2013001024915   30.0       31.0
##  6 2013001197136   17.0       15.0
##  7 2013001287881   28.0       30.0
##  8 2014000586399   31.0       31.0
##  9 2014000590389   27.0       27.0
## 10 2014000834363   20.0       21.0
## 11 2014000852554   26.0       26.0
## 12 2015000644196   17.0       15.0
## 13 2015001485242   22.0       22.0
## 14 2016000078078   25.0       25.0
## 15 2016000567173   26.0       26.0
## 16 2016000859862   21.0       21.0
## 17 2016001254178   26.0       27.0
## 18 2016001281941   27.0       28.0

byHH <- pdat%>%group_by(serialno)%>%summarize(
                                        numDeafKids=mean(numDeafKids),
                                        numKids=mean(numKids),
                                        numDeafParents=mean(numDeafParents),
                                        numParents=mean(numParents),
                                        numDeafAdults=ifelse(any(deafCharge),mean(numDeafAdults[deafCharge]),0),
                                        numAdults=ifelse(any(deafCharge),mean(numAdults[deafCharge]),0),
                                        size=n())
