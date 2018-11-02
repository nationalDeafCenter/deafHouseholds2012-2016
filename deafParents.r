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
        fakeAge <- age+14,
        unmarriedPartnerParent=(relp==13)&(agep>supressWarnings(min(fakeAge[deafKid&(relp==15)]))),
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
        parent=
            (any(relp%in%c(2:4,14,7))&(relp==0))| #ego is ref; kid or grandkid in HH
            (any(relp%in%c(2:4,7,14))&(relp%in%c(1,13)))| # ego married/partner to ref; ref's (grand)kid in HH
            (relp==6)| # ego is parent of ref
            (relp%in%c(2:4,9,14)&any(relp==7))| #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
            unmarriedPartnerParent|
            ((relp==0)&any(unmarriedPartnerParent)), # ego's unmarried partner is a parent (?)
        deafCharge=deafKid&(fakeAge<supressWarnings(max(agep[agep>17]))), # there's at least one adult 15 years
                                        #older than this deaf kid
        numDeafKids=sum(deafKid),
        numKids=sum(kid),
        numDeafParents=sum(parent&(dear==1)),
        anyDeafParents=numDeafParents>0,
        twoPlusDeafParents=numDeafParents>1,
        numParents=sum(parent))%>%
    ungroup()

pdat <- within(pdat,{
    numAdults=vapply(1:nrow(pdat),function(i) sum(adult&(serialno==serialno[i])&(agep>fakeAge[i])),1)
    numDeafAdults=vapply(1:nrow(pdat),function(i) sum(adult&(serialno==serialno[i])&(agep>fakeAge[i])&(dear==1)),1)
})
pdat <- within(pdat,{
    anyDeafAdults=numDeafAdults>0
    twoPlusDeafAdults=numDeafAdults>1
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


byHH <- pdat%>%group_by(serialno)%>%summarize(
                                        numDeafKids=mean(numDeafKids),
                                        numKids=mean(numKids),
                                        numDeafParents=mean(numDeafParents),
                                        numParents=mean(numParents),
                                        numDeafAdults=ifelse(any(deafCharge),mean(numDeafAdults[deafCharge]),0),
                                        numAdults=ifelse(any(deafCharge),mean(numAdults[deafCharge]),0),
                                        size=n())
