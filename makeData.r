library(readr)
library(dplyr)


###In households with a deaf child between the age of 0-22(?),
###how many of them have a head of household (or actually, a parent) who are also deaf?

pVars <- c('serialno','agep','dear','relp')

firstTry <- read_csv('../../data/acs5yr2016/ss16pusa.csv',n_max=10)

ct <- ifelse(names(firstTry)=='SERIALNO','n',ifelse(tolower(names(firstTry))%in%pVars,'i','-'))
ct <- paste(ct,collapse='')

pdat <- read_csv('../../data/acs5yr2016/ss16pusa.csv',col_types=ct)

pdat <- rbind(
    pdat,
    read_csv('../../data/acs5yr2016/ss16pusb.csv',col_types=ct),
    read_csv('../../data/acs5yr2016/ss16pusc.csv',col_types=ct),
    read_csv('../../data/acs5yr2016/ss16pusd.csv',col_types=ct))

names(pdat) <- tolower(names(pdat))

### only keep HHs with deaf person < 19
### for now: parents can't be kids
### grandkids are kids, grandparents are parents

pdat <- pdat%>%mutate(deafKid= (dear==1)&(agep<19))%>%
    group_by(serialno)%>%filter(any(deafKid))%>%
    mutate(
        kid=agep<19 &
            (((relp==0)&any(relp==6)&!any(relp%in%c(2:4,7,14)))| # ego is ref, parent in HH, ego is not parent
            ((relp%in%c(2:4,14))&!any(relp==7))|  # ego is ref's kid, ref has no grandkids in house
                                        # (excludes ppl living with parents and with
                                        #   siblings who are themselves parents)
            (relp==7)), #ego is ref's grandkid
        parent=!kid&
            ((any(relp%in%c(2:4,14,7))&(relp==0))| #ego is ref; kid or grandkid in HH
             (any(relp%in%c(2:4,7,14))&(relp%in%c(1,13)))| # ego married/partner to ref; ref's (grand)kid in HH
             (relp==6)&!any(relp%in%c(2:4,7,14))| # ego is parent of ref, ref doesn't have kids
             (relp%in%c(2:4,9,14)&any(relp==7))), #ego is parent, ref is grandparent
                                        #(maybe ego is aunt/uncle of kids...impossible to tell I think)
        deafKidAge=max(agep[deafKid &!parent]),
        olderRelative=!parent&
            (agep>(deafKidAge+5))& # 5 years older than oldest deaf kid
            agep>18 & #"adult"
            ((relp%in%c(5,10))|(relp==0 & any(relp[deafKid]%in%c(5,10)))),
        numDeafKids=sum(deafKid),
        numKids=sum(kid),
        numDeafParents=sum(parent&(dear==1)),
        numParents=sum(parent),
        numDeafRel=sum(olderRelative&(dear==1)),
        numRel=sum(olderRelative))




