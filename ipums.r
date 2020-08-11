library(tidyverse)
#library(ff)
source('generalCode/estimationFunctions.r')

#dat <- read_csv('../../data/ipums/usa_00001.csv', n_max=20)
#dat <- read.csv.ffdf(file='../../data/ipums/usa_00001.csv')

dat <- read_csv('../../data/ipums/deafFam2018.csv')

names(dat) <- tolower(names(dat))

dat1 <- dat%>%
  filter(gq%in%c(1,2))%>%
    group_by(serial)%>%
      mutate(ndeaf=sum(diffhear==2,na.rm=TRUE))%>%
        filter(ndeaf>0)%>%
          ungroup()
rm(dat)


famDat <- dat1%>%
  group_by(serial)%>%
    summarize(nfams=nfams[1],nmothers=nmothers[1],nfathers=nfathers[1],npop=sum(poploc>0),
              nmom=sum(momloc>0),ownchildren=any(nchild>0),nkid=sum(age<17),
              ndeafkid=sum(age<17&diffhear==2),ndeaf=sum(diffhear==2),nppl=n())




deafKids <- dat1%>%
  group_by(serial)%>%
    arrange(pernum)%>%
      mutate(
        pn2=1:n(),
        hasDad=poploc>0,hasMom=momloc>0,
        hasParent=hasDad|hasMom,
        deafDad=ifelse(hasDad,diffhear[poploc]==2,FALSE),
        deafMom=ifelse(hasMom,diffhear[momloc]==2,FALSE),
        deafDad2 = ifelse(poploc2==0,FALSE,diffhear[poploc2]==2),
        deafMom2 = ifelse(momloc2==0,FALSE,diffhear[momloc2]==2),
        numParents=I(momloc>0)+I(poploc>0)+I(momloc2>0)+I(poploc2>0),
        deafParent=deafMom|deafDad|deafDad2|deafMom2,
        deafDad=deafDad|deafDad2,
        deafMom=deafMom|deafMom2,
        numAdults=sum(age>17),
        numDeafAdults=sum(age>17 & diffhear==2),
        anyDeafAdults=numDeafAdults>0,
        numDeafParents=deafDad+deafDad2+deafMom+deafMom2,
        twoDeafParents=numDeafParents>=2
         )

## just to check
mean(deafKids$pernum==deafKids$pn2)

deafKids <- filter(deafKids,diffhear==2,age<=17)

deafKids%>%group_by(age)%>%summarize(hasParent=mean(hasParent))%>%ggplot(aes(age,hasParent))+geom_point()+ylim(0,1)+geom_smooth()


### proportions deaf kids
kidEst <- rbind(
  `N deaf kids (0-17)`=svTot(deafKids,w1='perwt',wrep=paste0('repwtp',1:80)),
  `N deaf kids living with any parent(s)`=svTot(deafKids,'hasParent',w1='perwt',wrep=paste0('repwtp',1:80)),
  `N deaf kids living with 1 parent`=svTot(deafKids,'numParents==1',w1='perwt',wrep=paste0('repwtp',1:80)),
  `N deaf kids living with 2 parents`=svTot(deafKids,'numParents==2',w1='perwt',wrep=paste0('repwtp',1:80)),
  `N deaf kids living with any deaf parent(s)`=svTot(deafKids,'numDeafParents>0',w1='perwt',wrep=paste0('repwtp',1:80)),
  `% deaf kids living w any deaf parent (denominator: deaf kids living w a parent)`=estExpr(deafParent,hasParent,deafKids,w1name='perwt',wrepname=paste0('repwtp',1:80)),
  `N deaf kids living with 1 parent`=svTot(deafKids,'numDeafParents==1',w1='perwt',wrep=paste0('repwtp',1:80)),
  `% deaf kids living w 1 deaf parent (denominator: deaf kids living w any parents)`=estExpr(numDeafParents==1,hasParent,deafKids,w1name='perwt',wrepname=paste0('repwtp',1:80)),
  `N deaf kids living with 2 parents`=svTot(deafKids,'numDeafParents==2',w1='perwt',wrep=paste0('repwtp',1:80)),
  `% deaf kids living w 2 deaf parents (denominator: deaf kids living w a parent)`=estExpr(numDeafParents==2,hasParent,deafKids,w1name='perwt',wrepname=paste0('repwtp',1:80)),
  `N deaf kids living w at least 1 adult (18+)`=svTot(deafKids,'numAdults>0',w1='perwt',wrep=paste0('repwtp',1:80)),
  `N deaf kids living w at least 1 deaf adult (18+)`=svTot(deafKids,'anyDeafAdults',w1='perwt',wrep=paste0('repwtp',1:80)),
  `% deaf kids w at least 1 deaf adult (18+) (denominator: all deaf kids (0-17)`=estExpr(anyDeafAdults,sdat=deafKids,w1name='perwt',wrepname=paste0('repwtp',1:80))
  )

colnames(kidEst) <- c('Estimate','Std. Error','n')

write.csv(kidEst,'deafKidsWdeafParents.csv')

