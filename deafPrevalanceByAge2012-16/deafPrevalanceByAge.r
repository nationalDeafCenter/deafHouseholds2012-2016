library(scales)
library(readr)
library(dplyr)
library(ggplot2)
source('../generalCode/estimationFunctions.r')


pVars <- c('agep','dear','sex','pwgtp','adjinc',paste0('pwgtp',1:80))

firstTry <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',n_max=10)

ct <- ifelse(tolower(names(firstTry))%in%pVars,'i','-')
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

#pdat$agep <- as.factor(pdat$agep)

fun <- function(.data,...){
  propDeaf=estSEstr('dear==1',sdat=.data)
  nDeaf=svTot(.data,'dear==1')
  data.frame(
    proportion=propDeaf['est'],
    propSE=propDeaf['se'],
    totalNumber=nDeaf[1],
    numSE=nDeaf[2],
    n=propDeaf['n']
  )
}

ests <- pdat%>%
  group_by(agep)%>%
  group_map(fun)

## by age categories
##0-4; 5-18; 19-34; 35-64; 65+
estsCat <- pdat%>%
  mutate(ageCat=cut(agep,c(0,5,19,35,65,100),include.lowest=TRUE,right=FALSE))%>%
  group_by(ageCat)%>%
  group_map(fun)

openxlsx::write.xlsx(list(deafByAge=ests,
                          deafByAgeCategories=estsCat),
                     file='deafByAge.xlsx')

estsSex <- pdat%>%
  group_by(sex,agep)%>%
  group_map(fun)


save(ests,estsCat,estsSex,file='deafPropByAge.RData')




ggplot(ests,aes(agep,proportion))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=proportion-2*propSE,ymax=proportion+2*propSE),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2013-2017')
ggsave('percentDeafByAge.pdf')

ggplot(filter(ests,agep<65),aes(agep,proportion))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=proportion-2*propSE,ymax=proportion+2*propSE),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2013-2017')
ggsave('percentDeafByAge0-64.pdf')

ggplot(filter(ests,agep<45),aes(agep,proportion))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=proportion-2*propSE,ymax=proportion+2*propSE),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2013-2017')
ggsave('percentDeafByAge0-44.pdf')

ggplot(ests,aes(agep,totalNumber,ymin=totalNumber-2*numSE,ymax=totalNumber+2*numSE))+
  geom_point()+
  geom_errorbar(width=0)+
  ylab('Number Deaf')+
  xlab('Age')+
  scale_y_continuous(breaks=seq(0,300000,50000),labels=function(x) paste0(round(x/1000),'K'))+
  ggtitle('Number Deaf byAge','ACS 2013-2017')

estsSex$agep=as.numeric(as.character(estsSex$agep))
ggplot(estsSex,aes(agep,proportion,color=as.factor(sex)))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=proportion-2*propSE,ymax=proportion+2*propSE),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+xlab('Age')+labs(color='Sex')+
    scale_y_continuous(labels=percent)+
    scale_color_discrete(breaks=c(1,2),labels=c('Male','Female'))+
    theme(legend.position='top')+
    ggtitle('% Deaf By Age & Sex','ACS 2013-2017')
ggsave('percentDeafByAgeSex.pdf')

ggplot(filter(estsSex,agep<65),aes(agep,proportion,color=as.factor(sex)))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=proportion-2*propSE,ymax=proportion+2*propSE),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+xlab('agep')+labs(color='Sex')+
    scale_y_continuous(labels=percent)+
    scale_color_discrete(breaks=c(1,2),labels=c('Male','Female'))+
    theme(legend.position='top')+
    ggtitle('% Deaf By Age & Sex','ACS 2013-2017')
ggsave('percentDeafByAgeSex0-64.pdf')

## ests[,1:2] <- round(ests[,1:2]*100,1)
## names(ests)[1:2] <- c('% Deaf','SE')

## estsSex$Age <- NULL
## estsSex[,3:4] <- round(estsSex[,3:4]*100,1)
## names(estsSex)[2:4] <- c('Age','% Deaf','SE')
estsSex$sex <- c('Male','Female')[estsSex$sex]

openxlsx::write.xlsx(list(deafByAge=ests,
                          deafByAgeAndSex=estsSex),
                     file='PercentDeafByAge.xlsx')


## N <- 1000#sum(ests$n)
## increase <- data.frame(
##     Age=1:max(ests$Age),
##     ndeaf=ests$est[-1]*N)
## increase$nmore=increase$ndeaf-ests$est[-nrow(ests)]*N


### look at logs
ggplot(filter(ests,agep<65,agep>1),aes(agep,log(proportion)))+
  geom_point()+
  geom_errorbar(mapping=aes(ymin=log(proportion-2*propSE),ymax=log(proportion+2*propSE)),width=0)+
  geom_smooth(data=filter(ests,agep>1,agep<39),method='lm')+
  geom_smooth(data=filter(ests,agep>37,agep<65),method='lm')+
  ylab('Log(Proportion Deaf)')
ggsave('LogProportionDeafByAge.jpg')

estsSex$Sex=c('Male','Female')[estsSex$sex]
ggplot(filter(estsSex,agep<65,agep>1),aes(agep,log(proportion),color=Sex,group=Sex))+
  geom_point()+
  geom_errorbar(mapping=aes(ymin=log(proportion-2*propSE),ymax=log(proportion+2*propSE)),width=0)+
  geom_smooth(data=filter(estsSex,agep>1,agep<39),method='lm')+
  geom_smooth(data=filter(estsSex,agep>37,agep<65),method='lm')+
  ylab('Log(Proportion Deaf)')+
  geom_vline(xintercept=38,linetype='dotted')
ggsave('LogProportionDeafByAgeSex.jpg')


### does it depend on the year?
estYear <- pdat%>%
  filter(agep<65,agep>1)%>%
  mutate(deaf=dear==1)%>%
  group_by(agep,adjinc)%>%
  summarize(proportion=svmean(deaf,pwgtp))#%>%

ggplot(estYear,aes(agep,log(proportion)))+#,color=as.factor(adjinc),group=as.factor(adjinc)))+
  geom_point()+
  geom_smooth(data=filter(estYear,agep>1,agep<39),method='lm')+
  geom_smooth(data=filter(estYear,agep>37,agep<65),method='lm')+
  facet_wrap(~adjinc)
