library(scales)
library(readr)
library(dplyr)
library(ggplot2)
source('../generalCode/estimationFunctions.r')


pVars <- c('agep','dear','sex','pwgtp',paste0('pwgtp',1:80))

firstTry <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',n_max=10)

ct <- ifelse(tolower(names(firstTry))%in%pVars,'i','-')
print(table(ct))
print(names(firstTry)[ct!='-'])
ct <- paste(ct,collapse='')

pdat <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',col_types=ct)
str(pdat)

pdat <- rbind(
    pdat,
    read_csv('../../../data/acs5yr2016/ss16pusb.csv',col_types=ct),
    read_csv('../../../data/acs5yr2016/ss16pusc.csv',col_types=ct),
    read_csv('../../../data/acs5yr2016/ss16pusd.csv',col_types=ct))

names(pdat) <- tolower(names(pdat))

#pdat$agep <- as.factor(pdat$agep)

fun <- function(.data,...){
  propDeaf=estSEstr('dear==1',sdat=.data)
  nDeaf=svTot(.data,'dear==1')
  data.frame(
    percent=propDeaf['est']*100,
    propSE=propDeaf['se']*100,
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
                          deafByAgeCategories=estsSex),
                     file='PercentDeafByAge.xlsx')


estsSex <- pdat%>%group_by(sex,agep)%>%do(propDeaf=estSEstr('dear==1',sdat=.))
es2 <- sapply(estsSex[,3][[1]],function(x) x)
rownames(es2) <- c('est','se','n')
estsSex <- cbind(estsSex[,1:2],t(es2))

save(ests,estsSex,file='deafPropByAge.RData')


ggplot(ests,aes(Age,est))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=est-2*se,ymax=est+2*se),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2012-2016')
ggsave('percentDeafByAge.pdf')

ggplot(filter(ests,Age<65),aes(Age,est))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=est-2*se,ymax=est+2*se),width=0)+
    geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2012-2016')
ggsave('percentDeafByAge0-64.pdf')

ggplot(filter(ests,Age<45),aes(Age,est))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=est-2*se,ymax=est+2*se),width=0)+
    geom_smooth(se=FALSE)+
    ylab('% Deaf')+
    scale_y_continuous(labels=percent)+
    ggtitle('% Deaf By Age','ACS 2012-2016')
ggsave('percentDeafByAge0-44.pdf')

estsSex$Age=as.numeric(as.character(estsSex$agep))
ggplot(estsSex,aes(Age,est,color=as.factor(sex)))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=est-2*se,ymax=est+2*se),width=0)+
    #geom_smooth(se=FALSE)+
    ylab('% Deaf')+xlab('Age')+labs(color='Sex')+
    scale_y_continuous(labels=percent)+
    scale_color_discrete(breaks=c(1,2),labels=c('Male','Female'))+
    theme(legend.position='top')+
    ggtitle('% Deaf By Age & Sex','ACS 2012-2016')
ggsave('percentDeafByAgeSex.pdf')

ggplot(filter(estsSex,Age<65),aes(Age,est,color=as.factor(sex)))+
    geom_point()+
    geom_errorbar(mapping=aes(ymin=est-2*se,ymax=est+2*se),width=0)+
    geom_smooth(se=FALSE)+
    ylab('% Deaf')+xlab('Age')+labs(color='Sex')+
    scale_y_continuous(labels=percent)+
    scale_color_discrete(breaks=c(1,2),labels=c('Male','Female'))+
    theme(legend.position='top')+
    ggtitle('% Deaf By Age & Sex','ACS 2012-2016')
ggsave('percentDeafByAgeSex0-64.pdf')

ests[,1:2] <- round(ests[,1:2]*100,1)
names(ests)[1:2] <- c('% Deaf','SE')

estsSex$Age <- NULL
estsSex[,3:4] <- round(estsSex[,3:4]*100,1)
names(estsSex)[2:4] <- c('Age','% Deaf','SE')
estsSex$sex <- c('Male','Female')[estsSex$sex]

openxlsx::write.xlsx(list(deafByAge=ests,
                          deafByAgeAndSex=estsSex),
                     file='PercentDeafByAge.xlsx')


N <- 1000#sum(ests$n)
increase <- data.frame(
    Age=1:max(ests$Age),
    ndeaf=ests$est[-1]*N)
increase$nmore=increase$ndeaf-ests$est[-nrow(ests)]*N


### look at logs
ggplot(filter(ests,Age<65,Age>1),aes(Age,log(est)))+
  geom_point()+
  geom_smooth(data=filter(ests,Age>1,Age<38),method='lm')+
  geom_smooth(data=filter(ests,Age>36,Age<65),method='lm')+
  ylab('Log(Proportion Deaf)')


estsSex$Sex=c('Male','Female')[estsSex$sex]
ggplot(filter(estsSex,Age<65,Age>1),aes(Age,log(est),color=Sex,group=Sex))+
  geom_point()+
  geom_smooth(data=filter(estsSex,Age>1,Age<38),method='lm')+
  geom_smooth(data=filter(estsSex,Age>36,Age<65),method='lm')+
  ylab('Log(Proportion Deaf)')+
  geom_vline(xintercept=37,linetype='dotted')

