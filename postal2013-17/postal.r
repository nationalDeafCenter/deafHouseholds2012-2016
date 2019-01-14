library(readr) ## read in the csvs faster
library(dplyr)
library(survey)
library(tibble)

source('../generalCode/estimationFunctions.r')


pVars <- c('DEAR','ST','AGEP','COW','SCHL','WKW','WKHP','SCH', 'OCCP','NAICSP','INDP','OCCP', 'SOCP','ESR','RELP','PWGTP',paste0('PWGTP',1:80))


firstTry <- read_csv('../../../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(tolower(names(firstTry))%in%tolower(pVars),
              ifelse(names(firstTry)%in%c('SOCP','OCCP','NAICSP','INDP'),'c','i'),'-')
missingVars <- setdiff(tolower(pVars),tolower(names(firstTry)[ccc!='-']))
if(length(missingVars)) cat('WARNING: Missing these variables:\n',missingVars,'\n')
ccc <- paste(ccc,collapse='')


dat <- lapply(c('a','b','c','d'),function(part)
  read_csv(paste0('../../../data/acs5yr2016/ss16pus',part,'.csv'),col_types=ccc)
)

varNames <- lapply(dat,names)
sameVarNames <- sapply(varNames,identical,varNames[[1]])
stopifnot(all(sameVarNames))

dat <- do.call('rbind',dat)

gc()

names(dat) <- tolower(names(dat))


edlevs <- c(
    '<Grade 10',
    'Grade 10',
    'Grade 11',
    '12th grade - no diploma',
    'Regular high school diploma',
    'GED or alternative credential',
    'Some college, but less than 1 year',
    '1 or more years of college credit, no degree',
    'Associates degree',
    'Bachelors degree',
    'Masters degree',
    'Professional degree beyond a bachelors degree',
    'Doctorate degree')

dat$attain <- ifelse(dat$schl<13,1,dat$schl-11)
dat$attain <- factor(edlevs[dat$attain],levels=edlevs,ordered=TRUE)

gc()

dat$indpInt <- parse_integer(dat$indp)

dat <- dat%>%filter(agep>17,agep<65,relp!=16,esr%in%c(1,2,4,5))%>% ## relp==16 for institutionalized, limit to employed
  mutate(
      deaf=factor(ifelse(dear==1,'deaf','hearing')),
      Age=ordered(ifelse(agep<35,'25-34',
        ifelse(agep<45,'35-44',
          ifelse(agep<55,'45-54','55-64')))),
      attainCum=ordered(
        ifelse(attain<'Regular high school diploma','No HS',
          ifelse(attain<'Some college, but less than 1 year','HS Diploma',
            ifelse(attain<'Associates degree','Some College',
              ifelse(attain<'Bachelors degree','Associates',
                ifelse(attain<'Masters degree','Bachelors','Post-Graduate'))))),
        levels=c('No HS','HS Diploma','Some College','Associates','Bachelors','Post-Graduate')),
      postal=occp%in%c(5540,5550,5560),
      postalCat=ifelse(occp==5540,'clerk',
          ifelse(occp==5550,'carrier',
                 ifelse(occp==5560,'sorter etc.','notPostal'))),

      fulltime=(wkw==1 & wkhp>=35))
gc()

cats <- c('clerk','carrier','sorter etc.')

#### estimation time

estEmploymentPercent <- function(dat){
  overall <-
    dat%>%group_by(deaf)%>%summarize(propPostal=100*svmean(postal,pwgtp),n=n())

  dat6 <- filter(dat,postal)

  byCat <- lapply(cats,
    function(x){
      dat6$rcat <- dat6$postalCat==x
      res <- dat6%>%group_by(deaf)%>%summarize(xx=svmean(rcat,pwgtp))
      names(res)[names(res)=='xx'] <- x
      res
    })

  ## percent of total
  tot <- rbind(n=overall$n,
    `% Postal Worker`=overall$propPostal,
    do.call('rbind',lapply(byCat,function(x) t(x[,2])*overall$propPostal)))

  ## check sums
  print(c(sum(tot[3:nrow(tot),1]),tot[2,1]))
  print(c(sum(tot[3:nrow(tot),2]),tot[2,2]))

  ## percent of all postal
  perOfPostal <- do.call('rbind',lapply(byCat,function(x) t(x[,2])))*100
  postaln <- dat6%>%group_by(deaf)%>%summarize(n=n())

  if(all(sapply(byCat,function(x) identical(x$deaf,overall$deaf)))){
    colnames(tot) <- overall$deaf
  } else stop('deaf and hearing not lined up right')
  if(identical(overall$deaf,postaln$deaf)){
    perOfPostal <- rbind(postaln$n,c(100,100),perOfPostal)
    colnames(perOfPostal) <- overall$deaf
  } else stop('deaf and hearing not lined up right')


  # combine
  tot <- round(cbind(tot,perOfPostal),1)
  rownames(tot)[-c(1:2)] <- paste('%',rownames(tot)[-c(1:2)])

  ## colnames
  tot <- rbind(c('% of Total','','% of all postal',''),
    colnames(tot),
    tot
  )

  tot
}

## overall
tot <- estEmploymentPercent(dat)

## full time
totFT <- estEmploymentPercent(filter(dat,fulltime))

### by education level

estEdLev <- function(dat){

  overallEd <-
    FIX(dat%>%group_by(deaf,postal)%>%do(edLevel=factorProps('attainCum',.)))

  byCatEd <-
    FIX(dat%>%group_by(deaf,postalCat)%>%do(edLevel=factorProps('attainCum',.)))


  ## check "not postal"="n/a"
  rbind(subset(overallEd,deaf=='deaf'&!postal)[,-c(1:2)],
    subset(byCatEd,deaf=='deaf'&postalCat=='notPostal')[,-c(1:2)])

  rbind(subset(overallEd,deaf=='hearing'&!postal)[,-c(1:2)],
    subset(byCatEd,deaf=='hearing'&postalCat=='notPostal')[,-c(1:2)])

  edres <-
    lapply(c('deaf','hearing'), function(x)
      cbind(t(overallEd%>%filter(deaf==x)%>%
                mutate(postal=ifelse(postal,'Any Postal Job','Not Postal'))%>%
                select(-deaf)%>%column_to_rownames('postal')),
        t(byCatEd%>%filter(deaf=='deaf',postalCat!='n/a')%>%select(-deaf)%>%column_to_rownames('postalCat'))))

  edres <- cbind(
    rbind(c('deaf',rep('',ncol(edres[[1]])-1)),
      colnames(edres[[1]]), round(edres[[1]],1)),
    rbind(c('hearing',rep('',ncol(edres[[2]])-1)),
      colnames(edres[[2]]), round(edres[[2]],1)))

  edres <- edres[-grep('SE',rownames(edres)),]
  rownames(edres) <- sub('X....','%>=',rownames(edres),fixed=TRUE)
  rownames(edres) <- sub('X..','%',rownames(edres),fixed=TRUE)
  colnames(edres) <- NULL

  edres
}

## employed
edres <- estEdLev(dat)

## employed full time
edresFT <- estEdLev(filter(dat,fulltime))


info <- data.frame(
  info=c('ACS 2012-2016 Data',
    'POPULATION:',
    'Ages 18-64',
    'Non-institutionalized',
    'Employed',
    '',
    '"Full Time" is defined as:',
    'Worked at least 50 weeks in past 12 months',
    'At least 35 hr/per week',
    '',
    'Postal categories are the types of postal jobs, as listed in OCCP',
    'These are:',
    cats))



### write everything to xlsx
openxlsx::write.xlsx(
  list(info=info,
    overall=tot,
    `full-time`=totFT,
    `ed. attainment`=edres,
    `ed. attainment full-time`=edresFT),
  file='postal.xlsx',
  rowNames=TRUE,colNames=FALSE,colWidth='auto')


