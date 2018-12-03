library(readr) ## read in the csvs faster
library(dplyr)
library(survey)
library(tibble)

source('../generalCode/estimationFunctions.r')

#(1) percentage of deaf/hearing in retail (general) << same as our employment report.
#(2) percentage of deaf/hearing in retail by category (if possible),
#(3) the education levels of (1) & (2)

## let's make our defintion of retail all the blokes that work at Whole Foods. So, 1-6.
## [1] "Retail Salespersons"
## [2] "First-Line Supervisors Of Retail Sales Workers"
## [3] "Cashiers"
## [4] "Stock Clerks And Order Fillers"
## [5] "Customer Service Representatives"
## [6] "Laborers And Freight, Stock, And Material Movers, Hand"

## asales [10:46 AM]
## good morning
## do they also need to work in a "retail" industry?
## for instance: customer service representative for a hospital
## i'd say that's not retail, right?

## Jeffrey [10:48 AM]
## yeah-- I agree w you

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
      retailINDP= (indp>=4670)&(indp<=5790),
      retailINDPint= (indpInt>=4670)&(indpInt<=5790),
      naicspPrefix=substr(naicsp,1,2),
      retailNAICSP=naicspPrefix%in%c('44','45','4M'),
      ## retail1= (socp>=411012) & (socp <= 412031),
      ## retailCat=factor(
      ##   ifelse(socp==411011,"First-Line Supervisors Of Retail Sales Workers",
      ##   ifelse(socp==411012,"First-Line Supervisors Of Non-Retail Sales Workers",
      ##     ifelse(socp==412010,"Cashiers",
      ##       ifelse(socp==412021,"Counter And Rental Clerks",
      ##         ifelse(412022,"Parts Salespersons",
      ##           ifelse(412031,"Retail Salespersons","Not Retail"))))))),
      ## retail=retailCat!="Not Retail",
      fulltime=(wkw==1 & wkhp>=35))
gc()

## get 3-letter categories and full labels for OCCP codes
## file "occp.tab" copied and pasted from data dictionary
## with some edting to make it more machine-readable
occp <- read.table('occp.tab',header=FALSE,sep='&',colClasses='character')
occp$cat <- substr(occp$V2,1,3)
dat$occpCat <- occp$cat[match(dat$occp,occp$V1)]
occp$lab <- substr(occp$V2,5,500)
dat$occpLab <- occp$lab[match(dat$occp,occp$V1)]

## top 20 occupations for people working in retail industry (in dataset, not in population)
head(names(sort(table(dat$occpLab[dat$retailINDP]),decreasing=TRUE)),20)

## top 6 occupations for people working in retail industry (in dataset):
top6 <- names(sort(table(dat$occpLab[dat$retailINDP]),decreasing=TRUE))[1:6]

dat <- mutate(dat,
  retailAny = (occpLab%in%top6)&(retailINDP),
  retailCat = ifelse(retailAny,occpLab,'n/a'))

#### estimation time

overall <-
  dat%>%group_by(deaf)%>%summarize(propRetail=100*svmean(retailAny,pwgtp),n=n())

byCat <- lapply(top6,
  function(x){
    dat$rcat <- dat$retailCat==x
    res <- dat%>%group_by(deaf)%>%summarize(xx=100*svmean(rcat,pwgtp))
    names(res)[names(res)=='xx'] <- x
    res
    })

tot <- rbind(n=overall$n,
  anyTop6retail=overall$propRetail,
  do.call('rbind',lapply(byCat,function(x) t(x[,2]))))

if(all(sapply(byCat,function(x) identical(x$deaf,overall$deaf)))){
  colnames(tot) <- overall$deaf
} else stop('deaf and hearing not lined up right')

## check sums
c(sum(tot[3:8,'deaf']),tot[2,'deaf'])
c(sum(tot[3:8,'hearing']),tot[2,'hearing'])

## full-time
overallFT <-
  dat%>%filter(fulltime)%>%group_by(deaf)%>%summarize(propRetail=100*svmean(retailAny,pwgtp),n=n())

byCatFT <- lapply(top6,
  function(x){
    dat$rcat <- dat$retailCat==x
    res <- dat%>%filter(fulltime)%>%group_by(deaf)%>%summarize(xx=100*svmean(rcat,pwgtp))
    names(res)[names(res)=='xx'] <- x
    res
    })

totFT <- rbind(n=overallFT$n,
  anyTop6retail=overallFT$propRetail,
  do.call('rbind',lapply(byCatFT,function(x) t(x[,2]))))

if(all(sapply(byCatFT,function(x) identical(x$deaf,overallFT$deaf)))){
  colnames(totFT) <- overall$deaf
} else stop('deaf and hearing not lined up right')

## check sums
c(sum(totFT[3:8,'deaf']),totFT[2,'deaf'])
c(sum(totFT[3:8,'hearing']),totFT[2,'hearing'])


### by education level


overallEd <-
  FIX(dat%>%group_by(deaf,retailAny)%>%do(edLevel=factorProps('attainCum',.)))

byCatEd <-
  FIX(dat%>%group_by(deaf,retailCat)%>%do(edLevel=factorProps('attainCum',.)))


## check "not retail"="n/a"
rbind(subset(overallEd,deaf=='deaf'&!retailAny)[,-c(1:2)],
  subset(byCatEd,deaf=='deaf'&retailCat=='n/a')[,-c(1:2)])

rbind(subset(overallEd,deaf=='hearing'&!retailAny)[,-c(1:2)],
  subset(byCatEd,deaf=='hearing'&retailCat=='n/a')[,-c(1:2)])

edres <-
  lapply(c('deaf','hearing'), function(x)
    cbind(t(overallEd%>%filter(deaf==x)%>%
              mutate(retail=ifelse(retailAny,'Any Top 6 Retail Jobs','Not top 6 retail jobs'))%>%
              select(-deaf,-retailAny)%>%column_to_rownames('retail')),
      t(byCatEd%>%filter(deaf=='deaf',retailCat!='n/a')%>%select(-deaf)%>%column_to_rownames('retailCat'))))

edres <- cbind(
  rbind(c('deaf',rep('',ncol(edres[[1]])-1)),
    colnames(edres[[1]]), round(edres[[1]],1)),
  rbind(c('hearing',rep('',ncol(edres[[2]])-1)),
    colnames(edres[[2]]), round(edres[[2]],1)))

edres <- edres[-grep('SE',rownames(edres)),]
rownames(edres) <- sub('X....','%>=',rownames(edres),fixed=TRUE)
rownames(edres) <- sub('X..','%',rownames(edres),fixed=TRUE)
colnames(edres) <- NULL

### full time
overallEdFT <-
  FIX(dat%>%filter(fulltime)%>%group_by(deaf,retailAny)%>%do(edLevel=factorProps('attainCum',.)))

byCatEdFT <-
  FIX(dat%>%filter(fulltime)%>%group_by(deaf,retailCat)%>%do(edLevel=factorProps('attainCum',.)))


## check "not retail"="n/a"
rbind(subset(overallEdFT,deaf=='deaf'&!retailAny)[,-c(1:2)],
  subset(byCatEdFT,deaf=='deaf'&retailCat=='n/a')[,-c(1:2)])

rbind(subset(overallEdFT,deaf=='hearing'&!retailAny)[,-c(1:2)],
  subset(byCatEdFT,deaf=='hearing'&retailCat=='n/a')[,-c(1:2)])

edresFT <-
  lapply(c('deaf','hearing'), function(x)
    cbind(t(overallEdFT%>%filter(deaf==x)%>%
              mutate(retail=ifelse(retailAny,'Any Top 6 Retail Jobs','Not top 6 retail jobs'))%>%
              select(-deaf,-retailAny)%>%column_to_rownames('retail')),
      t(byCatEdFT%>%filter(deaf=='deaf',retailCat!='n/a')%>%select(-deaf)%>%column_to_rownames('retailCat'))))

edresFT <- cbind(
  rbind(c('deaf',rep('',ncol(edresFT[[1]])-1)),
    colnames(edresFT[[1]]), round(edresFT[[1]],1)),
  rbind(c('hearing',rep('',ncol(edresFT[[2]])-1)),
    colnames(edresFT[[2]]), round(edresFT[[2]],1)))

edresFT <- edresFT[-grep('SE',rownames(edresFT)),]
rownames(edresFT) <- sub('X....','%>=',rownames(edresFT),fixed=TRUE)
rownames(edresFT) <- sub('X..','%',rownames(edresFT),fixed=TRUE)
colnames(edresFT) <- NULL

info <- data.frame(
  c('ACS 2012-2016 Data',
    'POPULATION:',
    'Ages 18-64',
    'Non-institutionalized',
    'Employed',
    '',
    '"Full Time" is defined as:',
    'Worked at least 50 weeks in past 12 months',
    'At least 35 hr/per week',
    '',
    'Retail categories are top 6 occupation codes for people who work in retail industry (as defined by INDP or NAICSP)',
    'These are:',
    top6))

### write everything to google sheet
openxlsx::write.xlsx(
  list(info=info,
    overall=tot,
    `full-time`=totFT,
    `ed. attainment`=edres,
    `ed. attainment full-time`=edresFT),
  file='retail.xlsx',
  rowNames=TRUE,colNames=TRUE,colWidth='auto')


