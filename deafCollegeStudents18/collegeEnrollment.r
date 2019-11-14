library(tidyverse)
library(openxlsx)
library(reshape2)
source('../generalCode/estimationFunctions.r')
states <- read.csv('../generalCode/states.csv')
jobs <- read.csv('../generalCode/occupations.csv')


varNames <- c('ST','AGEP','DEAR','SCHG','PWGTP',paste0('PWGTP',1:80))


ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))


datA <- read_csv('../../../data/byYear/ss18pusa.csv',col_types=colTypes)
datB <- read_csv('../../../data/byYear/ss18pusb.csv',col_types=colTypes)
## need: DEAR, attain, employment,PERNP, fulltime
dat <- rbind(datA[,varNames],datB[,varNames])

rm(datA,datB); gc()

names(dat) <- tolower(names(dat))

dat$state <- states$abb[match(dat$st,states$x)]

### only keep post-secondary students, schg=15 (college) or schg=16 (grad/prof school)
### UPDATE: only keep undergrad, schg=15
dat <-
  dat%>%
  filter(!is.na(schg),schg==15)%>%
  mutate(
    deaf=ifelse(dear==1,'deaf','hearing')#,
#    college=schg==15,
#    gradProf=schg==16
  )%>%
  select(-st,-dear,-schg)

## total college
ncollege <-
  dat%>%
  group_by(deaf,state)%>%
  group_map(~ {
    st <- svTot(.x)
    data.frame(est=st[1],se=round(st[2]))
  })%>%
  ungroup()%>%
  melt(id.vars=c('state','deaf'))%>%
  dcast(state~deaf+variable)

write.xlsx(ncollege,'collegeEnrollment2018.xlsx')

