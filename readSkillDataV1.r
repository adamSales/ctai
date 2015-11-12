files <- list.files('data/data/anon_2008r1_section_skills')
files <- files[!grepl('test',files,ignore.case=TRUE)]


dat <- read.csv(paste('data/data/anon_2008r1_section_skills/',files[1],sep=''))
dat <- dat[,-c(1,2,4:7)]

for(f in files[2:5]){
    newFile <- read.csv(
        paste('data/data/anon_2008r1_section_skills/',f,sep=''))
    unit <- newFile$unit[1]
    section <- newFile$section[1]

    newFile <- newFile[,-c(1,2,4:7)]
    names(newFile)[-1] <- paste(unit,section,names(newFile)[-1],sep='.')

    nfDup <- duplicated(newFile$student)
    newFile <- newFile[!nfDup,]

    dat <- merge(dat,newFile,by='student',all=TRUE)
    cat(f,' ',dim(dat),'\n')
}


save(dat,file='datFull2.RData')
#dat <- dat[,!grepl('.[xy]$',names(dat))]
save(dat,file='skillsDataYr22.RData')
dat$mastered <- apply(dat,1,function(row) sum(row>0.9 &row<=1,na.rm=TRUE)-1)
dat$incomplete <- apply(dat,1,function(row) sum(row<=0.9,na.rm=TRUE))
dat$total <- apply(dat[,-1],1,function(row) sum(row,na.rm=TRUE))

save(dat,file='skillsDataYr22.RData')

skillDat <- dat
load('model1dat.RData')
usage <- merge(usage,skillDat[,c('student','mastered','incomplete','total')],by.x='id',by.y='student',all.x=TRUE)

save(covs ,    usage  ,  dat   ,   outcomes, covsImp,  ids ,file='model1dat.RData')
