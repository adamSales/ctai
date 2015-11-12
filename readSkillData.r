setwd('data/data')
folders <- list.files()

dat <- read.csv(paste(folders[1],'/',list.files(folders[1])[1],sep=''))
dat <- dat[,-c(1,2,4:7)]

for(folder in folders){
    files <- list.files(folder)
    files <- files[!grepl('test',files,ignore.case=TRUE)]

    print(folder)

    for(f in files){
        if(folder==folders[1] & f==files[1]) next()
        newFile <- read.csv(paste(folder,'/',f,sep=''))

        unit <- newFile$unit[1]
        section <- newFile$section[1]

        newFile <- newFile[,-c(1,2,4:7)]
        names(newFile)[-1] <- paste(unit,section,names(newFile)[-1],sep='.')

        nfDup <- duplicated(newFile$student)
        newFile <- newFile[!nfDup,]

        dat <- merge(dat,newFile,by='student',all=TRUE)
        cat(f,' ',dim(dat),'\n')
    }
}
save(dat,file='datFull.RData')
#dat <- dat[,!grepl('.[xy]$',names(dat))]
save(dat,file='skillsDataYr2.RData')
dat$mastered <- apply(dat,1,function(row) sum(row>0.9 &row<=1,na.rm=TRUE)-1)
dat$incomplete <- apply(dat,1,function(row) sum(row<=0.9,na.rm=TRUE))
dat$total <- apply(dat[,-1],1,function(row) sum(row,na.rm=TRUE))

save(dat,file='skillsDataYr2.RData')
setwd('~/Dropbox/cognitiveTutor/')
