units=function(sid,dat1){
#    dat1 <- x[x$id==sid,,drop=FALSE]
    units1 <- unique(dat1$unit)
    units <- vapply(levels(dat1$unit),
                 function(un) is.element(un,units1),
                 logical(1))
    c(units,numUnit=sum(units))
}

sections <- function(sid,dat1){
    sections1 <- unique(dat1$section)
    sum(sections1%in%levels(dat1$section))
}







studDat <- split(x,x$RAND_field_id)
oneYr <- vapply(x$RAND_field_id,function(id) length(unique(x$study.year[x$RAND_field_id==id])),1)
x <- x[oneYr==1,]
xm <- melt(x,id.vars=c(1:4))
xLong <- x
x <- dcast(xm,RAND_field_id+study.year+RAND_school~variable)






h2 <- read.csv('H2_algebra_rcal_20121119_fieldid.csv',row.names=1)

h2cov <- h2[,1:21]
h2out <- h2[,21:25]



