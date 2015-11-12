# duplicates across years?
yrStuds <- list()
for(yr in 1:3)
    yrStuds[[yr]] <- unique(x$id[x$study.year==yr])
numStuds <- vapply(yrStuds,function(yr) sum(length(yr)),1)
sum(numStuds)
length(unique(x$id))
## 131 stuidents re-took (unless someone took 3 yrs...)
length(intersect(intersect(yrStuds[[1]],yrStuds[[2]]),yrStuds[[3]]))
#nope no 3-years

## if someone's in there multiple times, use only first showing
oneAndTwo <- intersect(yrStuds[[1]],yrStuds[[2]])
drop <- 1:nrow(x)
drop <- drop[x$id%in%oneAndTwo & x$study.year==2]
x <- x[-drop,]
length(unique(x$id)) #should =9696

twoAndThree <- intersect(yrStuds[[2]],yrStuds[[3]])
length(twoAndThree)
length(intersect(yrStuds[[1]],yrStuds[[3]])) #for completeness
### k problem solved
