for(p in unique(ids$pair)){
    print(p)
    for(sch in unique(subset(ids,pair==p)$schoolid2)){
        if(mean(Z[ids$schoolid2==sch])==1) cat(sch,' Trt\n')
        else cat(sch,' Ctrl\n')
    }
    cat('\n')
}

for(sch in unique(ids$schoolid2)) print(length(unique(ids$pair[ids$schoolid2==sch])))

print(1:10)