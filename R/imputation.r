cat("###################################################################
#################### Class LongData & Partition ###################
################### Imputations des manquantes ####################
###################################################################\n")

### LocfBegin
# Real name shoud be 'Fist Occurence Brings Toward...'

.LongData.LocfBegin <- function(traj){
    if(all(is.na(traj))){
        cat("impute:LocfBegin : there is only NA, impossible to impute\n")
        return(traj)
    }else{}
    NAinf <- which.min(is.na(traj))
    traj[1:NAinf]<-traj[NAinf]
    return(traj)
}
cleanProg(.LongData.LocfBegin,,,0)

.LongData.LocfMiddleEnd <- function(traj){
    if(all(is.na(traj))){
        cat("impute:LocfMiddleEnd : there is only NA, impossible to impute\n")
        return(traj)
    }else{
        if(is.na(traj[1])){stop("impute:LocfMiddleEnd : first value is NA; run LocfBegin first\n")}else{}
    }
    while(any(is.na(traj))){
        traj[which.max(is.na(traj))]<-traj[which.max(is.na(traj))-1]
    }
    return(traj)
}
cleanProg(.LongData.LocfMiddleEnd,,,0)

.LongData.Locf <- function(traj){
    return(.LongData.LocfMiddleEnd(.LongData.LocfBegin(traj)))
}
cleanProg(.LongData.Locf,,,0)



.LongData.InterpoLinBeginEnd <- function(traj){
    if(all(is.na(traj))){
        cat("impute:InterpoLinBeginEnd : there is only NA, impossible to impute\n")
        return(traj)
    }else{}
    lengthTraj <- length(traj)
    if(sum(!is.na(traj))==1){return(rep(na.omit(traj),length=lengthTraj))}else{}

    firstNoNA <- which.min(is.na(traj))
    lastNoNA <- lengthTraj-which.min(is.na(rev(traj)))+1
    a <- (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA)
    b <- traj[lastNoNA] - a*lastNoNA
    indNA <- c(1:firstNoNA,lastNoNA:lengthTraj)
    traj[indNA]<-a*indNA+b
    return(traj)
}
cleanProg(.LongData.InterpoLinBeginEnd,,,0)


.LongData.InterpoLinMiddle <- function(traj){
    if(all(is.na(traj))){
        cat("impute:InterpoLinMiddle : there is only NA, impossible to impute\n")
        return(traj)
    }else{
        if(is.na(traj[1])|is.na(traj[length(traj)])){
            stop("impute:InterpoLinMiddle : first or last value is NA; run InterpolBeginEnd first\n")
        }else{}
    }

    while(any(is.na(traj))){
        NAinfM <- which.max(is.na(traj))-1
        NAsupM <- which.min(is.na( traj[-(1:NAinfM)] )) + NAinfM
        traj[NAinfM:NAsupM] <- seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
    }
    return(traj)
}
cleanProg(.LongData.InterpoLinMiddle,,,0)


.LongData.InterpoLin <- function(traj){
    return(.LongData.InterpoLinMiddle(.LongData.InterpoLinBeginEnd(traj)))
}
cleanProg(.LongData.InterpoLin,,,0)


### copy
# paste the shape of the model
.LongData.copyBegin <- function(traj,model){
    if(all(is.na(traj))){
        cat("impute:copyBegin : there is only NA, impossible to impute\n")
        return(traj)
    }else{}
    NAinf <- which.min(is.na(traj))
    traj[1:NAinf]<-model[1:NAinf] + traj[NAinf]-model[NAinf]
    return(traj)
}
cleanProg(.LongData.copyBegin,,,0)

.LongData.copyEnd <- function(traj,model){
    if(all(is.na(traj))){
        cat("impute:copyEnd : there is only NA, impossible to impute\n")
        return(traj)
    }else{}
    return(rev(.LongData.copyBegin(rev(traj),rev(model))))
}
cleanProg(.LongData.copyEnd,,,0)

.LongData.copyMiddle <- function(traj,model){
    if(all(is.na(traj))){
        cat("impute:copyMiddle : there is only NA, impossible to impute\n")
        return(traj)
    }else{}
    if(any(is.na(model))){stop("impute:copyMiddle : there is NA in model, impossible to impute\n")}else{}
    while(any(is.na(traj))){
        NAinfM <- which.max(is.na(traj))-1
        NAsupM <- which.min(is.na( traj[-(1:NAinfM)] )) + NAinfM
        traj[NAinfM:NAsupM] <- (model[NAinfM:NAsupM]
            + seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
            - seq(from=model[NAinfM],to=model[NAsupM],length.out=NAsupM-NAinfM+1))
    }
    return(traj)
}
cleanProg(.LongData.copyMiddle,,,0)


.LongData.copy <- function(traj,model){
    return(.LongData.copyMiddle(.LongData.copyEnd(.LongData.copyBegin(traj,model),model),model))
}
cleanProg(.LongData.copy,,,0)


### Autre méthode : scaleShape
### Pour le centre, on fixe un facteur p, puis on pose d=(y_B'+p)/(y_B+p).
### ensuite, y_C'=y_D'+Delta C * d
### avec Delta C = y_C-y_D

### Pour les extrèmes, on considère les droites T' passant par y_A' et y_B' et T passant par y_A et y_B
### on calcule le  Delta C relativement a T, on l'ajoute a T' avec éventuellement un scale

###Autre méthode, dans le cas a <- c(NA,0,3,NA),  (m <- 1:4) on pourrait imputer en (-3,0,3,6)

### Plus précisément : on calcule les variations de la moyenne par rapport à une ligne moyenne qui irait tout droit
###   On ajoute ces variations a la ligne reliant InfNA et SupNA
### Une autre méthode est possible [MARCHE PAS] : calculer les variations par rapport a meanLine[infNA]
###   puis les normaliser en divisant par (meanLine[SupNA]-meanLine[InfNA])
###   puis les adapter a line en multipliant par (line[SupNA]-line[InfNA])
###   et enfin ajouter a line[InfNA]

#plot(line2,ylim=c(0,15),type="b",col="red",lwd=3)
#lines(meanLine,type="b",col="blue",lwd=2)
#line2imp <- .LongData.imputeOne(line2,meanLine)
#lines(line2imp-0.1,type="b",col="red",lty="dotted")

#plot(line2,ylim=c(0,15),type="b",col="red",lwd=3)
#lines(meanLine-1:9*2+15,type="b",col="green")
#line2imp2<-.LongData.imputeOne(line2,meanLine-1:9*2+10)
#lines(line2imp2+0.1,type="b",col="red",lty="dotted")
#rm(firstNA,lastNA,line,line2,line3,meanLine,line2imp,line2imp2)
### Fonctions moyenne, ecart type et which.min résistante aux NA.



#line <- c(NA,NA,2,NA,4,NA,2,NA,4,NA,NA,8,NA)



cat("### imputeLongData, methode ###\n")
#.Object <- ld2n#;partition <- p2b

.LongData.ImputeLongDataSansPart <- function(.Object,method){
    if(method=="LOCF"){
        return(t(apply(.Object@traj,1,.LongData.Locf)))
    }else{}
    if(method=="interpoLin"){
        return(t(apply(.Object@traj,1,.LongData.InterpoLin)))
    }else{}
    stop("Longdata(InputeLongData) : unknow method\n")
}
cleanProg(.LongData.ImputeLongDataSansPart,,,2) # .LongData.InterpoLin  .LongData.Locf
setMethod("imputeLongData",
    signature=c(.Object="LongData",method="ANY",yPartition="missing"),
    .LongData.ImputeLongDataSansPart
)


cat("### imputeLongData, methode ###\n")
#.Object <- ld2n;yPartition <- p2b
.LongData.ImputeLongDataAvecPart <- function(.Object,method,yPartition){
    clusters <- yPartition@clusters
    traj <- .Object@traj[!is.na(clusters),]
    clusters <- clusters[!is.na(clusters)]
    trajMeanObs <- aggregate(traj,by=list(clusters),FUN=meanNA)[,-1]
    trajMeanObs <- t(apply(trajMeanObs,1,.LongData.InterpoLin))
    trajMeanObs <- trajMeanObs[as.integer(clusters),]
    valAndMeans <- array(c(traj,trajMeanObs),dim=c(dim(traj),2))
    copyValAndMeans <- function(xy){.LongData.copy(xy[,1],xy[,2])}
    if(method=="copyMean"){
        return(t(apply(valAndMeans,1,copyValAndMeans)))
    }else{}
    stop("Longdata(InputeLongData) : unknow method\n")
}
cleanProg(.LongData.ImputeLongDataAvecPart,,,2) # .LongData.InterpoLin  .LongData.Locf
setMethod("imputeLongData",
    signature=c(.Object="LongData",method="ANY",yPartition="Partition"),
    .LongData.ImputeLongDataAvecPart
)

cat("### imputeLongData, methode ###\n")
#.Object <- ld2n@traj;yPartition <- p2b@clusters
.LongData.ImputeLongDataAvecPartBis <- function(.Object,method,yPartition){
    clusters <- yPartition
    traj <- .Object[!is.na(clusters),]
    clusters <- clusters[!is.na(clusters)]
    trajMeanObs <- aggregate(traj,by=list(clusters),FUN=meanNA)[,-1]
    trajMeanObs <- t(apply(trajMeanObs,1,.LongData.InterpoLin))
    trajMeanObs <- trajMeanObs[as.integer(clusters),]
    valAndMeans <- array(c(traj,trajMeanObs),dim=c(dim(traj),2))
    copyValAndMeans <- function(xy){.LongData.copy(xy[,1],xy[,2])}
    if(method=="copyMean"){
        return(t(apply(valAndMeans,1,copyValAndMeans)))
    }else{}
    stop("Longdata(InputeLongData) : unknow method\n")
}
cleanProg(.LongData.ImputeLongDataAvecPart,,,2) # .LongData.InterpoLin  .LongData.Locf
setMethod("imputeLongData",
    signature=c(.Object="matrix",yPartition="factor"),
    .LongData.ImputeLongDataAvecPartBis
)







