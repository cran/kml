### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################### Class ClustLongData ######################
############################## Creation ############################
####################################################################\n")

.ClusterLongData.validity <- function(object){
    validObject(as(object,"LongData"))
    validObject(as(object,"ListPartition"))
    return(TRUE)
}
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterLongData",
    contains=c("LongData","ListPartition"),
    validity=.ClusterLongData.validity
)

setMethod("clusterLongData",signature=c("missing","missing","missing","missing","missing","missing"),
    function(traj,idAll,time,timeInData,varNames,maxNA){new("ClusterLongData")}
)


##############################
### Code copier intégralement depuis "LongData.r"

### Data.frame ou array en 2D
.clusterLongData.constructor <- function(traj,idAll,time,timeInData,varNames,maxNA){

    ## First part : set all the parameters
    if(is.data.frame(traj)){
        if(missing(idAll)){
            idAll <- traj[,1]
            if(missing(timeInData)){
                timeInData <- 2:ncol(traj)
            }else{}
        }else{
            if(missing(timeInData)){
                timeInData <- 1:ncol(traj)
            }else{}
        }
        traj <- as.matrix(traj[,timeInData])
    }else{
        if(is.array(traj)){
            if(missing(idAll)){
                idAll <- paste("i",1:nrow(traj),sep="")
            }else{}
            if(missing(timeInData)){
                timeInData <- 1:ncol(traj)
            }else{}
            traj <- traj[,timeInData,drop=FALSE]
        }else{
            stop("[ClusterLongData:constructor]: 'traj' should be either a data.frame, a matrix or an array")
        }
    }
    if(missing(varNames)){varNames <- "V"}else{}
    if(missing(time)){time <- 1:ncol(traj)}else{}
    if(missing(maxNA)){maxNA <- ncol(traj)-2}else{}

    ## Second part : all the arguments are non-missing, the object can be build.

    ## X1 <- apply(traj,c(1,3),function(x){sum(is.na(x))}) compte le nombre de NA par indiv et par variable
    ## X2 <- t(X1)<=maxNA pour chaque ligne (ie chaque variable), indique TRUE si le nombre de NA est plus petit que le maxNA correspondant
    ## apply(X2,2,all) vérifie que la condition est bonne pour toutes les variables.

    keepId <- apply(t(apply(traj,1,function(x){sum(is.na(x))}))<=maxNA,2,all)

    ## Si on permet l'excusion globale, la formule est :
    ## keepId <- apply(traj,1,function(x)(sum(is.na(x))<=maxNA))
    traj <- traj[keepId,,drop=FALSE]
    idFewNA <- idAll[keepId]
    dimnames(traj) <- list(idFewNA,paste("t",time,sep=""))
    reverse <- matrix(c(0,1),2,1,dimnames=list(c("mean","sd"),varNames))
    return(new("ClusterLongData",
        idAll=as.character(idAll),
        idFewNA=as.character(idFewNA),
        time=time,
        varNames=varNames,
        traj=traj,
        dimTraj=dim(traj),
        maxNA=maxNA,
        reverse=reverse)
    )
}

setMethod("clusterLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),.clusterLongData.constructor)

cld <- clusterLongData


## ### Valable pour traj = matrix
## .ClusterLongData.constructor <- function(traj,idAll,time,varNames,maxNA=length(time)-2){
##     clustLD <- as(longData(...),"ClusterLongData")
##     clustLD["criterionActif"] <- "Calinski.Harabatz"
##     return(clustLD)
## }
## setMethod("clusterLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),.ClusterLongData.constructor)
## cld <- clusterLongData

## setGeneric("as.clusterLongData",function(data,...){standardGeneric("as.clusterLongData")})

## setMethod("as.clusterLongData","LongData",
##     function(data,criterionActif="calinski"){
##         data <- as(data,"ClusterLongData")
##         data["criterionActif"] <- criterionActif
##         return(data)
##     }
## )

## setMethod("as.clusterLongData","data.frame",
##     function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski"){
##         if(missing(idAll)){idAll <- data[,1]}else{}
##         if(missing(varNames)){varNames <- names(timeDataFrame)}else{}
##         if(missing(time)){time <- 1:length(timeDataFrame[[1]])}else{}
##         matr <- as.matrix(data[,na.omit(unlist(timeDataFrame))])
##         traj <- array(matr[,rank(unlist(timeDataFrame),na.last="keep")],c(length(idAll),length(time),length(varNames)))
##         return(clusterLongData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
##     }
## )

## setMethod("as.clusterLongData","array",
##     function(data,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski"){
##         if(missing(idAll)){idAll <- 1:dim(data)[1]}else{}
##         if(missing(varNames)){varNames <- paste("V",1:dim(data)[3],sep="")}else{}
##         if(missing(time)){time <- 1:dim(data)[2]}else{}
##         return(clusterLongData(traj=data,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
##     }
## )


## as.cld <- as.clusterLongData

#as.cld <- as.clusterLongData <- function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski"){
#    if(class(data)!="LongData"){
#        data <- as.longData(data=data,idAll=idAll,time=time,timeDataFrame=timeDataFrame,varNames=varNames,maxNA=maxNA)
#    }else{}
#    cLongData <- as(data,"ClusterLongData")
#    cLongData["criterionActif"] <- criterionActif
#    return(cLongData)
#}

#as.cld <- as.clusterLongData <- function(data,...){
#    if(class(data)!="LongData"){
#        data <- as.longData(data=data,...)
#    }else{}
#    cLongData <- as(data,"ClusterLongData")
#    cLongData["criterionActif"] <- "calinski"
#    return(cLongData)
#}



#.ClusterLongData.show <- function(object){
#    show(as(object,"LongData"))
#    cat("/n")
#    show(as(object,"ListPartition"))
#}


.ClusterLongData.show <- function(object){
    cat("   ~~~ Class: ClusterLongData ~~~")
    cat("\n      ~ Sub-Class: LongData ~ ")
    showLongData(as(object,"LongData"))
    cat("\n    ~ Sub-Class: ListPartition ~ ")
    showListPartition(as(object,"ListPartition"))
}
setMethod("show","ClusterLongData",.ClusterLongData.show)


cat("### Getteur ###\n")
.ClusterLongData.get <- function(x,i,j,drop){
    if(is.numeric(i)){
        stop("[ClusterLongData:getteur]: to get a clusters list, use ['ci']")
    }else{}
    if(i%in%c(CRITERION_NAMES,"criterionActif",CLUSTER_NAMES,"criterionValues","criterionValuesAsMatrix","sorted","initializationMethod")){
        x <- as(x,"ListPartition")
    }else{
        x <- as(x,"LongData")
    }
    return(x[i,j])
}
setMethod("[","ClusterLongData",.ClusterLongData.get)

### MET-ON clusterRank = 1 par défaut ?
getClusters <- function(xCld,nbCluster,clusterRank=1,asInteger=FALSE){
    cluster <-  xCld["idAll"] %in% xCld["idFewNA"]
    cluster[cluster] <- xCld[paste("c",nbCluster,sep="")][[clusterRank]]["clustersAsInteger"]
    cluster[!cluster] <- NA
    if(!asInteger){cluster <- factor(LETTERS[cluster])}else{}
    return(cluster)
}


getBestPostProba <- function(xCld,nbCluster,clusterRank=1){
    bestPP <-  xCld["idAll"] %in% xCld["idFewNA"]
    bestPP[!bestPP] <- NA
    bestPP[!is.na(bestPP)] <- apply(xCld[paste("c",nbCluster,sep="")][[clusterRank]]["postProba"],1,max,na.rm=TRUE)
    return(bestPP)
}



cat("### Setteur ###\n")
### Héritage direct de ListPartition puisque set n'est pas défini pour LongData
### ATTENTION !!! Normalement, il faudrait vérifier que la partition est de la BONNE TAILLE !!!

.ClusterLongData.set <- function(x,i,j,...,value){
    if(i=="add"){
        if(length(value["clusters"])!=x["nbIdFewNA"]){
            stop("[ClusterLongData:set] the lenght of the Partition should be the same than 'idFewNA'")
        }else{}
    }
    callNextMethod(x, i, j,..., value=value)
}
setReplaceMethod("[","ClusterLongData",.ClusterLongData.set)



cat("\n####################################################################
######################### Class ClustLongData ######################
############################### Autres #############################
####################################################################\n")


### On a un cld et un num, on plot le longData et la Partition qui va avec.
.plot.clusterLongData.num <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=200,...){
#    if(class(y[1])=="character"){
 #       y[1] <- substr(y[1],2,3)
  #      y <- as.numeric(y)
   # }else{}
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[paste('c',y[1],sep="")][[y[2]]]
    plotTraj(x=as(x,"LongData"),y=yPartition,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
    return(invisible())
}
#setMethod("plot",signature=c("ClusterLongData","ANY"),.clusterLongData.num.plot)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData
###  - soit il n'est pas calculable et on appelle plot.LongData.num
.plot.clusterLongData.missingY <- function(x,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=200,...){
    if(all(is.tna(x["criterionValues"]))){
        plotTraj(x=as(x,"LongData"),parTraj=parTraj,parWin=parWin,nbSample=nbSample,...)
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
        .plot.clusterLongData.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
    }
    return(invisible())
}

##setMethod("plot",signature=c("ClusterLongData","missing"),.clusterLongData.plot)
.plotAll <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=1000,toPlot=c("both"),
                     criterion=x["criterionActif"],nbCriterion=100,standardized = FALSE,...){
    switch(EXPR=toPlot,
           "both"={
               listScreen <- split.screen(matrix(c(0,0.3,0.3,1,0,0,1,1),2))
               screen(listScreen[2])
               parSubWindows <- parWin
               parSubWindows['closeScreen']<-TRUE
               if(missing(y)){
                   .plot.clusterLongData.missingY(x,parTraj=parTraj,parMean=parMean,parWin=parSubWindows,nbSample=nbSample,...)
               }else{
                   .plot.clusterLongData.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parSubWindows,nbSample=nbSample,...)
               }
               screen(listScreen[1])
               ## ??? Liste des arguments a vérifier
               plotCriterion(as(x,"ListPartition"),criterion=criterion,nbCriterion=nbCriterion,standardized=standardized)

               if(parWin['closeScreen']){
                    close.screen(listScreen)
                   return(invisible())
               }else{
                   return(listScreen)
               }
           },
           "traj"={
               if(missing(y)){
                   .plot.clusterLongData.missingY(x,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
               }else{
                   .plot.clusterLongData.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
               }
           },
           "criterion"={
               plotCriterion(as(x,"ListPartition"),criterion=criterion,nbCriterion=nbCriterion,standardized=standardized)
           }
    )
}
setMethod("plot",signature=c("ClusterLongData","missing"),.plotAll)
setMethod("plot",signature=c("ClusterLongData","numeric"),.plotAll)
setMethod("plot",signature=c("ClusterLongData","Partition"),function(x,y,...){plotTraj(x,y,...)})



gald <- generateArtificialLongData <- function(
    nbEachClusters=50,time=0:10,varNames="V",
    meanTrajectories=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    personalVariation=function(t){rnorm(1,0,2)},
    residualVariation=function(t){rnorm(1,0,2)},
    decimal=2,percentOfMissing=0
){
    nbClusters <- length(meanTrajectories)
    if(length(nbEachClusters)==1){nbEachClusters <- rep(nbEachClusters,nbClusters)}else{}
    if(is.numeric(personalVariation)){eval(parse(text=paste("personalVariation <- function(t){rnorm(1,0,",personalVariation,")}",sep="")))}else{}
    if(length(personalVariation)==1){personalVariation <- rep(list(personalVariation),nbClusters)}else{}
    if(is.numeric(residualVariation)){eval(parse(text=paste("residualVariation <- function(t){rnorm(1,0,",residualVariation,")}",sep="")))}else{}
    if(length(residualVariation)==1){residualVariation <- rep(list(residualVariation),nbClusters)}else{}
    if(length(percentOfMissing)==1){percentOfMissing <- rep(percentOfMissing,nbClusters)}else{}
    nbTime <- length(time)
    idAll <- paste("i",1:(sum(nbEachClusters)),sep="")
    indivInCluster <- rep(1:nbClusters,times=nbEachClusters)

    traj <- matrix(NA,nrow=sum(nbEachClusters),ncol=nbTime)
    for (iIndiv in 1:nrow(traj)){
        traj[iIndiv,] <- meanTrajectories[[indivInCluster[iIndiv]]](time)+
                         personalVariation[[indivInCluster[iIndiv]]](time)+
                         apply(t(time),2,residualVariation[[indivInCluster[iIndiv]]])
    }
    traj <- round(traj,digits=decimal)


    for (iCluster in 1:nbClusters){
        nbVal <- nbTime*nbEachClusters[iCluster]
        while(sum(is.na(traj[indivInCluster==iCluster,]))/nbVal < percentOfMissing[iCluster]){
            randL <- floor(runif(1,cumsum(c(0,nbEachClusters))[iCluster]+1,cumsum(nbEachClusters)[iCluster]+1))
            randC <- floor(runif(1,1,nbTime+1))
            if(sum(!is.na(traj[randL,]))>1){traj[randL,randC]<-NA}else{}
        }
    }

    return(clusterLongData(traj,idAll=idAll,time=time,varNames=varNames))
}

cat("\n--------------------------------------------------------------------
------------------------- Class ClustLongData ----------------------
--------------------------------- Fin ------------------------------
--------------------------------------------------------------------\n")
