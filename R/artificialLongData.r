cat("\n####################################################################
####################### Class ArtificiaLongData ###########################
############################### Creation ###############################
####################################################################\n")

setClass(
    Class="ArtificialLongData",
    representation=representation(
        name="character",
        clusterNames="character",
        nbClusters="numeric",
        nbEachClusters="numeric",
        functionClusters="list",
        functionNoise="list",
        trajMeanTheo="matrix",
        trajMeanReal="matrix",
        percentOfMissing="numeric"
    ),
    prototype=prototype(
        name=character(),
        clusterNames=character(),
        nbClusters=numeric(),
        nbEachClusters=numeric(),
        functionClusters=list(),
        functionNoise=list(),
        trajMeanTheo=matrix(nrow=0,ncol=0),
        trajMeanReal=matrix(nrow=0,ncol=0),
        percentOfMissing=numeric()
    ),
    contain="ClusterizLongData"
)

    name="";clusterNames="";nbEachClusters=rep(50,3);
    functionClusters=list(function(t){t},function(t){0},function(t){-t});
    functionNoise=function(t){rnorm(1,0,1)};
    time=0:7;decimal=2;percentOfMissing=0

gald <- generateArtificialLongData <- function(
    name="",clusterNames="",nbEachClusters=50,
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    functionNoise=function(t,sdSeq){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0
){
    nbClusters <- max(length(nbEachClusters),length(functionNoise),length(functionClusters))
    if(length(nbEachClusters)==1){nbEachClusters<-rep(nbEachClusters,nbClusters)}else{}
    nbEachClusters <- sort(nbEachClusters,decreasing=TRUE)
    if(length(functionClusters)==1){functionClusters<-unlist(list(rep(list(functionClusters),nbClusters)))}else{}
    if(length(functionNoise)==1){functionNoise<-unlist(list(rep(list(functionNoise),nbClusters)))}else{}
    if(length(percentOfMissing)==1){percentOfMissing<- rep(percentOfMissing,nbClusters)}else{}
    nbTime <- length(time)
    id <- paste("I-",1:(sum(nbEachClusters)),sep="")
    indivInCluster <- rep(1:nbClusters,times=nbEachClusters)

    traj <- matrix(0,nrow=sum(nbEachClusters),ncol=nbTime)
    for (iIndiv in 1:nrow(traj)){
        traj[iIndiv,] <- functionClusters[[indivInCluster[iIndiv]]](time)+apply(t(time),2,functionNoise[[indivInCluster[iIndiv]]])
    }
    traj <- round(traj,digit=decimal)

    for (iCluster in 1:nbClusters){
        nbVal <- nbTime*nbEachClusters[iCluster]
        while(sum(is.na(traj[indivInCluster==iCluster,]))/nbVal <= percentOfMissing[iCluster]){
            traj[floor(runif(1,cumsum(c(0,nbEachClusters))[iCluster]+1,cumsum(nbEachClusters)[iCluster]+1)),floor(runif(1,1,nbTime+1))] <- NA
        }
    }

    trajMeanTheo <- trajMeanReal <- matrix(data=0,nrow=nbClusters,ncol=nbTime)
    for (iCluster in 1:nbClusters){trajMeanTheo[iCluster,] <- functionClusters[[iCluster]](time)}
    trajMeanObs <- as.matrix(aggregate(traj,by=list(indivInCluster),FUN=meanNA)[,-1])

    colnames(traj) <- colnames(trajMeanTheo) <- colnames(trajMeanReal) <- paste("V",time,sep="")
    rownames(traj) <- id
    rownames(trajMeanTheo) <- rownames(trajMeanReal) <- paste("Cluster-",1:nbClusters,sep="")

    ald <- new("ArtificialLongData",id=id,traj=traj,name=name,varName="V",clusterNames=clusterNames,nbClusters=nbClusters,nbEachClusters=nbEachClusters,time=time,
               functionClusters=functionClusters,functionNoise=functionNoise,trajMeanTheo=trajMeanTheo,trajMeanReal=trajMeanReal,percentOfMissing=percentOfMissing,trajSizeMin=2)
    ald@clusters$c1 <- list(clusterization(ald,partition(id=id,nbClusters=nbClusters,clusters=LETTERS[indivInCluster])))
    return(ald)
}
cleanProg(generateArtificialLongData,,,2) # meanNA LETTERS



