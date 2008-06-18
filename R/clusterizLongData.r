cat("\n#######################################################################
######################## Class ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

.ClusterizLongData.validity <- function(object){
    cat("**** validity ClusterizLongData ****\n")
    validObject(as(object,"LongData"))
    return(TRUE)
}
cleanProg(.ClusterizLongData.validity,,,0)
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterizLongData",
    representation=representation(
        clusterizList="list"
    ),
    contains="LongData",
    prototype=prototype(
        clusterizList=list(c2=list(),c3=list(),c4=list(),c5=list(),c6=list(),c7=list(),c8=list(),c9=list(),c10=list())
    ),
    validity=.ClusterizLongData.validity
)
rm(.ClusterizLongData.validity)


cat("\n###################################################################
##################### Class ClusterizLongData #####################
########################### Constructeur ##########################
###################################################################\n")

cld <- clusterizLongData <- function(values){new("ClusterizLongData",values)}

as.cld <- as.clusterizLongData <- function(data,...){
    return(clusterizLongData(as.longData(data,...)))
}


cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################ Accesseurs ###########################
################################################################## #\n")


setGenericVerif("addClusterization",function(.Object,xClusterization)standardGeneric("addClusterization"))
.clusterizLongData.addClusterization <- function(.Object,xClusterization){
    nameObject<-deparse(substitute(.Object))
    listIndex <- paste("c",xClusterization@nbClusters,sep="")
    .Object@clusterizList[[listIndex]] <- c(.Object@clusterizList[[listIndex]],list(xClusterization))
    lengthList <- length(.Object@clusterizList[[listIndex]])
    names(.Object@clusterizList[[listIndex]])[lengthList] <- lengthList
    .Object@clusterizList[[listIndex]] <- .Object@clusterizList[[listIndex]][
        order(unlist(lapply(.Object@clusterizList[[listIndex]],getCalinskiCriterion)),decreasing=TRUE)
    ]
    assign(nameObject,.Object,envir=parent.frame())
    return(invisible())
}
cleanProg(.clusterizLongData.addClusterization,,,1) # getCalinskiCriterion
setMethod("addClusterization","ClusterizLongData",.clusterizLongData.addClusterization)
rm(.clusterizLongData.addClusterization)

setGenericVerif("addPartition",function(.Object,yPartition)standardGeneric("addPartition"))
.clusterizLongData.addPartition <- function(.Object,yPartition){
    nameObject<-deparse(substitute(.Object))
    addClusterization(.Object,clusterization(xLongData=as(.Object,"LongData"),yPartition=yPartition))
    assign(nameObject,.Object,envir=parent.frame())
    return(invisible())
}
cleanProg(.clusterizLongData.addPartition,,,0)
setMethod("addPartition","ClusterizLongData",.clusterizLongData.addPartition)
rm(.clusterizLongData.addPartition)

### Si y n'a qu'une coordonnée, c'est le nombre de cluster. Dans ce cas, on lui donne '1' pour deuxième coordonnée
### Si y est absent
###   - S'il n'y a aucun Calinski, on affiche longData avec moyenne si moyenne=black ou color.
###   - S'il y a des Calinski, on lui donne le plus grand


cat("### getClusterizList ###\n")
setGenericVerif("getClusterizList",function(object,nbClusters,clusterNumber){standardGeneric("getClusterizList")})
setMethod("getClusterizList",signature=c("ClusterizLongData","missing","missing"),function(object,nbClusters){return(object@clusterizList)})

setMethod("getClusterizList",signature=c("ClusterizLongData","numeric","missing"),
    function(object,nbClusters){return(object@clusterizList[[paste("c",nbClusters,sep="")]])}
)
setMethod("getClusterizList",signature=c("ClusterizLongData","numeric","numeric"),
    function(object,nbClusters,clusterNumber){return(object@clusterizList[[paste("c",nbClusters,sep="")]][[clusterNumber]])}
)

setGenericVerif("getCalinskiCriterion",function(object){standardGeneric("getCalinskiCriterion")})
.clusterizLongData.getCalinski <- function(object){
    ncolMatrix = max(c(unlist(lapply(object@clusterizList,length)),1))
    calinskiCriterionList <- matrix(NA,nrow=9,ncol=ncolMatrix,dimnames=list(paste("c",2:10,sep=""),list()))
    for(i in 1:9){
        calLine <-lapply(object@clusterizList[[i]],getCalinskiCriterion)
        calinskiCriterionList[i,] <- c(unlist(calLine),rep(NA,ncolMatrix-length(calLine)))
    }
    class(calinskiCriterionList) <- c("Criterion",class(calinskiCriterionList))
    return(calinskiCriterionList)
}
cleanProg(.clusterizLongData.getCalinski,,,2)  # getCalinskiCriterion length
setMethod("getCalinskiCriterion","ClusterizLongData",.clusterizLongData.getCalinski)
rm(.clusterizLongData.getCalinski)



cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################ Affichage ############################
###################################################################\n")

cat("### Method: 'show' pour ClusterizLongData ###\n")
.clusterizLongData.show <- function(object){
    cat("*** show(ClusterizLongData) **\n")
    show(as(object,"LongData"))
    cat("Calinski Criterion of clusterizList\n")
    toPrint <- getCalinskiCriterion(object)
    if(ncol(toPrint)>10){
        toPrint <- as.data.frame(toPrint[,1:10])
        toPrint$more <- "..."
        print(toPrint)
    }else{
        print(toPrint)
    }
    cat("******** End of show *********\n")
    return(invisible(object))
}
cleanProg(.clusterizLongData.show,,,0)
setMethod("show","ClusterizLongData",.clusterizLongData.show)
rm(.clusterizLongData.show)


cat("### Method: 'print' pour ClusterizLongData ###\n")
.clusterizLongData.print <- function(x,...){
    cat("*** print(ClusterizLongData) **\n")
#        if(length(x@id)==0){
 #           cat("* empty ClusterizLongData\n")
  #      }else{
    print(as(x,"LongData"))
    for(iCluster in paste("c",2:10,sep="")){
        lapply(x@clusterizList[iCluster],print)
    }
#}
    cat("******** End of print *********\n")
    return(invisible(x))
}
cleanProg(.clusterizLongData.print,,,1)  # print
setMethod("print","ClusterizLongData",.clusterizLongData.print)
rm(.clusterizLongData.print)


plot.Criterion <- function(x,...){
    if(!all(is.na(x))){
        matplot(1:ncol(x),t(rbind(NA,x)),type="b",xlab="",ylab="",main="Calinski Criterion")
    }else{
        plot(1,main="All Calinski criterion are undefined",type="n")
    }
    return(invisible())
}
cleanProg(plot.Criterion,,,0)

cat("### Method: 'plot' pour clusterizLongData ###\n")
.clusterizLongData.plot <- function(x,y,color=c("color","color","color"),main="",calinski=FALSE,...){
    xLongData <- as(x,"LongData")
    color <-  match.arg(color,choices=c("color","no","black"),TRUE)
    calMatrix <- getCalinskiCriterion(x)
    if(missing(y)){
        if(all(.is.tna(calMatrix))){#only true NA => no cluster define => plot without subgroups
            plot(xLongData,mean=(color[1]%in%c("back","color")),main=main,...)
            return(invisible())
        }else{
            y <- c(  max( c(which.min(-calMatrix[,1]),1),na.rm=TRUE  ),1)
        }
    }else{
        if(length(y)==1){y <- c(y-1,1)}else{y[1]<-y[1]-1}
    }

    if(calinski){
        dev.set(3)
        if(!3%in%dev.list()){
            if(.Platform$OS.type=="windows"){windows(5,5,xpos=0)}else{x11(xpos=1)}
        }else{}
        plot(calMatrix)
        points(y[2],calMatrix[y[1],y[2]],lwd=7,pch=16)
    }else{}
    dev.set(2)
    if(.is.tna(calMatrix[y[1],y[2]])){
        layout(1)
        plot(1,main=paste("No partition with",y[1]+1,"clusters"))
    }else{
        mainTitle <- paste("Part: ",y[2]," Calinski=",formatC(calMatrix[y[1],y[2]]))
        clust <- paste("c",y[1]+1,sep="")
        plot(xLongData,x@clusterizList[[clust]][[y[2]]],color=color,main=mainTitle)
    }
    bringToTop(2)
}
cleanProg(.clusterizLongData.plot,,,0)
setMethod("plot","ClusterizLongData",.clusterizLongData.plot)
rm(.clusterizLongData.plot)



cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")

cat("### Method: 'choice' pour clusterizLongData ###\n")
setGenericVerif("choice",function(x){standardGeneric("choice")})
.clusterizLongData.choice <- function(x){
    calMatrix <- getCalinskiCriterion(x)
#    if(missing(y)){  #y is in [2:10]
    y <- c(which.min(-calMatrix[,1])+1,1)
#    }else{}
#    if(length(y)==1){y <- c(y,1)}else{}
    plot(x,y,calinski=TRUE)

    getGraphicsEvent("Arrow to change partition, Return to select one...",
        onKeybd=function(key){
            if(key!="ctrl-J"){
                if(key=="Up" && y[1]>2)                 {y[1]<<-y[1]-1;y[2]<<-1}
                if(key=="Down" && y[1]<10)              {y[1]<<- y[1]+1;y[2]<<-1}
                if(key=="Right" && y[2]<ncol(calMatrix)){y[2]<<-y[2]+1}
                if(key=="Left" && y[2]>1)               {y[2]<<-y[2]-1}
                plot(x,y,calinski=TRUE)
                return(invisible())
            }else{return(TRUE)}
        }
    )

    bringToTop(-1)
    if(.is.tna(calMatrix[y[1]-1,y[2]])){
        cat("No partition selected\n")
    }else{
        cat("Name of the file (without extension):")
        filesName <- scan("",what="character",n=1)
        clust <- paste("c",y[1],sep="")
        exportClusterization(x@clusterizList[[clust]][[y[2]]],fileName=filesName)
        choicePlot(as(x,"LongData"),x@clusterizList[[clust]][[y[2]]])
    }
    return(invisible())
}
cleanProg(.clusterizLongData.choice,,,0)
setMethod("choice","ClusterizLongData",.clusterizLongData.choice)
rm(.clusterizLongData.choice)

#trajNoNA <- cld3@traj
#clusters <- p3a@clusters
#nbTime <- 3
#nbId <- 5
#nbClusters <- 2

.clusterizLongData.kml1 <- function(trajNoNA,clusters,nbTime,nbId=243,nbClusters=3,maxIt=200,print="all",distance="euclidean"){
    exClusterIndex <- rep(NA,nbId)
    iterations <- 0

    if(print=="all"){
        ld <- longData(traj=trajNoNA,id=1:nrow(trajNoNA),time=1:ncol(trajNoNA),trajSizeMin=2)
        part <- partition(id=1:length(clusters),clusters=LETTERS[clusters],nbClusters=max(clusters))
        plot(ld,part,color=c("black","color","color"),main=paste("Iteration =",iterations))
    }else{}

    while(!identical(clusters,exClusterIndex) & iterations <= maxIt){
        iterations <- iterations + 1
        exClusterIndex <- clusters
        trajMeanObs <- as.matrix(aggregate(trajNoNA,by=list(clusters),FUN=.meanNA)[,-1])
        distToMean <- array(0,dim=c(nbId,nbClusters))

        for(iCluster in 1:nbClusters){
            funcDist <- function(x){
                if(iCluster<=nrow(trajMeanObs)){
                    return(dist(t(cbind(x,trajMeanObs[iCluster,])),method=distance))
                }else{return(NA)}
            }
            distToMean[,iCluster] <- apply(trajNoNA,1,funcDist)
        }
        clusters <- apply(distToMean,1,.which.minNA)
        if(print=="all"){
            ld <- longData(traj=trajNoNA,id=1:nrow(trajNoNA),time=1:ncol(trajNoNA),trajSizeMin=2)
            part <- partition(id=1:length(clusters),clusters=LETTERS[clusters],nbClusters=max(clusters))
            plot(ld,part,color=c("black","color","color"),main=paste("Iteration =",iterations))
        }else{}
    }

    return(list(clusters=clusters,convergenceTime=iterations))
}
cleanProg(.clusterizLongData.kml1,,,3)  # .meanNA .which.minNA LETTERS
setGenericVerif("kml1",function(trajNoNA,clusters,nbTime,nbId,nbClusters=3,maxIt=200,print="all",distance="euclidean"){standardGeneric("kml1")})
setMethod("kml1","matrix",.clusterizLongData.kml1)



#.Object <- cld3
#nbClusters=2:3
#nbRedrawing=5

.clusterizLongData.kml <- function(.Object,nbClusters=2:6,nbRedrawing=20,maxIt=200,print="calinski",distance="euclidean"){
    noNA<-selectTrajNoNA(.Object)
    trajNoNA <- .Object@traj[noNA,]
    nbTime <- length(.Object@time)
    nbId <- nrow(trajNoNA)

    if(print=="all"){
        dev.off(2)
        if(.Platform$OS.type=="windows"){windows()}else{x11()}
    }else{}

    dev.off(3)
    if(.Platform$OS.type=="windows"){windows(5,5,xpos=0)}else{x11(xpos=1)}
    dev.set(3)
    plot(getCalinskiCriterion(.Object))
    nameObject<-deparse(substitute(.Object))
    for(iRedraw in 1:nbRedrawing){
        for(iNbClusters in nbClusters){
            clustersInit <- floor(runif(nbId,min=1,max=iNbClusters+1))
            if(print=="all"){
                dev.set(2)
            }else{}
            resultkml <- kml1(trajNoNA=trajNoNA,clusters=clustersInit,nbTime=nbTime,nbId=nbId,nbClusters=iNbClusters,maxIt=maxIt,print=print,distance=distance)
            yPartition <- partition(id=.Object@id[noNA],nbClusters=iNbClusters,clusters=LETTERS[resultkml[[1]]])
            yPartition <- expandPartition(object=yPartition,listId=.Object@id)
            addClusterization(.Object,clusterization(xLongData=as(.Object,"LongData"),yPartition=yPartition,convergenceTime=resultkml[[2]]))
            assign(nameObject,.Object,envir=parent.frame())
            dev.set(3)
            plot(getCalinskiCriterion(.Object))
        }
    }
    return(invisible())
}
cleanProg(.clusterizLongData.kml,,,1) # LETTERS
setGeneric("kml",function(.Object,nbClusters=2:6,nbRedrawing=20,maxIt=200,print="calinski",distance="euclidean"){standardGeneric("kml")})
setMethod("kml","ClusterizLongData",.clusterizLongData.kml)



setGenericVerif("importClusterization",function(xLongData,fileName)standardGeneric("importClusterization"))
#fileName <- "essaiExport2";xLongData <- ld4
.Clusterization.import <- function(xLongData,fileName){
    fullFileNameR<-paste(fileName,"-CLUSTERS.csv",sep="")
    data <- read.table(file=fullFileNameR,header=TRUE,sep=";")
    nbClusters <- as.numeric(substr(names(data)[2],2,3))
    data[,2] <- factor(LETTERS[data[,2]],levels=LETTERS[1:nbClusters])
    return(clusterization(xLongData=xLongData,yPartition=partition(nbClusters=nbClusters,id=data[,1],clusters=data[,2])))
}
cleanProg(.Clusterization.import,,,1) #LETTERS
setMethod("importClusterization","LongData",.Clusterization.import)



setGenericVerif("exportClusterization",function(object,fileName)standardGeneric("exportClusterization"))
.Clusterization.export <- function(object,fileName){
    if(length(fileName)==0){
        print(object)
    }else{
        if(substr(fileName,1,2)=="->"){
            nameExport <- substr(fileName,3,nchar(fileName))
            assign(paste(nameExport,"_clusters",sep=""),data.frame(id=object@id,clusters=as.character(object@clusters)),pos=.GlobalEnv)
            assign(paste(nameExport,"_criters",sep=""),
                list(nbClusters=object@nbClusters,percentEachCluster=object@percentEachCluster,CalinskiCriterion=object@calinskiCriterion,
                  traceBetween=object@traceBetween,traceWithin=object@traceWithin,detWithin=object@detWithin,convergenceTime=object@convergenceTime)
                ,pos=.GlobalEnv)
        }else{
            toPrint <- cbind(object@id,as.character(object@clusters))

            fullFileNameR<-paste(fileName,"-CLUSTERS.csv",sep="")
            write.table(toPrint,
                        file=fullFileNameR,row.names=FALSE,
                        col.names=c("id",paste("c",object@nbClusters,sep=""))
                        ,sep=";")
            fullFileNameC<-paste(fileName,"-CRITERS.csv",sep="")
            criters <- cbind(
                c("nbClusters",paste("Percent Cluster",LETTERS[1:object@nbClusters]),"Calinski Criterion",
                  "Trace Between","Trace Within","Det Within","Convergence time"),
                c(object@nbClusters,object@percentEachCluster,object@calinskiCriterion,
                  object@traceBetween,object@traceWithin,object@detWithin,object@convergenceTime)
                )
            write.table(criters,file=fullFileNameC,row.names=FALSE,col.names=FALSE,sep=";",dec=".")
        }
    }
}

cleanProg(.Clusterization.export,,,2) # LETTERS, .GlobalEnv
setMethod("exportClusterization","Clusterization",.Clusterization.export)
#exportClusterization(object,"->toto")

























