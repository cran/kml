#setGenericVerif <- function(x,y){if(!isGeneric(x)){setGeneric(x,y)}else{}}

setGeneric("longData",function(traj,id,time,varName="V",trajSizeMin=1){standardGeneric("longData")})
setGeneric("selectSupTrajSizeMin",function(object){standardGeneric("selectSupTrajSizeMin")})
setGeneric("partition",function(id,nbClusters,clusters){standardGeneric("partition")})
setGeneric("expandPartition",function(object,listId){standardGeneric("expandPartition")})
setGeneric("imputeLongData",function(.Object,method,yPartition)standardGeneric("imputeLongData"))
setGeneric("plotTraj",function(x,y,...){standardGeneric("plotTraj")})
setGeneric("plotSubGroups",function(x,y,...){standardGeneric("plotSubGroups")})
setGeneric("plotAll",function(x,y,...){standardGeneric("plotAll")})
setGeneric("clusterizLongData",function(traj,id,time,varName="V",trajSizeMin=1){standardGeneric("clusterizLongData")})
setGeneric("as.clusterizLongData",function(data,...){standardGeneric("as.clusterizLongData")})
setGeneric("as.longData",function(data,...){standardGeneric("as.longData")})

#setGeneric("kml1",function(trajNoNA,clusters,nbTime,nbId,nbClusters=3,maxIt=200,screenPlot=NA,distance="euclidean"){standardGeneric("kml1")})
setGeneric("kml",function(Object,nbClusters=2:6,nbRedrawing=20,saveFreq=100,maxIt=200,printCal=FALSE,printTraj=FALSE,distance=function(x,y){return(dist(t(cbind(x,y))))}){standardGeneric("kml")})
#setGeneric("kmlb",function(Object,nbClusters=2:6,nbRedrawing=20,saveFreq=100,maxIt=200,printCal=FALSE,printTraj=FALSE,distance=function(x,y){return(dist(t(cbind(x,y))))}){standardGeneric("kmlb")})
setGeneric("exportClusterization",function(object,y,typeGraph,...)standardGeneric("exportClusterization"))
setGeneric("choice",function(Object,typeGraph="bmp"){standardGeneric("choice")})

