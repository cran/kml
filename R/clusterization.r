### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################## Class Clusterization ######################
############################## Création ############################
####################################################################\n")

cat("### Definition ###\n")
.Clusterization.validity <- function(object){
    cat("**** validity Clusterization ****\n")
    validObject(as(object,"Partition"))
    return(TRUE)
}
cleanProg(.Clusterization.validity)
# nbCluster : cluster number
# clusterIndex :
setClass(
   Class="Clusterization",
   representation=representation(
      percentEachCluster="numeric",
      varBetween="matrix",
      traceBetween="numeric",
      varWithin="matrix",
      traceWithin="numeric",
      detWithin="numeric",
      calinskiCriterion="numeric",
      convergenceTime="numeric"
   ),
   contain="Partition",
   prototype=prototype(
      percentEachCluster=numeric(),
      varBetween=matrix(),
      traceBetween=numeric(),
      varWithin=matrix(),
      traceWithin=numeric(),
      detWithin=numeric(),
      calinskiCriterion=numeric(),
      convergenceTime=numeric()
   ),
   validity=.Clusterization.validity
)


cat("####################################################################
######################## Class Clusterization ######################
############################ Constructeur ##########################
####################################################################\n")

cat("### Initialize ###\n")
#.Object <- new("Clusterization")
#xLongData <- ld4n#.Object
#yPartition <- p4an#clusters
.Clusterization.initialize <- function(.Object,xLongData,yPartition,convergenceTime=0){
    cat("*** initialize Clusterization ***\n")
    if(missing(xLongData) && missing(yPartition)){
    }else{
        if(missing(xLongData) | missing(yPartition)){stop("[Clusterization:initialize] : Trajectories or Partition is missing !")}else{}
        if(length(getId(xLongData))!=length(getId(yPartition))){
            stop("Clusterization(initialize) : the partition has not the same length that the number of trajectoire")}else{}
        if(!identical(getId(xLongData),getId(yPartition))){
            warning("Clusterization(initialize) : the Partition has not the same id that the LongData")}else{}

        clusters <- getClusters(yPartition)
        traj <- getTraj(xLongData)
        toKeep <- apply(traj,1,function(x)sum(!is.na(x))>=xLongData@trajSizeMin) & !is.na(clusters)
        clusters <- clusters[toKeep]
        traj <- traj[toKeep,,drop=FALSE]
        show(traj)
        print(dim(traj))
        show(clusters)
        print(length(clusters))
        values <- imputeLongData(traj,"copyMean",clusters)
        values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
        cls.attr <- cls.attrib(values,match(clusters,LETTERS[1:10]))
        varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
        varWithin <- wcls.matrix(values,match(clusters,LETTERS[1:10]),cls.attr$cluster.center)
        traceBetween <- sum(diag(varBetween))
        traceWithin <- sum(diag(varWithin))
        detWithin <- det(varWithin)
        calinskiCriterion <- traceBetween/traceWithin*(length(clusters)-yPartition@nbClusters)/(yPartition@nbClusters-1)
        if(is.na(calinskiCriterion)){calinskiCriterion<-NaN}

        .Object@id <- xLongData@id
        .Object@nbClusters <- yPartition@nbClusters
        .Object@clusters <- factor(yPartition@clusters,levels=names(sort(table(yPartition@clusters),decreasing=TRUE)),labels=LETTERS[1:yPartition@nbClusters])
        .Object@percentEachCluster <- as.numeric(table(.Object@clusters)/length(.Object@clusters))
        .Object@varBetween <- varBetween
        .Object@traceBetween <- traceBetween
        .Object@varWithin <- varWithin
        .Object@traceWithin <- traceWithin
        .Object@detWithin <- detWithin
        .Object@calinskiCriterion <- calinskiCriterion
        .Object@convergenceTime <- convergenceTime
    }
    validObject(.Object)
    return(.Object)
}

setMethod("initialize","Clusterization",.Clusterization.initialize)

clusterization <- function(xLongData,yPartition,convergenceTime=0){
    new("Clusterization",xLongData=xLongData,yPartition=yPartition,convergenceTime=convergenceTime)
}



cat("\n####################################################################
######################## Test  Clusterization ######################
############################# Accesseurs ###########################
####################################################################\n")

setGenericVerif("getCalinskiCriterion",function(object)standardGeneric("getCalinskiCriterion"))
setMethod("getCalinskiCriterion","Clusterization",function(object){return(object@calinskiCriterion)})

setGenericVerif("getPercentEachCluster",function(object)standardGeneric("getPercentEachCluster"))
setMethod("getPercentEachCluster","Clusterization",function(object){return(object@percentEachCluster)})

setGenericVerif("getTraceBetween",function(object)standardGeneric("getTraceBetween"))
setMethod("getTraceBetween","Clusterization",function(object){return(object@traceBetween)})

setGenericVerif("getTraceWithin",function(object)standardGeneric("getTraceWithin"))
setMethod("getTraceWithin","Clusterization",function(object){return(object@traceWithin)})

setGenericVerif("getDetWithin",function(object)standardGeneric("getDetWithin"))
setMethod("getDetWithin","Clusterization",function(object){return(object@detWithin)})

setGenericVerif("getConvergenceTime",function(object)standardGeneric("getConvergenceTime"))
setMethod("getConvergenceTime","Clusterization",function(object){return(object@convergenceTime)})

setGenericVerif("getVarBetween",function(object)standardGeneric("getVarBetween"))
setMethod("getVarBetween","Clusterization",function(object){return(object@varBetween)})

setGenericVerif("getVarWithin",function(object)standardGeneric("getVarWithin"))
setMethod("getVarWithin","Clusterization",function(object){return(object@varWithin)})

#setGenericVerif("setTrajSizeMin<-",function(object,values)standardGeneric("setTrajSizeMin<-"))
#setReplaceMethod("setTrajSizeMin","Clusterization",
#    function(object,values){
#        object@valuesAtLeast <- values
#        validObject(object)
#        return(object)
#    }
#)


cat("####################################################################
######################## Class Clusterization ######################
############################## Affichage ###########################
####################################################################\n")



cat("### Method : 'show' for yPartition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Clusterization.show <- function(object){
    cat("******* show(Clusterization) *******")
    if(length(object@nbClusters)==0){
        cat("\n* empty partition")
    }else{
        cat("\n* id       = ");.catShort(object@id)
        cat("\n* nbClusters = ",object@nbClusters)
        cat("\n* clusters   :")
        for (iCluster in LETTERS[1:object@nbClusters]){
            cat("\n*   ",iCluster,":")
            .catShort(which(iCluster==object@clusters))
        }
        cat("\n* percentEachCluster = ",formatC(object@percentEachCluster,digits=2))
        cat("\n* traceBetween       = ",formatC(object@traceBetween,digits=2))
        cat("\n* traceWithin        = ",formatC(object@traceWithin,digits=2))
        cat("\n* detWithin          = ",formatC(object@detWithin,digits=2))
        cat("\n* calinskiCriterion  = ",formatC(object@calinskiCriterion,digits=2))
        cat("\n* convergenceTime    = ",formatC(object@convergenceTime,digits=2))
    }
    cat("\n*** End of  show(Clusterization) ***\n")
    return(invisible(object))
}
setMethod(f="show",signature="Clusterization",definition=.Clusterization.show)


cat("### Method : 'print' for Clusterization ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Clusterization.print <- function(x,...){
    cat("******* print(Clusterization) *******\n",sep="")
    cat("* nbClusters = ",x@nbClusters,"\n")
    cat("* id          :\n") ; print(x@id)
    cat("* clusters    :\n") ; print(x@clusters)
    cat("* varBetween  :\n"); print(x@varBetween)
    cat("* varWithin   :\n"); print(x@varWithin)
    cat("* percentEachCluster  = ",formatC(x@percentEachCluster,digits=2))
    cat("\n* traceBetween      = ",formatC(x@traceBetween,digits=2))
    cat("\n* traceWithin       = ",formatC(x@traceWithin,digits=2))
    cat("\n* detWithin         = ",formatC(x@detWithin,digits=2))
    cat("\n* calinskiCriterion = ",formatC(x@calinskiCriterion,digits=2))
    cat("\n* convergenceTime   = ",formatC(x@convergenceTime,digits=2))
    cat("\n*** End of  print(Clusterization) ***\n",sep="")
    return(invisible(x))
}

setMethod(f="print",signature="Clusterization",definition=.Clusterization.print)



cat("\n####################################################################
######################## Class Clusterization ######################
############################### Autres #############################
####################################################################\n")

#########
### setMethod("exportClusterization","Clusterization",.Clusterization.export)
### fait dans clusterizLongData.r
