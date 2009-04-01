### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################## Class Clusterization ######################
############################## Création ############################
####################################################################\n")

cat("### Definition ###\n")

.Clusterization.validity <- function(object){
    validObject(as(object,"Partition"))
    return(TRUE)
}

# nbCluster : cluster number
# clusterIndex :
setClass(
   Class="Clusterization",
   representation=representation(
      percentEachCluster="numeric",
      convergenceTime="numeric",
      criterionName="character",
      criterionValue="numeric",
      imputationMethod="character",
      startingCondition="character",
      algorithmUsed="character"
   ),
   contain="Partition",
   prototype=prototype(
      percentEachCluster=numeric(),
      convergenceTime=numeric(),
      criterionName=character(),
      criterionValue=numeric(),
      imputationMethod=character(),
      startingCondition=character(),
      algorithmUsed=character()
   ),
   validity=.Clusterization.validity
)


cat("####################################################################
######################## Class Clusterization ######################
############################ Constructeur ##########################
####################################################################\n")

clusterization <- function(xPartition,yLongData,convergenceTime=0,
    criterionName=c("calinski"),criterionValue=numeric(),
    imputationMethod="linearInterpolation",startingCondition="",algorithmUsed=""
){
   cat("*** initialize Clusterization ***\n")
    if(missing(yLongData) && missing(xPartition)){
        new("Clusterization")
    }else{
        if(missing(yLongData) | missing(xPartition)){stop("[Clusterization:initialize] : Trajectories or Partition is missing !")}else{}
        if(length(yLongData["id"])!=length(xPartition["clusters"])){
            stop("Clusterization(initialize) : the partition has not the same length that the number of trajectoire")}else{}
        clusters <- xPartition["clusters"]
        traj <- yLongData["traj"]
        tab <- as.numeric(table(clusters))
        new("Clusterization",clusters=clusters,nbClusters=xPartition["nbClusters"],percentEachCluster=tab/sum(tab),convergenceTime=convergenceTime,
             criterionName=criterionName,criterionValue=unlist(criterion(yLongData,xPartition,imputationMethod)[criterionName]),
             imputationMethod=imputationMethod,startingCondition=startingCondition,algorithmUsed=algorithmUsed)
    }
}



cat("\n####################################################################
######################## Test  Clusterization ######################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur ###\n")
setMethod("[","Clusterization",
    function(x,i,j,drop){
        switch(EXPR=i,
            "clusters"={return(x@clusters)},
            "nbClusters"={return(x@nbClusters)},
            "percentEachCluster"={return(x@percentEachCluster)},
            "convergenceTime"={return(x@convergenceTime)},
            "criterionName"={return(x@criterionName)},
            "criterionValue"={return(x@criterionValue)},
            "imputationMethod"={return(x@imputationMethod)},
            "startingCondition"={return(x@startingCondition)},
            "algorithmUsed"={return(x@algorithmUsed)},
            if(any(x@criterionName %in% i)){
                return(x@criterionValue[x@criterionName %in% i])
            }else{
                stop("[Clusterization:getteur]: there is not such a slot in Clusterization")
            }
        )
    }
)



cat("####################################################################
######################## Class Clusterization ######################
############################## Affichage ###########################
####################################################################\n")



cat("### Method : 'show' for yPartition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Clusterization.show <- function(object){
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n ~ nbClusters         = ",object@nbClusters)
    cat("\n ~ convergenceTime    = ",object@convergenceTime)
    cat("\n ~ percentEachCluster = ",formatC(object@percentEachCluster,digits=2))
    cat("\n ~ criterionName      = ",object@criterionName)
    cat("\n ~ criterionValue     = ",formatC(object@criterionValue,digits=2))
    cat("\n ~ imputationMethod   = ",object@imputationMethod)
    cat("\n ~ startingCondition  = ",object@startingCondition)
    cat("\n ~ algorithmUsed      = ",object@algorithmUsed)
    cat("\n ~ clusters   : [",length(object@clusters),"]",sep="")
    if(length(object@nbClusters)!=0){
        for (iCluster in LETTERSletters[1:object@nbClusters]){
            toKeep <- iCluster==object@clusters
            cat("\n    ",iCluster," : [",sum(toKeep,na.rm=TRUE),"] ",sep="")
            catShort((1:length(object@clusters))[toKeep & !is.na(toKeep)])
        }
        cat("\n   <NA> : [",sum(is.na(object@clusters)),"] ",sep="")
        catShort((1:length(object@clusters))[is.na(object@clusters)])
        cat("\n")
    }else{
        cat("\n     <empty Partition>\n")
    }
    return(invisible(object))
}
setMethod(f="show",signature="Clusterization",definition=.Clusterization.show)


cat("\n####################################################################
######################## Class Clusterization ######################
############################### Autres #############################
####################################################################\n")

