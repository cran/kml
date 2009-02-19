### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################## Class Clusterization ######################
############################## Création ############################
####################################################################\n")

cat("### Definition ###\n")
.Clusterization.validity <- function(object){
#    cat("**** validity Clusterization ****\n")
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
#      varBetween="matrix",
#      traceBetween="numeric",
#      varWithin="matrix",
#      traceWithin="numeric",
#      detWithin="numeric",
      calinski="numeric",
      convergenceTime="numeric"
   ),
   contain="Partition",
   prototype=prototype(
      percentEachCluster=numeric(),
#      varBetween=matrix(),
#      traceBetween=numeric(),
#      varWithin=matrix(),
#      traceWithin=numeric(),
#      detWithin=numeric(),
      calinski=numeric(),
      convergenceTime=numeric()
   ),
   validity=.Clusterization.validity
)


cat("####################################################################
######################## Class Clusterization ######################
############################ Constructeur ##########################
####################################################################\n")

clusterization <- function(xLongData,yPartition,convergenceTime=0){
#    cat("*** initialize Clusterization ***\n")
    if(missing(xLongData) && missing(yPartition)){
        new("Clusterization")
    }else{
        if(missing(xLongData) | missing(yPartition)){stop("[Clusterization:initialize] : Trajectories or Partition is missing !")}else{}
        if(length(xLongData["id"])!=length(yPartition["id"])){
            stop("Clusterization(initialize) : the partition has not the same length that the number of trajectoire")}else{}
        if(!identical(xLongData["id"],yPartition["id"])){
            warning("Clusterization(initialize) : the Partition has not the same id that the LongData")}else{}

        clusters <- yPartition["clusters"]
        traj <- xLongData["traj"]
        toKeep <- apply(traj,1,function(x)sum(!is.na(x))>=xLongData@trajSizeMin) & !is.na(clusters)
        clusters <- clusters[toKeep]
        traj <- traj[toKeep,,drop=FALSE]
#        show(traj)
#        print(dim(traj))
#        show(clusters)
#        print(length(clusters))
        values <- imputeLongData(traj,"copyMean",clusters)
        values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
        cls.attr <- cls.attrib(values,match(clusters,LETTERS[1:25]))
        varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
        varWithin <- wcls.matrix(values,match(clusters,LETTERS[1:25]),cls.attr$cluster.center)
        traceBetween <- sum(diag(varBetween))
        traceWithin <- sum(diag(varWithin))
        calinski <- traceBetween/traceWithin*(length(clusters)-yPartition@nbClusters)/(yPartition@nbClusters-1)
        if(is.na(calinski)){calinski<-NaN}

        new("Clusterization",
            id = xLongData@id,
#?#         clusters = factor(yPartition@clusters,levels=names(sort(table(yPartition@clusters),decreasing=TRUE)),labels=LETTERS[1:yPartition@nbClusters]),
            clusters = yPartition@clusters,
            #clusters = factor(clusters,levels=LETTERS[order(table(clusters),decreasing=TRUE)],labels=LETTERS[1:nbClusters])
            nbClusters=yPartition@nbClusters,
            percentEachCluster = as.numeric(table(yPartition@clusters)/length(yPartition@clusters)),
            calinski = calinski,
            convergenceTime = convergenceTime
        )
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
            "id"={return(x@id)},
            "clusters"={return(x@clusters)},
            "nbClusters"={return(x@nbClusters)},
            "percentEachCluster"={return(x@percentEachCluster)},
            "calinski"={return(x@calinski)},
            "convergenceTime"={return(x@convergenceTime)},
            stop("[Clusterization:getteur]: there is not such a slot in Clusterization")
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
    cat("\n ~ nbClusters = ",object@nbClusters)
    cat("\n ~ percentEachCluster = ",formatC(object@percentEachCluster,digits=2))
    cat("\n ~ calinski  = ",formatC(object@calinski,digits=2))
    cat("\n ~ convergenceTime    = ",formatC(object@convergenceTime,digits=2))
    cat("\n ~ clusters   :")
    if(length(object@nbClusters)!=0){
        for (iCluster in LETTERS[1:object@nbClusters]){
            toKeep <- iCluster==object@clusters
            cat("\n    ",iCluster," : [",sum(toKeep,na.rm=TRUE),"] ",sep="")
            catShort(object@id[toKeep & !is.na(toKeep)])
        }
        cat("\n   <NA> : [",sum(is.na(object@clusters)),"] ",sep="")
        catShort(object@id[is.na(object@clusters)])
        cat("\n")
    }else{
        cat("\n     <empty Clusterization>\n")
    }
    return(invisible(object))
}
cleanProg(.Clusterization.show,,,1) #LETTERS
setMethod(f="show",signature="Clusterization",definition=.Clusterization.show)
rm(.Clusterization.show)

cat("\n####################################################################
######################## Class Clusterization ######################
############################### Autres #############################
####################################################################\n")

