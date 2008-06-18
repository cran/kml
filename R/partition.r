### partition est un découpage non associé a une longData
### Donc pas de calcul d'indices ou autre.
###  - nbCluster est le nombre de cluster
###  - cluster est la liste des clusters sous forme d'un facteur
###
### nbCluster doit être égal ou supérieur au nombre effectif de cluster
### cluster est a valeur dans A, B, C...
### A priori, une lettre peut être absente (a voir : classe-t-on les lettre dans l'ordre ?)

#cleanProg <- function(...){return(invisible())}
cat("####################################################################
########################## Class Partition #########################
############################# Creation #############################
####################################################################\n")

cat("### Definition ###\n")

.Partition.validity <- function(object){
    cat("**** validity Partition ****\n")
    if(!(length(object@nbClusters)==0&length(object@clusters)==0&length(object@id)==0)){#not empty object
        if(any(c(length(object@nbClusters)==0,length(object@clusters)==0,length(object@id)==0))){
            stop("[Partition:validity]: at least one slot is empty")}else{}
        if(object@nbClusters > 10){
            stop("[Partition:validity]: More than 10 clusters")}else{}
        if(length(object@id)!=length(object@clusters)){
            stop("[Partition:validity]: Some individual have no cluster")}else{}
        if(!all(na.omit(object@clusters)%in%LETTERS[1:object@nbClusters])){
            stop("[Partition:validity]: Bad clusters name (should be in {A,B,C,D,E,F,G,H,I,J})")}else{}
        if(!identical(sort(tabulate(na.omit(object@clusters)),decreasing=TRUE),tabulate(na.omit(object@clusters)))){
            stop("[Partition:validity]: Clusters are not in decreasing order")}else{}
        if(any(is.na(object@id))){
            stop("[Partition:validity]: Some id are NA")}else{}
        if(any(duplicated(object@id))){
            stop("[Partition:validity]: Some id are duplicate")}else{}
#        if(object@nbClusters < max(c(match(object@clusters,LETTERS[1:10]),-Inf),na.rm=TRUE)){
 #           stop("[Partition:validity]: More cluster than indicated in nbClusters")}else{}
    }else{}
    return(TRUE)

}
cleanProg(.Partition.validity,,,1)  # LETTERS
setClass(
   Class="Partition",
   representation=representation(
      id="character",
      nbClusters = "numeric",
      clusters = "factor"
   ),
   prototype=prototype(
      id=character(),
      nbClusters=numeric(),
      clusters=factor()
   ),
   validity=.Partition.validity
)
rm(.Partition.validity)
cat("\n####################################################################
########################## Class Partition #########################
############################ Constructeur ##########################
####################################################################\n")

#cleanProg(.Partition.initialize,,,1)     # LETTERS est une globale
#setMethod(f="initialize",signature="Partition",definition=.Partition.initialize)
#rm(.Partition.initialize)




#cat("### Initialize ###\n")
#.Partition.initialize <- function(.Object,nbClusters,id,clusters){
#    cat("*** initialize Partition ***\n")
#    if(missing(nbClusters) & missing(id) & missing(clusters)){
#        return(.Object)
#    }else{
#        if(missing(id)){id <- seq_along(clusters)}else{}
#        if(length(names(clusters))==0){names(clusters) <- id}else{}
#        if(missing(nbClusters)){ # cluster with no (implicit) nbCluster
#            nbClusters <- max(1,match(clusters,LETTERS[1:10]),na.rm=TRUE)
#        }else{
#            if(max(match(clusters,LETTERS[1:10]),na.rm=TRUE)>nbClusters){stop("[Partition:initiale] : More cluster than indicated in nbClusters")}else{}
#        }
#        .Object@nbClusters <- nbClusters
#        .Object@clusters <- factor(clusters,levels=LETTERS[1:.Object@nbClusters])
#     }else{
#        if(missing(nbClusters)){ # No cluster, no nbCluster -> empty object, no validation
#        }else{ # nbCluster but no cluster
#            .Object@nbClusters <- nbClusters
#        }
#    }
#    validObject(.Object)
#   return(.Object)
#}
#cleanProg(.Partition.initialize,,,1)     # LETTERS est une globale
#setMethod(f="initialize",signature="Partition",definition=.Partition.initialize)
#rm(.Partition.initialize)


setGenericVerif("partition",function(nbClusters,id,clusters){standardGeneric("partition")})
setMethod("partition",signature=c("missing","missing","missing"),
    function(nbClusters,id,clusters){new("Partition")})
setMethod("partition",signature=c("ANY","ANY","ANY"),
    function(nbClusters,id,clusters){
        if(missing(nbClusters)){
            nbClusters <- max(1,match(clusters,LETTERS[1:10]),na.rm=TRUE)
        }else{}
        clusters=factor(clusters,levels=LETTERS[1:nbClusters])
        new("Partition",
            nbClusters=nbClusters,
            id=as.character(id),
            clusters=factor(clusters,levels=LETTERS[order(table(clusters),decreasing=TRUE)],labels=LETTERS[1:nbClusters])
        )
    }
)


cat("\n####################################################################
########################## Class Partition #########################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur : 'nbCluster' ###\n")
setGenericVerif("getNbClusters",function(object){standardGeneric("getNbClusters")})
setMethod("getNbClusters","Partition",function(object){return(object@nbClusters)})

cat("### Getteur : 'clusters' ###\n")
setGenericVerif("getClusters",function(object){standardGeneric("getClusters")})
setMethod("getClusters","Partition",function(object){return(object@clusters)})

cat("### Getteur : 'id' ###\n")
setGenericVerif("getId",function(object){standardGeneric("getId")})
setMethod("getId","Partition",function(object){return(object@id)})

cat("### Setteur : 'nbClusters' ###\n")
.Partition.setNbClusters <- function(object,value){
    object@nbClusters <- value
    validObject(object)
    return(object)
}
cleanProg(.Partition.setNbClusters,,,0)
setGenericVerif("setNbClusters<-",function(object,value){standardGeneric("setNbClusters<-")})
setReplaceMethod("setNbClusters","Partition",.Partition.setNbClusters)
rm(.Partition.setNbClusters)


cat("### Setteur : 'clusters' ###\n")
.Partition.setClusters <- function(object,value){
    object@clusters <- factor(value,levels=LETTERS[0:object@nbClusters])
    validObject(object)
    return(object)
}
cleanProg(.Partition.setClusters,,,1) # LETTERS
setGenericVerif("setClusters<-",function(object,value){standardGeneric("setClusters<-")})
setReplaceMethod("setClusters","Partition",.Partition.setClusters)
rm(.Partition.setClusters)

cat("### Setteur : 'id' ###\n")
.Partition.setId <- function(object,value){
    object@id <- as.character(value)
    validObject(object)
    return(object)
}
cleanProg(.Partition.setId,,,0)
setGenericVerif("setId<-",function(object,value){standardGeneric("setId<-")})
setReplaceMethod("setId","Partition",.Partition.setId)
rm(.Partition.setId)



cat("\n####################################################################
########################## Class Partition #########################
############################# Affichage ############################
####################################################################\n")

cat("### Method : 'show' for partition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Partition.show <- function(object){
    cat("******* show(Partition) *******\n",sep="")
    if(length(object@nbClusters)==0){
        cat("* empty partition")
    }else{
        cat("* nbClusters = ",object@nbClusters,"\n")
        cat("* clusters   :")
        for (iCluster in LETTERS[1:object@nbClusters]){
            cat("\n*   ",iCluster," : ")
            toKeep <- iCluster==object@clusters
            .catShort(object@id[toKeep & !is.na(toKeep)])
        }
        cat("\n*  <NA> : ")
        .catShort(object@id[is.na(object@clusters)])
    }
    cat("\n*** End of  show(Partition) ***\n",sep="")
    return(invisible(object))
}
cleanProg(.Partition.show,,,1) #LETTERS
setMethod(f="show",signature="Partition",definition=.Partition.show)
rm(.Partition.show)


cat("### Method : 'print' for partition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Partition.print <- function(x,...){
    cat("******* print(Partition) *******\n",sep="")
    if(length(x@nbClusters)==0){
        cat("* empty partition")
    }else{
        cat("* nbClusters = ",x@nbClusters,"\n")
        cat("* id         :\n")
        print(x@id)
        cat("* clusters   :\n")
        print(x@clusters)
    }
    cat("\n*** End of  print(Partition) ***\n",sep="")
    return(invisible(x))
}
cleanProg(.Partition.print,,,1) #LETTERS
setMethod(f="print",signature="Partition",definition=.Partition.print)
rm(.Partition.print)



cat("\n####################################################################
########################## Class Partition #########################
############################### Autre ##############################
####################################################################\n")

cat("### expend : 'clusters' ###\n")
.Partition.expandPartition <- function(object,listId){
    clusters <- object@clusters[match(listId,object@id)]
    return(partition(clusters=clusters,id=listId,nbClusters=object@nbClusters))
}
cleanProg(.Partition.expandPartition,,,0) # LETTERS
setGenericVerif("expandPartition",function(object,listId){standardGeneric("expandPartition")})
#lockBinding("setClusters<-",.GlobalEnv)
setMethod("expandPartition","Partition",.Partition.expandPartition)
rm(.Partition.expandPartition)



