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
#    cat("**** validity Partition ****\n")
    if(!(length(object@nbClusters)==0&length(object@clusters)==0&length(object@id)==0)){#not empty object
        if(any(c(length(object@nbClusters)==0,length(object@clusters)==0,length(object@id)==0))){
            stop("[Partition:validity]: at least one slot is empty")}else{}
        if(object@nbClusters > 25){
            stop("[Partition:validity]: More than 25 clusters")}else{}
        if(length(object@id)!=length(object@clusters)){
            stop("[Partition:validity]: Some individual have no cluster")}else{}
        if(!all(na.omit(object@clusters)%in%LETTERS[1:object@nbClusters])){
            stop("[Partition:validity]: Clusters name out of range")}else{}
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


setMethod("partition",signature=c("missing","missing","missing"),function(id,nbClusters,clusters){new("Partition")})
setMethod("partition",signature=c("ANY","ANY","ANY"),
    function(id,nbClusters,clusters){
        if(missing(nbClusters)){
            nbClusters <- max(1,match(clusters,LETTERS),na.rm=TRUE)
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


cat("### Getteur ###\n")
setMethod("[","Partition",
    function(x,i,j,drop){
        switch(EXPR=i,
            "id"={return(x@id)},
            "nbClusters"={return(x@nbClusters)},
            "clusters"={return(x@clusters)},
            stop("[Partition:getteur]: there is not such a slot in Partition")
        )
    }
)

cat("### Setteur ###\n")
setReplaceMethod("[","Partition",
    function(x,i,j,value){
        switch(EXPR=i,
            "id"={x@id<-as.character(value)},
            "clusters"={
                clusters <- factor(value,levels=LETTERS[1:x@nbClusters])
                x@clusters <- factor(clusters,levels=LETTERS[order(table(clusters),decreasing=TRUE)],labels=LETTERS[1:x@nbClusters])
            },
            "nbClusters"={x@nbClusters<-value},
            stop("[LongData:getteur]: this is not a LongData slot")
        )
        validObject(x)
        return(x)
    }
)



cat("\n####################################################################
########################## Class Partition #########################
############################# Affichage ############################
####################################################################\n")

cat("### Method : 'show' for partition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Partition.show <- function(object){
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n ~ nbClusters = ",object@nbClusters)
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
        cat("\n     <empty Partition>\n")
    }
    return(invisible(object))
}
cleanProg(.Partition.show,,,1) #LETTERS
setMethod(f="show",signature="Partition",definition=.Partition.show)
rm(.Partition.show)



cat("\n####################################################################
########################## Class Partition #########################
############################### Autre ##############################
####################################################################\n")

# Fonction qui augmente ajoute des individus a une partition existante.
# Elle ne les met pas dans un cluster mais leur donne pour valeur NA.
cat("### expend : 'clusters' ###\n")
.Partition.expandPartition <- function(object,listId){
    clusters <- object@clusters[match(listId,object@id)]
    return(partition(clusters=clusters,id=listId,nbClusters=object@nbClusters))
}
cleanProg(.Partition.expandPartition,,,0)
setMethod("expandPartition","Partition",.Partition.expandPartition)
rm(.Partition.expandPartition)



