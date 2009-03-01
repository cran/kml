#dyn.load("kml")

cat("\n#######################################################################
######################## Class ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

.ClusterizLongData.validity <- function(object){
    validObject(as(object,"LongData"))
    return(TRUE)
}
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterizLongData",
    representation=representation(
        trajMinSize="numeric",
        clusters="list"
    ),
    contains="LongData",
    prototype=prototype(
        trajMinSize=numeric(),
        clusters=list(
           c1=list(),c2=list(),c3=list(),c4=list(),c5=list(),
           c6=list(),c7=list(),c8=list(),c9=list(),c10=list(),
           c11=list(),c12=list(),c13=list(),c14=list(),c15=list(),
           c16=list(),c17=list(),c18=list(),c19=list(),c20=list(),
           c21=list(),c22=list(),c23=list(),c24=list(),c25=list(),
           c26=list(),c27=list(),c28=list(),c29=list(),c30=list(),
           c31=list(),c32=list(),c33=list(),c34=list(),c35=list(),
           c36=list(),c37=list(),c38=list(),c39=list(),c40=list(),
           c41=list(),c42=list(),c43=list(),c44=list(),c45=list(),
           c46=list(),c47=list(),c48=list(),c49=list(),c50=list(),
           c51=list(),c52=list()
        )
    ),
    validity=.ClusterizLongData.validity
)


cat("\n###################################################################
##################### Class ClusterizLongData #####################
########################### Constructeur ##########################
###################################################################\n")

#setMethod("clusterizLongData",signature=c("missing","missing","missing","missing","missing"),
#    function(traj,id,time,varName,trajMinSize){new("ClusterizLongData")}
#)
#setMethod("clusterizLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),
clusterizLongData <- function(traj,id,time,varName="V",trajMinSize=1){
    dimnames(traj) <- list(id,paste(varName,time,sep=""))
    new("ClusterizLongData",id=as.character(id),time=time,varName=varName,traj=traj,trajMinSize=trajMinSize)
}
#)
cld <- clusterizLongData

setMethod("as.clusterizLongData",signature=c("LongData"),
    function(data,trajMinSize=1,...){
        new("ClusterizLongData",id=data["id"],time=data["time"],varName=data["varName"],
            traj=data["traj"],trajMinSize=trajMinSize)
    }
 )

setMethod("as.clusterizLongData",signature=c("data.frame"),
    function(data,trajMinSize=1,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),
        varName=sub("[[:digit:]]*$", "",names(data)[timeCol[1]]),...
    ){
        traj <- as.matrix(data[,timeCol])
        dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
        return(clusterizLongData(id=id,time=timeReal,varName=varName,traj=traj,trajMinSize=trajMinSize))
    }
)
as.cld <- as.clusterizLongData

cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################ Accesseurs ###########################
################################################################## #\n")


# Si on veut rendre [<- utilisable pour partition, il faut modifier ICI
cat("### Setteur ###\n")
setReplaceMethod("[","ClusterizLongData",
    function(x,i,j,value){
        switch(EXPR=i,
            "id"={x@id<-as.character(value)},
            "time"={x@time<-value},
            "varName"={x@varName<-value},
            "traj"={x@traj<-value},
            "other"={x@other<-value},
            "trajMinSize"={x@trajMinSize<-value},
            "clusters"={
                if(missing(j)){j <- "add"}else{}
                switch(EXPR=j,
                    "add"={
                        x@clusters[[value@nbClusters]] <- c(x@clusters[[value@nbClusters]],list(value)) # ICI
                        getCriterion <- function(object){object["criterionValue"][1]}
                        x@clusters[[value@nbClusters]] <- x@clusters[[value@nbClusters]][
                            order(unlist(lapply(x@clusters[[value@nbClusters]],getCriterion)),decreasing=TRUE)
                        ]
                    },
                    "clear"={
                        if(value=="all"){
                            x@clusters <- list(
                                c1=list(),c2=list(),c3=list(),c4=list(),c5=list(),c6=list(),c7=list(),c8=list(),c9=list(),c10=list(),
                                c11=list(),c12=list(),c13=list(),c14=list(),c15=list(),c16=list(),c17=list(),c18=list(),c19=list(),c20=list(),
                                c21=list(),c22=list(),c23=list(),c24=list(),c25=list(),c26=list(),c27=list(),c28=list(),c29=list(),c30=list(),
                                c31=list(),c32=list(),c33=list(),c34=list(),c35=list(),c36=list(),c37=list(),c38=list(),c39=list(),c40=list(),
                                c41=list(),c42=list(),c43=list(),c44=list(),c45=list(),c46=list(),c47=list(),c48=list(),c49=list(),c50=list(),
                                c51=list(),c52=list())
                        }else{
                            x@clusters[[value]] <- list()
                        }
                    },
                    stop("[ClusterizLongData:setteur]: j should be 'add' or 'clear'")
                )
            },
            stop("[ClusterizLongData:setteur]: this is not a ClusterizLongData slot")
        )
        dimnames(x@traj) <- list(x@id,paste(x@varName,x@time,sep=""))
        validObject(x)
        return(x)
    }
)


#setGenericVerif("addPartition",function(.Object,yPartition)standardGeneric("addPartition"))
#.clusterizLongData.addPartition <- function(.Object,yPartition){
#    nameObject<-deparse(substitute(.Object))
#    addClusterization(.Object,clusterization(xLongData=as(.Object,"LongData"),yPartition=yPartition))
#    assign(nameObject,.Object,envir=parent.frame())
#    return(invisible())
#}
#cleanProg(.clusterizLongData.addPartition,,,0)
#setMethod("addPartition","ClusterizLongData",.clusterizLongData.addPartition)
#rm(.clusterizLongData.addPartition)


cat("### Getteur ###\n")
setMethod("[","ClusterizLongData",
    function(x,i,j,drop){
        switch(EXPR=i,
            "id"={return(x@id)},
            "time"={return(x@time)},
            "varName"={return(x@varName)},
            "traj"={return(x@traj)},
            "other"={return(x@other)},
            "trajMinSize"={return(x@trajMinSize)},
            "clusters"={
                if(missing(j)){
                    return(x@clusters)
                }else{
                    if(length(j)==1){
                        return(x@clusters[[j]])
                    }else{
                        if(length(j)==2){
                            return(x@clusters[[j[1]]][[as.integer(j[2])]])
                        }else{
                            stop("[ClusterizLongData] : j should be 'c2' or c('c2',3), nothing else")
                        }
                    }
                }
            },
            "criterionName"={
                getCriterionName <- function(object){object["criterionName"]}
                criterionList <- character()
                for(k in 1:52){
                    criterionList <-unique(c(unlist(lapply(x@clusters[[k]],getCriterionName)),criterionList))
                }
                return(criterionList)
            },
#           "criterionValue"={
 #              ncolMatrix = max(c(unlist(lapply(x@clusters,length)),1))
  #             criterionList <- matrix(NA,nrow=52,ncol=ncolMatrix,dimnames=list(paste("c",1:52,sep=""),list()))
   #            getCriterion <- function(object){object["criterionValue"][1]}
    #           for(i in 1:52){
     #              calLine <-lapply(x@clusters[[i]],getCriterion)
      #             criterionList[i,] <- c(unlist(calLine),rep(NA,ncolMatrix-length(calLine)))

                                        #        }
        #       if(missing(j)){
         #          return(criterionList)
          #     }else{
           #        if(length(j)==1){
            #           return(criterionList[j,])
             #      }else{
              #         if(length(j)==2){
               #            return(criterionList[j[1],as.integer(j[2])])
                #       }else{
                 #          stop("[ClusterizLongData] : j should be 'cx' or c('cx',y), nothing else")
                  #     }
   #                }
  #             }
 #              return(criterionList)
#           },
#           "criterionName"={
 #              ncolMatrix = max(c(unlist(lapply(x@clusters,length)),1))
  #             criterionList <- matrix(NA,nrow=52,ncol=ncolMatrix,dimnames=list(paste("c",1:52,sep=""),list()))
   #            getCriterionName <- function(object){object["criterionName"][1]}
    #           for(i in 1:52){
     #              calLine <-lapply(x@clusters[[i]],getCriterionName)
      #             criterionList[i,] <- c(unlist(calLine),rep(NA,ncolMatrix-length(calLine)))
       #        }
        #       if(missing(j)){
         #          return(criterionList)
          #     }else{
           #        if(length(j)==1){
            #           return(criterionList[j,])
             #      }else{
              #         if(length(j)==2){
               #            return(criterionList[j[1],as.integer(j[2])])
                #       }else{
                 #          stop("[ClusterizLongData] : j should be 'cx' or c('cx',y), nothing else")
                  #     }
   #                }
  #             }
 #              return(criterionList)
#           },
            if(i %in% paste("c",1:52,sep="")){
                if(missing(j)){
                    return(x@clusters[[i]])
                }else{
                    return(x@clusters[[i]][[j]])}
            }else{
                if(i %in% c("calinski","ray","davies")){
                    ncolMatrix = max(c(unlist(lapply(x@clusters,length)),1))
                    criterionList <- matrix(NA,nrow=52,ncol=ncolMatrix,dimnames=list(paste("c",1:52,sep=""),list()))
                    getCriterion <- function(object){object[i][1]}
                    for(k in 1:52){
                        calLine <-lapply(x@clusters[[k]],getCriterion)
                        criterionList[k,] <- c(unlist(calLine),rep(NA,ncolMatrix-length(calLine)))
                    }
                    if(missing(j)){
                        return(criterionList)
                    }else{
                        if(length(j)==1){
                            return(criterionList[j,])
                        }else{
                            if(length(j)==2){
                                return(criterionList[j[1],as.integer(j[2])])
                            }else{
                                stop("[ClusterizLongData] : j should be 'cx' or c('cx',y), nothing else")
                            }
                        }
                    }
                }else{
                    stop("[ClusterizLongData:getteur]: there is not such a slot in ClusterizLongData")
                }
            }
        )
    }
)


### Si y n'a qu'une coordonnée, c'est le nombre de cluster. Dans ce cas, on lui donne '1' pour deuxième coordonnée
### Si y est absent
###   - S'il n'y a aucun Calinski, on affiche longData avec moyenne si moyenne=black ou color.
###   - S'il y a des Calinski, on lui donne le plus grand





cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################ Affichage ############################
###################################################################\n")

cat("### Method: 'show' pour ClusterizLongData ###\n")
.clusterizLongData.show <- function(object){
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n~ id   : [",length(object@id),"] ",sep="");catShort(object@id)
    cat("\n~ time : [",length(object@time),"] ",sep="");catShort(object@time)
    cat("\n~ varName     :",object@varName)
    cat("\n~ other   :\n")
    if(length(object@other)!=0){
      cat("  ",names(object@other),"\n")
    }else{}
    cat("\n~ trajMinSize :",object@trajMinSize,"\n")
    cat("\n~ traj : [",length(object@id),"x",length(object@time),"] (limited to 10x10) :\n",sep="")
    if(length(object@id)!=0){
        if(ncol(object@traj)>10){
            trajToShow <- as.data.frame(object@traj[,1:10])
            trajToShow$more <- "..."
        }else{
            trajToShow <- as.data.frame(object@traj)
        }
        if(nrow(object@traj)>10){
            print(trajToShow[1:10,])
            cat("... ...\n")
        }else{
            print(trajToShow)
        }
    }else{cat("   <no trajectories>\n")}


    cat("\n ~ criterion name availables :")
    toPrint <- object["criterionName"]
    if(length(toPrint)==0){
        cat(" <no clusterization>\n")
    }else{
        cat(" ",toPrint)
    }
    cat("\n ~ criterion value (calinski)\n")

    toPrint <- object["calinski"]
    toPrint <- toPrint[!is.na(toPrint[,1]),,drop=FALSE]
    if(length(toPrint)==0){
        cat("   <no clusterization>\n")
    }else{
        if(ncol(toPrint)>10){
            toPrint <- as.data.frame(toPrint[,1:10])
            toPrint$more <- "..."
            print(toPrint)
        }else{
            print(toPrint)
        }
    }
    return(invisible(object))
}
setMethod("show","ClusterizLongData",.clusterizLongData.show)




.clusterizLongData.plotAllCriterion <- function(x,method="linearInterpolation"){
    toEvaluate <- !is.tna(x["calinski"][,1])
    crit <- matrix(NA,52,3)
    for (i in 1:52){
        if(toEvaluate[i]){
            crit[i,] <- unlist(criterion3(x["traj"],x["clusters"][[c(i,1)]],method=method))
        }else{}
    }

    crit[,1] <- crit[,1]-min(crit[,1],na.rm=TRUE)
    crit[,1] <- crit[,1]/max(crit[,1],na.rm=TRUE)

    crit[,2] <- crit[,2]-min(crit[,2],na.rm=TRUE)
    crit[,2] <- crit[,2]/max(crit[,2],na.rm=TRUE)

    crit[,3] <- max(crit[,3],na.rm=TRUE)-crit[,3]
    crit[,3] <- crit[,3]/max(crit[,3],na.rm=TRUE)

    minVal <- min(which(apply(crit,1,function(x){any(!is.na(x))})))
    maxVal <- max(which(apply(crit,1,function(x){any(!is.na(x))})))

    xlab <- "red=+Calinski ; green=+Ray ; blue=-Davies"
    main <- "Qualities criteria (normalized, higher is better)"
    matplot(c(1:52)[minVal:maxVal],(crit[minVal:maxVal,]),col=2:4,type="b",lty=1,xlab=xlab,ylab="",main=main,pch=c("C","R","D"))
    return(invisible())
}
setMethod("plotAllCriterion","ClusterizLongData",.clusterizLongData.plotAllCriterion)



.clusterizLongData.plotCriterion <- function(x,criterion="calinski",nbCriterion=100,allCrit=FALSE){
    crit <- x[criterion]
    if(!all(is.na(crit))){
        if(!allCrit){
            plot(NAtrunc(crit[,1]),type="b",xlab="",ylab="",main=criterion)
        }else{
            matplot(1:min(ncol(crit),nbCriterion),t(crit[,1:min(ncol(crit),nbCriterion)]),type="b",xlab="",ylab="",main=criterion)
        }
    }else{
        plot(1,main="All criterion are undefined",type="n")
    }
    return(invisible())
}
setMethod("plotCriterion","ClusterizLongData",.clusterizLongData.plotCriterion)



cat("### Method: 'plot' pour clusterizLongData ###\n")
.clusterizLongData.plot <- function(x,y,col="clusters",col.mean="clusters",main="",type="l",type.mean="b",size=1,...){
    if(missing(y)){
        if(all(is.tna(x["calinski"]))){#only true NA => no cluster define => plot without subgroups
            part <- partition(clusters=rep("A",length(x["id"])),nbClusters=1)
            plot(x=as(x,"LongData"),y=part,col=col,col.mean=col.mean,main=main,type=type,type.mean=type.mean,size=size,...)
            return(invisible())
        }else{
            y <- c(which.max(x["calinski"][,1]),1)
        }
    }else{}

    if(length(y)==1){y<-c(y,1)}else{}
    part <- x["clusters",y]
    plot(x=as(x,"LongData"),y=part,col=col,col.mean=col.mean,main=main,type=type,type.mean=type.mean,size=size,...)
    return(invisible())
}
#setGenericVerif("plotClusterizLongData",function(x,y,...){standardGeneric("plotClusterizLongData")})
#setMethod("plotTraj",c("ClusterizLongData","ANY"),.clusterizLongData.plot)
setMethod("plot",c("ClusterizLongData","numeric"),.clusterizLongData.plot)
setMethod("plot",c("ClusterizLongData","missing"),.clusterizLongData.plot)


.ClusterizLongData.plotSubGroups <- function(x,y,col="clusters",col.mean="clusters",main="",type="l",type.mean="b",size=1,...){
    if(missing(y)){
        if(all(is.tna(x["calinski"]))){#only true NA => no cluster define => plot without subgroups
            part <- partition(clusters=rep("A",length(x["id"])),nbClusters=1)
            plot(x=as(x,"LongData"),y=part,col=col,col.mean=col.mean,main=main,type=type,type.mean=type.mean,size=size,...)
            return(invisible())
        }else{
            y <- c(which.max(x["calinski"][,1]),1)
        }
    }else{}

    if(length(y)==1){y<-c(y,1)}else{}
    part <- x["clusters",y]
    plotSubGroups(x=as(x,"LongData"),y=part,col=col,col.mean=col.mean,main=main,type=type,type.mean=type.mean,size=size,...)
    return(invisible())
}
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotSubGroups",signature=c("ClusterizLongData","ANY"),def=.ClusterizLongData.plotSubGroups)



#x <- a
#print.cal=TRUE;print.traj=TRUE;print.sub=FALSE;
#all=TRUE;nbCriterion=100
#col=1;type="l";col.mean="clusters";type.mean="b";main="";size=1;ylim=NA;
#col.sub=1;type.sub="l";col.mean.sub="clusters";type.mean.sub="b";main.sub="";size.sub=1;ylim.sub=NA



.ClusterizLongData.plotAll <- function(x,y,print.cal=TRUE,print.traj=TRUE,print.sub=FALSE,
                    allCrit=TRUE,nbCriterion=100,
                    col=1,type="l",col.mean="clusters",type.mean="b",main="",size=1,ylim=NA,
                    col.sub=1,type.sub="l",col.mean.sub="clusters",type.mean.sub="b",main.sub="",size.sub=1,ylim.sub=NA,...){
    par("bg"="white")
    close.screen(all=TRUE)

    if(!print.cal & !print.traj & !print.sub){ #...
        plot(1,main="No Graph",axes=FALSE,type="n",xlab="",ylab="")
        printScreen <- c(NA,NA,NA)
    }else{
        if(print.cal & !print.traj & !print.sub){  # T..
            printScreen <- c(split.screen(c(1,1)),NA,NA)
        }else{
            if(!print.cal & print.traj & !print.sub){  #.T.
                printScreen <- c(NA,split.screen(c(1,1)),NA)
            }else{
                if(!print.cal & !print.traj & print.sub){  #..T
                    printScreen <- c(NA,NA,split.screen(c(1,1)))
                }else{
                    if(print.cal & print.traj & !print.sub){   #TT.
                        printScreen <- c(split.screen(c(1,2)),NA)
                    }else{
                        if(print.cal & !print.traj & print.sub){   #T.T
                            sc <- split.screen(c(1,2))
                            printScreen <- c(sc[1],NA,sc[2])
                        }else{
                            if(!print.cal & print.traj & print.sub){   #.TT
                                printScreen <- c(NA,split.screen(c(1,2)))
                            }else{
                                if(print.cal & print.traj & print.sub){    #TTT
                                    sc1 <- split.screen(c(1,2))
                                    sc2 <- split.screen(c(2,1))
                                    printScreen <- c(sc2,sc1[2])
                                }else{}
                            }
                        }
                    }
                }
            }
        }
    }

    if(missing(y)){
        if(all(is.tna(x["calinski"]))){#only true NA => no cluster define => plot without subgroups

            if(print.traj){
                screen(printScreen[2])
                plot(x=x,col=col,type=type,col.mean=col.mean,type.mean=type.mean,main=main,size=size,ylim=NA,...)
            }else{}
            if(print.sub){
                screen(printScreen[3])
                plotSubGroups(x=x,col=col.sub,type=type.sub,col.mean=col.mean.sub,type.mean=type.mean.sub,main=main.sub,size=size.sub,ylim=ylim.sub,...)
            }else{}
            if(print.cal){
                screen(printScreen[1])
                plotCriterion(x,allCrit=allCrit,nbCriterion=nbCriterion)
            }else{}
            return(printScreen)

        }else{
            y <- c(which.max(x["calinski"][,1]),1)
        }
    }else{}
    if(length(y)==1){y<-c(y,1)}else{}
    if(print.traj){
        screen(printScreen[2])
        plot(x=x,y=y,col=col,type=type,col.mean=col.mean,type.mean=type.mean,main=main,size=size,ylim=NA,...)
    }else{}
    if(print.sub){
        screen(printScreen[3])
        plotSubGroups(x=x,y=y,col=col.sub,type=type.sub,col.mean=col.mean.sub,type.mean=type.mean.sub,main=main.sub,size=size.sub,ylim=ylim.sub,...)
    }else{}
    if(print.cal){
        screen(printScreen[1])
        plotCriterion(x,nbCriterion=nbCriterion,allCrit=allCrit)
    }else{}
    return(printScreen)
}
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotAll",signature=c(x="ClusterizLongData"),def=.ClusterizLongData.plotAll)



updateClusterization <- function(object){
    return(
        new("Clusterization",clusters=object@clusters,nbClusters=object@nbClusters,
             percentEachCluster=object@percentEachCluster,convergenceTime=object@convergenceTime,
             criterionName="calinski",criterionValue=object@calinski,imputationMethod="copyMean",
             startingCondition="randomAll",algorithmUsed="kmeans"
        )
    )
}

updateClusterizLongData <- function(object){
    newObject <- new("ClusterizLongData",id=as.character(object@id),time=object@time,varName=object@varName,traj=object@traj,trajMinSize=object@trajSizeMin)
    for(i in 2:25){
        newObject@clusters[[i]] <- lapply(object@clusters[[i]],updateClusterization)
    }
    return(newObject)
}

