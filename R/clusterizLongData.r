cat("\n#######################################################################
######################## Class ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

.ClusterizLongData.validity <- function(object){
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
        clusters="list"
    ),
    contains="LongData",
    prototype=prototype(
        clusters=list(
           c1=list(),c2=list(),c3=list(),c4=list(),c5=list(),c6=list(),c7=list(),c8=list(),c9=list(),c10=list(),
           c11=list(),c12=list(),c13=list(),c14=list(),c15=list(),c16=list(),c17=list(),c18=list(),c19=list(),c20=list(),
           c21=list(),c22=list(),c23=list(),c24=list(),c25=list()
        )
    ),
    validity=.ClusterizLongData.validity
)
rm(.ClusterizLongData.validity)


cat("\n###################################################################
##################### Class ClusterizLongData #####################
########################### Constructeur ##########################
###################################################################\n")

setMethod("clusterizLongData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,id,time,varName,trajSizeMin){new("ClusterizLongData")}
)
setMethod("clusterizLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),
    function(traj,id,time,varName="V",trajSizeMin=1){
        dimnames(traj) <- list(id,paste(varName,time,sep=""))
        new("ClusterizLongData",id=as.character(id),time=time,varName=varName,traj=traj,trajSizeMin=trajSizeMin)
    }
)
cld <- clusterizLongData

setMethod("as.clusterizLongData",signature=c("LongData"),
    function(data,...){
        new("ClusterizLongData",id=data["id"],time=data["time"],varName=data["varName"],traj=data["traj"],trajSizeMin=data["trajSizeMin"])
    }
)

setMethod("as.clusterizLongData",signature=c("data.frame"),
    function(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),
        varName=sub("[[:digit:]]*$", "",names(data)[timeCol[1]]),trajSizeMin=1,...
    ){
        traj <- as.matrix(data[,timeCol])
        dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
        return(clusterizLongData(id=id,time=timeReal,varName=varName,traj=traj,trajSizeMin=trajSizeMin))
    }
)
as.cld <- as.clusterizLongData

cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################ Accesseurs ###########################
################################################################## #\n")

cat("### Getteur ###\n")
setMethod("[","ClusterizLongData",
    function(x,i,j,drop){
        switch(EXPR=i,
            "id"={return(x@id)},
            "varName"={return(x@varName)},
            "time"={return(x@time)},
            "trajSizeMin"={return(x@trajSizeMin)},
            "traj"={
                if(missing(j)){return(x@traj)}else{return(x@traj[j,])}
            },
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
            "calinski"={
                ncolMatrix = max(c(unlist(lapply(x@clusters,length)),1))
                calinskiList <- matrix(NA,nrow=25,ncol=ncolMatrix,dimnames=list(paste("c",1:25,sep=""),list()))
                getCalinskiCriterion <- function(object){object["calinski"]}
                for(i in 1:25){
                    calLine <-lapply(x@clusters[[i]],getCalinskiCriterion)
                    calinskiList[i,] <- c(unlist(calLine),rep(NA,ncolMatrix-length(calLine)))
                }
                class(calinskiList) <- c("calinski",class(calinskiList))
                if(missing(j)){
                    return(calinskiList)
                }else{
                    if(length(j)==1){
                        return(calinskiList[j,])
                    }else{
                        if(length(j)==2){
                            return(calinskiList[j[1],as.integer(j[2])])
                        }else{
                            stop("[ClusterizLongData] : j should be 'cx' or c('cx',y), nothing else")
                        }
                    }
                }
                return(calinskiList)
            },
            stop("[ClusterizLongData:getteur]: there is not such a slot in ClusterizLongData")
        )
    }
)


# Si on veut rendre [<- utilisable pour partition, il faut modifier ICI
cat("### Setteur ###\n")
setReplaceMethod("[","ClusterizLongData",
    function(x,i,j,value){
        switch(EXPR=i,
            "id"={x@id<-as.character(value)},
            "varName"={x@varName<-value},
            "time"={x@time<-value},
            "trajSizeMin"={x@trajSizeMin<-value},
            "traj"={
                if(missing(j)){x@traj<-value}else{x@traj[j,]<-value}
            },
            "clusters"={
                if(missing(j)){j <- "add"}else{}
                switch(EXPR=j,
                    "add"={
                        x@clusters[[value@nbClusters]] <- c(x@clusters[[value@nbClusters]],list(value)) # ICI
                        getCalinskiCriterion <- function(object){object["calinski"]}
                        x@clusters[[value@nbClusters]] <- x@clusters[[value@nbClusters]][
                            order(unlist(lapply(x@clusters[[value@nbClusters]],getCalinskiCriterion)),decreasing=TRUE)
                        ]
                    },
                    "clear"={
                        if(value=="all"){
                            x@clusters <- list(
                                c1=list(),c2=list(),c3=list(),c4=list(),c5=list(),c6=list(),c7=list(),c8=list(),c9=list(),c10=list(),
                                c11=list(),c12=list(),c13=list(),c14=list(),c15=list(),c16=list(),c17=list(),c18=list(),c19=list(),c20=list(),
                                c21=list(),c22=list(),c23=list(),c24=list(),c25=list())
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
    cat("\n~ trajSizeMin :",object@trajSizeMin,"\n")
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

    cat("\n ~ calinski\n")
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
cleanProg(.clusterizLongData.show,,,0)
setMethod("show","ClusterizLongData",.clusterizLongData.show)
rm(.clusterizLongData.show)


plotCalinski <- plot.calinski <- function(x,y,...){
    if(!all(is.na(x))){
        matplot(1:ncol(x),t(x),type="b",xlab="",ylab="",main="Calinski Criterion")
    }else{
        plot(1,main="All Calinski criterion are undefined",type="n")
    }
    return(invisible())
}
cleanProg(plot.calinski,,,0)


cat("### Method: 'plot' pour clusterizLongData ###\n")
.clusterizLongData.plot <- function(x,y,colorTraj="color",colorMean="both",main="",point="no",size=1,...){
    if(missing(y)){
        if(all(is.tna(x["calinski"]))){#only true NA => no cluster define => plot without subgroups
            y <- partition(clusters=rep("A",length(x["id"])),id=x["id"],nbClusters=2)
            plot(as(x,"LongData"),y,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,...)
            return(invisible())
        }else{
            y <- c(which.max(x["calinski"][,1]),1)
        }
    }else{}

    if(length(y)==1){y<-c(y,1)}else{}
    part <- x["clusters",y]
    plot(as(x,"LongData"),part,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,...)
    return(invisible())
}
cleanProg(.clusterizLongData.plot)
#setGenericVerif("plotClusterizLongData",function(x,y,...){standardGeneric("plotClusterizLongData")})
setMethod("plotTraj",c("ClusterizLongData"),.clusterizLongData.plot)
setMethod("plot",c("ClusterizLongData"),.clusterizLongData.plot)
rm(.clusterizLongData.plot)


.ClusterizLongData.plotSubGroups <- function(x,y,colorTraj="color",colorMean="both",main="",point="no",size=1,ylim=NA,...){
    if(missing(y)){
        if(all(is.tna(x["calinski"]))){#only true NA => no cluster define => plot without subgroups
            y <- partition(clusters=rep("A",length(x["id"])),id=x["id"],nbClusters=2)
            plot(as(x,"LongData"),y,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,...)
            return(invisible())
        }else{
            y <- c(which.max(x["calinski"][,1]),1)
        }
    }else{}

    if(length(y)==1){y<-c(y,1)}else{}
    part <- x["clusters",y]
    plotSubGroups(as(x,"LongData"),part,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,...)
    return(invisible())
}
cleanProg(.ClusterizLongData.plotSubGroups,,,2) # LETTERS meanNA
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotSubGroups",signature=c(x="ClusterizLongData"),def=.ClusterizLongData.plotSubGroups)
rm(.ClusterizLongData.plotSubGroups)


.ClusterizLongData.plotAll <- function(x,y,printCal=TRUE,printTraj=TRUE,printSub=FALSE,
                    colorTraj="color",colorMean="both",main="",point="no",size=1,
                    colorTrajSub="color",colorMeanSub="both",mainSub="",pointSub="no",sizeSub=1,ylimSub=NA,...){
    par("bg"="white")
    close.screen(all=TRUE)
    if(!printCal & !printTraj & !printSub){ #...
        plot(1,main="No Graph",axes=FALSE,type="n",xlab="",ylab="")
        printScreen <- c(NA,NA,NA)
    }else{
        if(printCal & !printTraj & !printSub){  # T..
            printScreen <- c(split.screen(c(1,1)),NA,NA)
        }else{
            if(!printCal & printTraj & !printSub){  #.T.
                printScreen <- c(NA,split.screen(c(1,1)),NA)
            }else{
                if(!printCal & !printTraj & printSub){  #..T
                    printScreen <- c(NA,NA,split.screen(c(1,1)))
                }else{
                    if(printCal & printTraj & !printSub){   #TT.
                        printScreen <- c(split.screen(c(1,2)),NA)
                    }else{
                        if(printCal & !printTraj & printSub){   #T.T
                            sc <- split.screen(c(1,2))
                            printScreen <- c(sc[1],NA,sc[2])
                        }else{
                            if(!printCal & printTraj & printSub){   #.TT
                                printScreen <- c(NA,split.screen(c(1,2)))
                            }else{
                                if(printCal & printTraj & printSub){    #TTT
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

    if(printTraj){
        screen(printScreen[2])
        plot(x,y,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,...)
    }else{}
    if(printSub){
        screen(printScreen[3])
        plotSubGroups(x,y,colorTraj=colorTrajSub,colorMean=colorMeanSub,main=mainSub,point=pointSub,size=sizeSub,ylim=ylimSub,...)
    }else{}
    if(printCal){
        screen(printScreen[1])
        plot(x["calinski"])
    }else{}
    return(printScreen)
}
cleanProg(.ClusterizLongData.plotAll,,,0) # LETTERS meanNA
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotAll",signature=c(x="ClusterizLongData"),def=.ClusterizLongData.plotAll)
rm(.ClusterizLongData.plotAll)





cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")


distEuclideGower <- function(x,y){return(dist(t(cbind(x,y))))}
distManathanGower <- function(x,y){return(dist(t(cbind(x,y)),method="manhattan"))}

.clusterizLongData.kmlSlow1 <- function(trajNoNA,nbId,nbTime,clustersCenter,nbClusters,maxIt=200,screenPlot=NA,distance=distEuclideGower){
    exClusterAffectation <- rep(0,nbId)
    for(iterations in 1:maxIt){
        clusterAffectation <- rep(1,nbId)
        distActuel <- rep(Inf,nbId)

        for(iNbClusters in 1:nbClusters){
            distToMean <- apply(trajNoNA,1,function(x){distance(x,clustersCenter[iNbClusters,])})
            clusterAffectation <- ifelse(distToMean<distActuel,rep(iNbClusters,nbId),clusterAffectation)
            distActuel <- ifelse(distToMean<distActuel,distToMean,distActuel)
        }
        if(identical(clusterAffectation,exClusterAffectation)){
            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
        }else{
            exClusterAffectation <- clusterAffectation
        }
        clustersCenter <- as.matrix(aggregate(trajNoNA,by=list(clusterAffectation),FUN=meanNA)[,-1])
        clustersCenter <- rbind(clustersCenter,rep(NA,nbTime*(nbClusters-nrow(clustersCenter))))
        if(!is.na(screenPlot)){
            ld <- longData(traj=trajNoNA,id=1:nrow(trajNoNA),time=1:ncol(trajNoNA),trajSizeMin=2)
            part <- partition(id=1:nbId,clusters=LETTERS[clusterAffectation],nbClusters=nbClusters)
            screen(screenPlot)
            plot(ld,part,colorTraj="color",colorMean="both",main=paste("Iteration =",iterations))
        }else{}
    }
    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
}
cleanProg(.clusterizLongData.kmlSlow1,,,3)  # meanNA which.minNA LETTERS

.clusterizLongData.kml <- function(Object,nbClusters=2:6,nbRedrawing=20,saveFreq=100,maxIt=200,printCal=FALSE,printTraj=FALSE,distance){
    noNA<-selectSupTrajSizeMin(Object)
    trajNoNA <- Object@traj[noNA,]
    nbTime <- length(Object@time)
    nbId <- nrow(trajNoNA)
    saveCld <-0
    scr <- plotAll(Object,printCal=printCal,printTraj=printTraj,printSub=FALSE,colorTraj="black",colorMean="no")
    if(!printTraj && missing(distance)){
        cat(" ~ Fast KmL ~\n")
        fast <- TRUE
    }else{
        cat(" ~ Slow KmL ~\n")
        fast <- FALSE
        if(printTraj){screenPlot <- scr[2]}else{screenPlot <- NA}
    }

    nameObject<-deparse(substitute(Object))
    uniqueTraj <- unique(trajNoNA)
    for(iRedraw in 1:nbRedrawing){
        for(iNbClusters in nbClusters){
            saveCld <- saveCld+1
            clustersCenter <- uniqueTraj[sample(1:nrow(uniqueTraj),iNbClusters),]
            if(fast){
                resultKml <- .C("kml1",as.double(trajNoNA),as.integer(nbId),as.integer(nbTime),
                                as.double(clustersCenter),as.integer(iNbClusters),convergenceTime=as.integer(maxIt),
                                clusterAffectation=integer(nbId),NAOK=TRUE,PACKAGE="kml")[c(7,6)]
            }else{
                resultKml <- .clusterizLongData.kmlSlow1(trajNoNA=trajNoNA,nbId=nbId,nbTime=nbTime,
                                clustersCenter=clustersCenter,nbClusters=iNbClusters,maxIt=maxIt,
                                screenPlot=scr[2],distance=distance)
            }
            yPartition <- partition(id=Object@id[noNA],nbClusters=iNbClusters,clusters=LETTERS[resultKml[[1]]])
            yPartition <- expandPartition(object=yPartition,listId=Object@id)
            Object["clusters"] <- clusterization(xLongData=as(Object,"LongData"),yPartition=yPartition,convergenceTime=resultKml[[2]])

            assign(nameObject,Object,envir=parent.frame())
            cat("*")
            if(saveCld>=saveFreq){
                save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
                saveCld <- 0
                cat("\n")
            }else{}
            if(printCal){
                screen(scr[1])
                plot(Object["calinski"])
            }else{}
        }
    }
    save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
    return(invisible())
}
cleanProg(.clusterizLongData.kml,,,1) # LETTERS
setMethod("kml","ClusterizLongData",.clusterizLongData.kml)
rm(.clusterizLongData.kml)
#kml(cld3,3,1)



.Clusterization.export <- function(object,y,typeGraph="bmp",
                                   colorTraj=colorTraj,colorMean=colorMean,point=point,size=size,main=main,
                                   colorTrajSub=colorTrajSub,colorMeanSub=colorMeanSub,pointSub=pointSub,sizeSub=sizeSub,mainSub=mainSub,...
                                   ){
    part <- object["clusters",y]
    nameObject <- paste(deparse(substitute(object)),"-C",y[1],"-",y[2],sep="")
    write.csv2(data.frame(id=object["id"],clusters=part["clusters"]),
               file=paste(nameObject,"-Clusters",sep=""),
               row.names=FALSE)
    detail <- c(part["nbClusters"],part["calinski"],part["percentEachCluster"])
    names(detail) <- c("nbClusters","calinski",LETTERS[1:part["nbClusters"]])
    write.csv2(detail,
               file=paste(nameObject,"-Details",sep=""),
               row.names=TRUE)
    plot(object,y,colorTraj=colorTraj,colorMean=colorMean,point=point,size=size,main=main,...)
    savePlot(filename=paste(nameObject,"-Traj",sep=""),type=typeGraph)
    plotSubGroups(object,y,colorTraj=colorTrajSub,colorMean=colorMeanSub,point=pointSub,size=sizeSub,main=mainSub,...)
    savePlot(filename=paste(nameObject,"-SubGroup",sep=""),type=typeGraph)
}
cleanProg(.Clusterization.export,,,2) # LETTERS, .GlobalEnv
setMethod("exportClusterization","ClusterizLongData",.Clusterization.export)
rm(.Clusterization.export)


cat("### Method: 'choice' pour clusterizLongData ###\n")
.clusterizLongData.choice <- function(Object,typeGraph="bmp"){
    par("bg"="white")
    printCal <- TRUE
    printTraj <- TRUE
    printSub <- FALSE
    colorTrajPoss <- ordered(c("color","black","no"),levels=c("color","black","no"))
    colorTraj <- colorTrajSub <- colorTrajPoss[1]
    colorMeanPoss <- ordered(c("both","color","black","no"),levels=c("both","color","black","no"))
    colorMean <- colorMeanSub <- colorMeanPoss[1]
    pointPoss <- ordered(c("no","point","letters","symbols"),levels=c("no","point","letters","symbols"))
    point <- pointSub <- pointPoss[1]
    size <- sizeSub <- 1
    pointCal <- function(z){points(z[2],Object["calinski",z],lwd=3,cex=3)}
    calMatrix <- Object["calinski"]
    calSelected <- list()
    y <- c(which.max(calMatrix[,1]),1)
    plotAll(Object,y,printCal=printCal,printTraj=printTraj,printSub=printSub)
    points(y[2],Object["calinski",y],pch=19,lwd=5)
    while(TRUE){

        texte <- paste("     ~ Choice : menu ~
 - 'Arrow' : change partition
 - 'Space' : select/unselect a partition
 -    d    : switch trajectoire on/off (",printTraj,")
 -    c    : switch sub groups on/off (",printSub,")
 -    e    : switch calinski on/off (",printCal,")
 -   r/f   : change the trajectories' color (Traj=",colorTraj,";Sub=",colorTrajSub,")
 -   t/g   : change the mean trajectories' color (Traj=",colorMean,";Sub=",colorMeanSub,")
 -   y/h   : change the mean trajectories' symbols (Traj=",point,";Sub=",pointSub,")
 -   u/j   : increase the symbol size (Traj=",size,";Sub=",sizeSub,")
 -   i/k   : decrease the symbol size
     ~ 'Return' when its done ~\n",sep="")

        choix <- getGraphicsEvent(texte,onKeybd=function(key){return(key)})
        switch(EXP=choix,
               "Up"    = {
                   if(y[1]>1){
                       y[2]<-1
                       y[1]<-y[1]-1
                       if(is.na(calMatrix[y[1],1])){
                           y[1] <- y[1]+1-which.min(is.na(calMatrix[,1][y[1]:1]))
                           if(is.na(calMatrix[y[1],1])){
                               y[1] <- y[1]-1+which.min(is.na(calMatrix[,1][y[1]:25]))
                           }else{}
                       }else{}
                   }else{}
               },
               "Down"  = {
                   if(y[1]<25){
                       y[2]<-1
                       y[1]<-y[1]+1
                       if(is.na(calMatrix[y[1],1])){
                           y[1] <- y[1]-1+which.min(is.na(calMatrix[,1][y[1]:25]))
                           if(is.na(calMatrix[y[1],1])){
                               y[1] <- y[1]+1-which.min(is.na(calMatrix[,1][y[1]:1]))
                           }else{}
                       }else{}
                   }else{}
               },
               "Right" = {if(y[2]<ncol(calMatrix) && !is.na(calMatrix[y[1],y[2]+1])){y[2]<-y[2]+1}else{}},
               "Left"  = {if(y[2]>1){y[2]<-y[2]-1}else{}},

               "ctrl-J" = {break()},
               " "      = {
                   if(list(y) %in% calSelected){
                       calSelected <- calSelected[!(calSelected %in% list(y))]
                   }else{
                       calSelected <- c(calSelected,list(y))
                   }
               },

               "c" = {printCal <- !printCal},
               "e" = {printTraj <- !printTraj},
               "d" = {printSub <- !printSub},

               "r" = {colorTraj <- colorTrajPoss[as.integer(colorTraj)%%3+1]},
               "f" = {colorTrajSub <- colorTrajPoss[as.integer(colorTrajSub)%%3+1]},

               "t" = {colorMean <- colorMeanPoss[as.integer(colorMean)%%4+1]},
               "g" = {colorMeanSub <- colorMeanPoss[as.integer(colorMeanSub)%%4+1]},

               "y" = {point <- pointPoss[as.integer(point)%%4+1]},
               "h" = {pointSub <- pointPoss[as.integer(pointSub)%%4+1]},

               "y" = {point <- pointPoss[as.integer(point)%%3+1]},
               "h" = {pointSub <- pointPoss[as.integer(pointSub)%%3+1]},

               "u" = {size <- size+0.1},
               "j" = {sizeSub <- sizeSub+0.1},
               "i" = {size <- size-0.1},
               "k" = {sizeSub <- sizeSub-+0.1},

               default={}
               )
                                        #        erase.screen()
        printScreen <- plotAll(Object,y,printCal=printCal,printTraj=printTraj,printSub=printSub,
                               colorTraj=colorTraj,colorMean=colorMean,main="",point=point,size=size,
                               colorTrajSub=colorTrajSub,colorMeanSub=colorMeanSub,mainSub="",pointSub=pointSub,sizeSub=sizeSub,ylimSub=NA)

        if(printCal){
            points(y[2],Object["calinski",y],pch=19,lwd=5)
            lapply(calSelected,pointCal)
        }else{}
    }

    close.screen(all=TRUE)
    for (iY in calSelected){
        exportClusterization(Object,iY,colorTraj=colorTraj,colorMean=colorMean,main="",point=point,size=size,
                             colorTrajSub=colorTrajSub,colorMeanSub=colorMeanSub,mainSub="",pointSub=pointSub,sizeSub=sizeSub,typeGraph=typeGraph)
    }
    bringToTop(-1)
    return(invisible())
}
cleanProg(.clusterizLongData.choice,,,0)
setMethod("choice",signature=c("ClusterizLongData"),.clusterizLongData.choice)
rm(.clusterizLongData.choice)
#choice(cld3)

#15H30
#16H21
#
#16H37
#19H00

#trajNoNA <- cld3@traj
#clusters <- p3a@clusters
#nbTime <- 3
#nbId <- 5
#nbClusters <- 2



#cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")


#setGenericVerif("importClusterization",function(xLongData,fileName)standardGeneric("importClusterization"))
#fileName <- "essaiExport2";xLongData <- ld4
#.Clusterization.import <- function(xLongData,fileName){
#    fullFileNameR<-paste(fileName,"-CLUSTERS.csv",sep="")
#    data <- read.table(file=fullFileNameR,header=TRUE,sep=";")
#    nbClusters <- as.numeric(substr(names(data)[2],2,3))
#    data[,2] <- factor(LETTERS[data[,2]],levels=LETTERS[1:nbClusters])
#    return(clusterization(xLongData=xLongData,yPartition=partition(nbClusters=nbClusters,id=data[,1],clusters=data[,2])))
#}
#cleanProg(.Clusterization.import,,,1) #LETTERS
#setMethod("importClusterization","LongData",.Clusterization.import)
#rm(.Clusterization.import)



                                        #exportClusterization(object,"->toto")

### Distance qui prend en compte les manquantes :
###   Il faut modifier l'algo : la moyenne les groupes doit 'comptabiliser' le nombre de manquante, puis le combiner avec la distance autre.
###   function(x,y)
#distEuclideGower <- function(x,y){
#   z <- ifelse(is.na(x)&is.na(y),NA,x-y)
#    return(sqrt(sum(z^2,na.rm=TRUE)*length(z)/sum(!is.na(z))))
#}
#distManathanGower <- function(x,y){
#    z <- ifelse(is.na(x)&is.na(y),NA,x-y)
#    return(sum(abs(z),na.rm=TRUE)*length(z)/sum(!is.na(z)))
#}
#
# distBoolean <- function(x,y)return( (sum(is.na(x)!=is.na(y))) + dist(rbind(x,y))  )
#

#trajNoNA <- cld3@traj
#nbId=243
#nbTime=27
#nbClusters=8
#clustersCenter=cld3@traj[c(1,3,5,15,18,25,28,21),]
#maxIt=200
#screenPlot=NA
#distance=distEuclideGower



#Trois choses :
#    - La beauté
#    - l'export
#    - la lisibilité

#pour la beauté, il faut avoir Cal a gauche, traj + 2/3/4 ou 5 a droite c'est a dire en limitant les subGroups a ABCDE !!! Gagné !!!!!
#
#Idéalement, il faut pouvoir switcher on / off graphique.
#Pour l'export et le choix, il faut proposer toutes les options : cal / traj ; cal /traj / sub...
