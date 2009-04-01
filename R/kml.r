cat("\n###################################################################
##################### Class ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")


.partitionInitialise <- function(nbClusters,lengthPart,method="randomK",matrixDist){
    switch(method,
        "randomK"={
            part <- rep(NA,lengthPart)
            seeds <- sample(lengthPart,nbClusters)
            part[seeds] <- LETTERSletters[1:nbClusters]
        },
        "randomAll"={
            part <- floor(runif(lengthPart,1,nbClusters+1))       # Chaque individu recoit une affectation
            seeds <- sample(lengthPart,nbClusters)                # Puis on choisit k individus pour éviter les clusters vides.
            part[seeds] <- 1:nbClusters
        },
        "maxDist"={
            part <- rep(NA,lengthPart)
            seeds <- which(matrixDist==max(matrixDist,na.rm=TRUE),arr.ind=TRUE)[1,]
            while(length(seeds)<nbClusters){
                matrixDist[,seeds] <- 0
	        seeds <- c(seeds,
                    which.max(
                        apply(matrixDist[seeds,],2,min)
                    )[1]
                )
            }
            part[seeds] <- LETTERSletters[1:nbClusters]
        },
        stop("[PartitionInitialize] invalid initialization methods")
    )
    return(partition(clusters=part,nbClusters=nbClusters))
}
setMethod("partitionInitialise",signature=c("numeric","numeric"),.partitionInitialise)


calculCenterGeneralized <- function(traj,xPart,centerMethod=meanNA){
    clustersCenter <- as.matrix(aggregate(traj,by=list(xPart["clusters"]),FUN=centerMethod)[,-1])
    clustersCenter <- rbind(clustersCenter,matrix(NA,ncol=ncol(clustersCenter),nrow=xPart["nbClusters"]-nrow(clustersCenter)))
    return(clustersCenter)
}

calculMean  <- function(traj,xPart){
#    print(as.integer(xPart["clusters"]))
    # L'initialisation de trajMean sous R a 0 est indispensable.
    trajMean <- matrix(0,xPart["nbClusters"],ncol(traj))
    result <- .C("calculMean",mTraj=as.double(t(traj)),iNbInd=as.integer(nrow(traj)),iNbTime=as.integer(ncol(traj)),
        vClusterAffectation=as.integer(xPart["clusters"]),iNbCluster=as.integer(xPart["nbClusters"]),
        mTrajMean=as.numeric(t(trajMean)),NAOK=TRUE)$mTrajMean

    return(matrix(result,xPart["nbClusters"],byrow=TRUE))
}

#traj<-ld2["traj"]
#clustersCenter<- cent2an
#distance=distEuclideGower
#power=2

### On suppose que si un centre est NA, il est en dernière ligne de clustersCenter
affectIndivGeneralized <- function(traj,clustersCenter,distance=function(x,y){return(dist(t(cbind(x,y))))}){
#    if (distance %in% METHODS){distanceFun <- ,method=distance))}}else{distanceFun <- distance}
    nbId <- nrow(traj)
    clusterAffectation <- rep(1,nbId)
    distActuel <- apply(traj,1,function(x){distance(x,clustersCenter[1,])})
 #   print(distActuel)
    for(iNbClusters in 2:nrow(clustersCenter)){
        distToMean <- apply(traj,1,function(x){distance(x,clustersCenter[iNbClusters,])})
 #       print(distToMean)
        cond <- distToMean<distActuel
        cond[is.na(cond)] <- FALSE # Car si cond==NA, c'est que distToMean==NA et donc on ne change pas l'affectation.
        clusterAffectation <- ifelse(cond,rep(iNbClusters,nbId),clusterAffectation)
        distActuel <- ifelse(distToMean<distActuel,distToMean,distActuel)
    }
    return(partition(clusterAffectation,nrow(clustersCenter)))
}

affectIndiv <- function(traj,clustersCenter,distance="euclidean",power=2){
    nbId <- nrow(traj)
    part <- rep(0,nrow(traj))
    power <- c(1,2,power,Inf,-1,-2)[[pmatch(distance,c("manhattan","euclidean","minkowski","maximum","canberra","binary"))]]
    result <- .C("affecteIndiv",mTraj=as.double(t(traj)),iNbInd=as.integer(nbId),iNbTime=as.integer(ncol(traj)),
        mTrajMean=as.numeric(t(clustersCenter)),iNbCluster=as.integer(nrow(clustersCenter)),power=as.numeric(power),
        vClusterAffectation=as.integer(part),NAOK=TRUE
    )
#    print(result)
    return(partition(result$vClusterAffectation,nrow(clustersCenter)))
}


#traj <- ld4n["traj"];clusterAffectation=partitionInitialise(5,180);screenPlot=1;distance="manathan";centerMethod=meanNA
#maxIt=200;screenPlot=NA;nbId=nrow(traj);nbTime=ncol(traj)
#power=2;centerMethod=meanNA
#distance=function(x,y)dist(rbind(x,y))

trajKmlSlow <- function(traj,clusterAffectation,nbId=nrow(traj),nbTime=ncol(traj),maxIt=200,screenPlot=NA,
   distance=function(x,y)dist(rbind(x,y)),centerMethod=meanNA,...
){
#    if (distance %in% METHODS){distanceFun <- function(x,y){return(dist(t(cbind(x,y)),method=distance))}}else{distanceFun <- distance}
 #   print(distanceFun)
    exClusterAffectation <- partition()
    for(iterations in 1:maxIt){
        clustersCenter <- calculCenterGeneralized(traj=traj,xPart=clusterAffectation,centerMethod=centerMethod)
        clusterAffectation <- affectIndivGeneralized(traj=traj,clustersCenter=clustersCenter,distance=distance)
        if(identical(clusterAffectation,exClusterAffectation)){
            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
        }else{
            exClusterAffectation <- clusterAffectation
        }
        if(!is.na(screenPlot)){
            ld <- longData(traj=traj,id=1:nrow(traj),time=1:ncol(traj))
            screen(screenPlot)
            plot(ld,clusterAffectation,col="clusters",col.mean="clusters",main=paste("Iteration =",iterations),...)
        }else{}
    }
    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
}
#trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,centerMethod=meanNA)



###
#Object <- a
#nbClusters=2:4;nbRedrawing=3;saveFreq=100;maxIt=200;trajMinSize=2;
#    print.cal=FALSE;print.traj=TRUE;
#    distance=function(x,y)dist(rbind(x,y));
#distance="euclidean"
#imputationMethod="copyMean"
#centerMethod=meanNA;startingCond="allMethods";distanceStartingCond="euclidean";power=2



.clusterizLongData.kml <- function(Object,nbClusters=2:6,nbRedrawing=20,saveFreq=100,maxIt=200,trajMinSize=2,
    print.cal=FALSE,print.traj=FALSE,imputationMethod="copyMean",
    distance="euclidean",power=2,centerMethod=meanNA,startingCond="allMethods",distanceStartingCond="euclidean",...
){
    nbIdFull <- nrow(Object["traj"])
    convergenceTime <- 0
    noNA<-selectSupTrajMinSize(Object,trajMinSize)
    trajNoNA <- Object["traj"][noNA,]
    nbTime <- length(Object["time"])
    nbId <- nrow(trajNoNA)
    saveCld <-0
    if(print.cal|print.traj){
        scr <- plotAll(Object,print.cal=print.cal,print.traj=print.traj,print.sub=FALSE,col="black",type.mean="n")
    }else{}

    ################
    ### Starting conditions
    if(length(startingCond)==1){
        if(startingCond=="allMethods"){
            startingCond <- c("maxDist","randomAll",rep("randomK",nbRedrawing))[1:nbRedrawing]
        }else{
            startingCond <- rep(startingCond,nbRedrawing)
        }
    }else{}

    ################
    ### Fast or Slow, according to distance and to print.traj
#    if(missing(distance)){distance<-"euclidean"}
    if(is.character(distance)){distInt <- pmatch(distance,METHODS)}else{distInt <- NA}
    if(print.traj){
        cat(" ~ Slow KmL ~\n")
        fast <- FALSE
        screenPlot <- scr[2]
        if(!is.na(distInt)){ ### Distance classique a expliciter dans le cas slow
            distanceSlow <- function(x,y){dist(rbind(x,y),method=distance)}
        }else{
            distanceSlow <- distance
        }
    }else{
        screenPlot <- NA
        if(is.na(distInt)){ ### Distance non classique
            cat(" ~ Slow KmL ~\n")
            fast <- FALSE
            distanceSlow <- distance
        }else{
            cat(" ~ Fast KmL ~\n")
            fast <- TRUE
        }
    }

    nameObject<-deparse(substitute(Object))
#    uniqueTraj <- unique(trajNoNA)

    for(iRedraw in 1:nbRedrawing){
        for(iNbClusters in nbClusters){
            saveCld <- saveCld+1
            clustersInit <- partitionInitialise(nbClusters=iNbClusters,method=startingCond[iRedraw],
                lengthPart=nbId,matrixDist=as.matrix(dist(trajNoNA,method=distanceStartingCond)))
            clust <- rep(NA,nbIdFull)
            if(fast){
                resultKml <- .C("kml1",as.double(t(trajNoNA)),iNbInd=as.integer(nbId),iNbTime=as.integer(nbTime),
                                iNbCluster=as.integer(iNbClusters),maxIt=as.integer(maxIt),
                                distance=as.integer(distInt),power=as.numeric(power),vClusterAffectation1=as.integer(clustersInit["clusters"]),
                                convergenceTime=as.integer(convergenceTime),
                                NAOK=TRUE,PACKAGE="kml")[c(8,9)]
                clust[noNA] <- resultKml[[1]]
            }else{
                resultKml <- trajKmlSlow(traj=trajNoNA,clusterAffectation=clustersInit,nbId=nbId,nbTime=nbTime,maxIt=maxIt,screenPlot=scr[2],
                                distance=distanceSlow,centerMethod=centerMethod,...)
                clust[noNA] <- resultKml[[1]]["clusters"]
            }
            yPartition <- ordered(partition(nbClusters=iNbClusters,clusters=clust))
            Object["clusters"] <- clusterization(yLongData=as(Object,"LongData"),
                            xPartition=yPartition,convergenceTime=resultKml[[2]],imputationMethod=imputationMethod,startingCondition=startingCond[iRedraw],algorithmUsed="kml")

            assign(nameObject,Object,envir=parent.frame())
            cat("*")
            if(saveCld>=saveFreq){
                save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
                saveCld <- 0
                cat("\n")
            }else{}
            if(print.cal){
                screen(scr[1])
                plotCriterion(Object,all=TRUE)
            }else{}
        }
    }
    save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
    return(invisible())
}
setMethod("kml","ClusterizLongData",.clusterizLongData.kml)



.Clusterization.export <- function(object,y,typeGraph="bmp",
    col="clusters",type="l",
    col.mean="clusters",type.mean="b",main="",cex=1,
    pch.mean="letters",pch.time=NA,...#,legends=TRUE,...
){
    part <- object["clusters"][[y]]
    nameObject <- paste(deparse(substitute(object)),"-C",y[1],"-",y[2],sep="")
    write.csv2(data.frame(id=object["id"],clusters=part["clusters"]),
               file=paste(nameObject,"-Clusters.csv",sep=""),
               row.names=FALSE)
    detail <- c(part["algorithmUsed"],part["nbClusters"],part["convergenceTime"],part["percentEachCluster"],part["criterionValue"],
       part["imputationMethod"],part["startingCondition"])
    names(detail) <- c("algorithmUsed","nbClusters","convergenceTime",paste("percent",LETTERSletters[1:part["nbClusters"]]),
       part["criterionName"],"imputationMethod","startingCondition")
    write.csv2(detail,
               file=paste(nameObject,"-Details.csv",sep=""),
               row.names=TRUE)
    plot(object,y,col=col,type=type,col.mean=col.mean,type.mean=type.mean,cex=cex,pch.mean=pch.mean,pch.time=pch.time,...)
        #lty=lty,lty.mean=lty.mean,pch=pch,pch.mean=pch.mean,pch.time=pch.time,
        #xlab=xlab,ylab=ylab,ylim=ylim,cex.mean=cex.mean,legends=legends,sizeMin=sizeMin,...)
    savePlot(filename=paste(nameObject,"-Traj",sep=""),type=typeGraph)
    plotSubGroups(object,y,col=col,type=type,col.mean=col.mean,type.mean=type.mean,cex=cex,pch.mean=pch.mean,pch.time=pch.time,...)
#    plotSubGroups(object,y,...)#subGroups=subGroups,type=type,type.mean=type.mean,col=col,col.mean=col.mean,
       # lty=lty,lty.mean=lty.mean,pch=pch,pch.mean=pch.mean,pch.time=pch.time,
       # xlab=xlab,ylab=ylab,ylim=ylim,cex.mean=cex.mean,legends=legends,sizeMin=sizeMin,...)
    savePlot(filename=paste(nameObject,"-SubGroup",sep=""),type=typeGraph)
    return(invisible())
}
setMethod("exportClusterization","ClusterizLongData",.Clusterization.export)




cat("### Method: 'choice' pour clusterizLongData ###\n")
.clusterizLongData.choice <- function(Object,typeGraph="bmp",...){
    par("bg"="white")
    print.cal <- TRUE
    print.traj <- TRUE
    print.sub <- FALSE

    colTrajPossible <- c("clusters","black","black")
    typeTrajPossible <- c("l","l","n")#,levels=c("l","l","n"))
    styleTraj <- 1

    colMeanPossible <- c("clusters","black","clusters","black","clusters","black","black")
    typeMeanPossible <- c("b","b","b","b","l","l","n")
    pchMeanPossible <- c("letters","letters","symbols","symbols","letters","letters","letters")
    styleMeanTraj <- 1

    pch.time <- Object["time"]
    pchFreq <- length(pch.time)

    size <- 1
    nbTime <- length(Object["time"])
    pointCal <- function(z){points(z[2],Object["calinski"][z[1],z[2]],lwd=3,cex=3)}
    calMatrix <- Object["calinski"]
    calSelected <- list()
    y <- c(which.max(calMatrix[,1]),1)
    plotAll(Object,y,print.cal=print.cal,print.traj=print.traj,print.sub=print.sub,...)
    points(y[2],Object["calinski"][y[1],y[2]],pch=19,lwd=5)
    while(TRUE){

        texte <- paste("     ~ Choice : menu ~
 - 'Arrow' : change partition
 - 'Space' : select/unselect a partition
 -    e    : switch trajectoire on/off (",print.traj,")
 -    d    : switch sub groups on/off (",print.sub,")
 -    c    : switch calinski on/off (",print.cal,")
 -    r    : change the trajectories's style (type=",typeTrajPossible[styleTraj],"; col=",colTrajPossible[styleTraj],")
 -    f    : change the mean's style (type=",typeMeanPossible[styleMeanTraj],"; col=",colMeanPossible[styleMeanTraj],"; pch=",pchMeanPossible[styleMeanTraj],")
 -   t/g   : increase the symbol size (Traj=",size,")
 -   y/h   : decrease the number of symbols (",pchFreq,")
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
                               y[1] <- y[1]-1+which.min(is.na(calMatrix[,1][y[1]:52]))
                           }else{}
                       }else{}
                   }else{}
               },
               "Down"  = {
                   if(y[1]<52){
                       y[2]<-1
                       y[1]<-y[1]+1
                       if(is.na(calMatrix[y[1],1])){
                           y[1] <- y[1]-1+which.min(is.na(calMatrix[,1][y[1]:52]))
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

               "e" = {print.traj <- !print.traj},
               "d" = {print.sub <- !print.sub},
               "c" = {print.cal <- !print.cal},

               "r" = {styleTraj <- styleTraj%%3+1},
               "f" = {styleMeanTraj <- styleMeanTraj%%7+1},

               "t" = {size <- size+0.1},
               "g" = {size <- size-0.1},

               "y" = {
                   pchFreq <- ceiling(pchFreq*1.05)
                   if(pchFreq>nbTime){pchFreq <- nbTime}else{}
                   pch.time <- Object["time"][seq(1,nbTime,length.out=pchFreq)]
               },
               "h" = {
                   pchFreq <- floor(pchFreq/1.05)
                   if(pchFreq<2){pchFreq <- 2}else{}
                   pch.time <- Object["time"][seq(1,nbTime,length.out=pchFreq)]
               },

               default={}
               )
                                        #        erase.screen()
        printScreen <- plotAll(Object,y,print.cal=print.cal,print.traj=print.traj,print.sub=print.sub,
                               all=TRUE,
                               col=colTrajPossible[styleTraj],type=typeTrajPossible[styleTraj],
                               col.mean=colMeanPossible[styleMeanTraj],type.mean=typeMeanPossible[styleMeanTraj],main="",cex=size,
                               pch.mean=pchMeanPossible[styleMeanTraj],pch.time=pch.time,
                               col.sub=colTrajPossible[styleTraj],type.sub=typeTrajPossible[styleTraj],
                               col.mean.sub=colMeanPossible[styleMeanTraj],type.mean.sub=typeMeanPossible[styleMeanTraj],main.sub="")

        if(print.cal){
            points(y[2],Object["calinski"][y[1],y[2]],pch=19,lwd=5)
            lapply(calSelected,pointCal)
        }else{}
    }

    close.screen(all=TRUE)
    pchTimeChar <- paste(pch.time,collapse=",")
    nameObject<-deparse(substitute(Object))
    for (iY in calSelected){
        textToCall <- paste('exportClusterization(',nameObject,
            ',c(',iY[1],',',iY[2],
            '),col="',colTrajPossible[styleTraj],'",type="',typeTrajPossible[styleTraj],
            '",col.mean="',colMeanPossible[styleMeanTraj],'",type.mean="',typeMeanPossible[styleMeanTraj],
            '",main="",cex=',size,
            ',pch.mean="',pchMeanPossible[styleMeanTraj],'",pch.time=c(',pchTimeChar,
            '),...,typeGraph="',typeGraph,'")',sep="")
        eval(parse(text=textToCall))
    }
    plotCriterion(Object,all=FALSE)
    savePlot(filename=paste(nameObject,"-Criterion",sep=""),type=typeGraph)
    plotCriterion(Object,all=TRUE)
    savePlot(filename=paste(nameObject,"-CriterionAll",sep=""),type=typeGraph)
    bringToTop(-1)
    return(invisible())
}
setMethod("choice",signature=c("ClusterizLongData"),.clusterizLongData.choice)

