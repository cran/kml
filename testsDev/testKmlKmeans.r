library(longitudinalData)
source("testKml.r")
dd <- function(){dyn.unload("kml.dll")}
ddd <- function(){dyn.load("kml.dll")}

superTest <- function(Object,nbClusters){
    nbRedrawing=100
    saveFreq=100
    maxIt=200
    trajMinSize=2
    print.cal=FALSE
    print.traj=FALSE
    imputationMethod="copyMean"
    distance="euclidean"
    power=2
    centerMethod=meanNA
    startingCond="allMethods"
    distanceStartingCond="euclidean"

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

    ## ##############
    ## Starting conditions
    if(length(startingCond)==1){
        if(startingCond=="allMethods"){
            startingCond <- c("maxDist","randomAll",rep("randomK",nbRedrawing))[1:nbRedrawing]
        }else{
            startingCond <- rep(startingCond,nbRedrawing)
        }
    }else{}

    ## #############
    ## Fast or Slow, according to distance and to print.traj
    ##    if(missing(distance)){distance<-"euclidean"}
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

    iRedraw <- 1
    iNbClusters <- nbClusters

    clustersInit <- partitionInitialise(nbClusters=iNbClusters,method=startingCond[iRedraw],
                                        lengthPart=nbId,matrixDist=as.matrix(dist(trajNoNA,method=distanceStartingCond)))
    clust <- rep(NA,nbIdFull)


    ## Fast
                                        #    ddd()
    s1 <- system.time(for (az in 1:100)resultKml <- .C("kml1",as.double(t(trajNoNA)),iNbInd=as.integer(nbId),iNbTime=as.integer(nbTime),
                    iNbCluster=as.integer(iNbClusters),maxIt=as.integer(maxIt),
                    distance=as.integer(distInt),power=as.numeric(power),
                    vClusterAffectation1=as.integer(clustersInit["clusters"]),
                    convergenceTime=as.integer(convergenceTime),
                    NAOK=TRUE,PACKAGE="kml")[c(8,9)])
    cl1 <- clust[noNA] <- resultKml[[1]]

    ## Slow
    distanceSlow <- function(x,y){dist(rbind(x,y),method=distance)}
    scr <- NA
    s2 <- system.time(resultKml <- trajKmlSlow(traj=trajNoNA,clusterAffectation=clustersInit,nbId=nbId,nbTime=nbTime,maxIt=maxIt,screenPlot=scr[2],
                             distance=distanceSlow,centerMethod=centerMethod))
    cl2 <- clust[noNA] <- resultKml[[1]]["clusters"]

    ## kmeans
    x <- Object["traj"]
    y <-  x[!is.na(clustersInit["clusters"]),]
    s3 <- system.time(for (az in 1:100)cl3 <- kmeans(x,y,iter.max=200,algorithm="Lloyd")[[1]])

    print(table(cl1,cl2))
    print(table(cl1,cl3))
    print(table(cl2,cl3))
    return(list(s1,s2,s3))
}


superTest(cld3,2)
superTest(cld3,3)
superTest(cld3,4)
superTest(cld3,5)
superTest(cld3,6)

superTest(cld4,2)
superTest(cld4,3)
superTest(cld4,4)
superTest(cld4,5)
superTest(cld4,6)


system.time(kml(cld3,2:6,50))


x <- cld3["traj"]
system.time(
            for(i in 2:6){
                j<-0
                while(j<50){
#                    al <- floor(runif(i,1,244))
                    y <-  x[1:i,]
#                    if(class(try(
                                 kmeans(x,y,iter.max=200,algorithm="Lloyd")
 #                                ))!="try-error"){
                        j <- j+1 ; cat("*",j)
  #                  }else{
   #                     cat("=")
    #                }
                }
            }
            )
