pkgname <- "kml"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('kml')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ClusterizLongData-class")
### * ClusterizLongData-class

flush(stderr()); flush(stdout())

### Name: ClusterizLongData-class
### Title: ~ Class: ClusterizLongData ~
### Aliases: ClusterizLongData ClusterizLongData-class
###   [,ClusterizLongData-method [<-,ClusterizLongData-method
### Keywords: classes chron spatial classif cluster nonparametric ts

### ** Examples

################
### Creation of some trajectories
mat <- matrix(c(1,2,3,1,NA,6,1,8,NA),3)

################
### Creation of LongitudinalData
clustLd <- new("ClusterizLongData",id=c("1","2","3"),time=c(2,4,8),varName="Age",traj=mat,trajMinSize=2)

################
### get and set
clustLd["id"]
clustLd["time"]<- c(1,3,9)
clustLd["varName"]
clustLd["traj"]
clustLd["traj"][3,]<-c(2,7,9)

################
### Creation of a clusterization
part <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1)])
clus <- clusterization(xPartition=part,yLongData=clustLd)

################
### Adding a clusterization to a clusterizLongData
(clustLd["clusters","add"] <- clus)

################
### Removing all the clusterization from a clusterizLongData
clustLd["clusters","clear"] <- "all"
(clustLd)



cleanEx()
nameEx("Clusterization-class")
### * Clusterization-class

flush(stderr()); flush(stdout())

### Name: Clusterization-class
### Title: ~ Class: Clusterization ~
### Aliases: Clusterization Clusterization-class [,Clusterization-method
###   [,Clusterization
### Keywords: classes classif cluster nonparametric ts

### ** Examples
showClass("Clusterization")


cleanEx()
nameEx("affectIndiv")
### * affectIndiv

flush(stderr()); flush(stdout())

### Name: affectIndiv
### Title: ~ Function: affectIndiv ~
### Aliases: affectIndiv

### ** Examples

#######################
### affectIndiv

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
affectIndiv(traj,center)




cleanEx()
nameEx("affectIndivGeneralized")
### * affectIndivGeneralized

flush(stderr()); flush(stdout())

### Name: affectIndivGeneralized
### Title: ~ Function: affectIndivGeneralized ~
### Aliases: affectIndivGeneralized
### Keywords: methods

### ** Examples

#######################
### affectIndiv

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Distance unusual
distCor <- function(x,y){return(cor(x,y))}

### Affectation of each individual
affectIndivGeneralized(traj,center,distance=distCor)



cleanEx()
nameEx("as.clusterizLongData")
### * as.clusterizLongData

flush(stderr()); flush(stdout())

### Name: as.clusterizLongData
### Title: ~ Function: as.clusterizLongData (or as.cld) ~
### Aliases: as.cld as.clusterizLongData
###   as.clusterizLongData,data.frame-method
### Keywords: datagen chron spatial ts

### ** Examples

### Simple use
dn <- data.frame(i=11:13,size12=c(15,13,14),size14=c(16,15,17),size18=c(18,16,16))
as.clusterizLongData(dn)

### Changing parameters
as.clusterizLongData(dn,i=c("H101","H108","B103"),timeCol=c(2,2,3,3,3,3,4),timeReal=12:18)



cleanEx()
nameEx("calculCenterGeneralized")
### * calculCenterGeneralized

flush(stderr()); flush(stdout())

### Name: calculCenterGeneralized
### Title: ~ Function: calculCenterGeneralized ~
### Aliases: calculCenterGeneralized

### ** Examples

#######################
### calculCenterGeneralized

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### A partition
part <- partition(floor(runif(200,1,5)),4)

### Clusters center
calculCenterGeneralized(traj,part,medianNA)



cleanEx()
nameEx("calculMean")
### * calculMean

flush(stderr()); flush(stdout())

### Name: calculMean
### Title: ~ Function: calculMean ~
### Aliases: calculMean

### ** Examples

#######################
### calculMean

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### A partition
part <- partition(floor(runif(200,1,5)),4)

### Clusters center
calculMean(traj,part)



cleanEx()
nameEx("choice")
### * choice

flush(stderr()); flush(stdout())

### Name: choice
### Title: ~ Function: choice ~
### Aliases: choice choice-methods choice,ClusterizLongData-method
### Keywords: iplot chron spatial classif cluster nonparametric ts

### ** Examples

### Creation of articficial data
cld1 <- as.cld(gald())

### Clusterisation
#kml(cld1,nbRedrawing=3,printCal=TRUE,printTraj=TRUE)

### Selection of the clusterization we want
#     (note that "try" is for compatibility with CRAN only,
#     you probably can use "choice(cld1)")
try(choice(cld1))



cleanEx()
nameEx("clusterizLongData")
### * clusterizLongData

flush(stderr()); flush(stdout())

### Name: clusterizLongData
### Title: ~ Function: clusterizLongData (or cld) ~
### Aliases: cld clusterizLongData
###   clusterizLongData,ANY,ANY,ANY,ANY,ANY-method
###   clusterizLongData,missing,missing,missing,missing,missing-method
### Keywords: classes

### ** Examples

################
### Creation of some trajectories
mat <- matrix(c(1,2,3,1,NA,6,1,8,NA),3)

################
### Creation of LongitudinalData
(ld1 <- cld(traj=mat,id=1:3,time=c(1,2,3),varName="V",trajMinSize=2))
(ld2 <- clusterizLongData(traj=mat,id=c("A-101","A-102","A-108"),time=c(2,4,8),varName="Age",trajMinSize=2))



cleanEx()
nameEx("clusterization")
### * clusterization

flush(stderr()); flush(stdout())

### Name: clusterization
### Title: ~ Function: clusterization ~
### Aliases: clusterization

### ** Examples

### Creation of a partition
part <- partition(rep(c(1,2),4),2)

### Some trajectories
traj1 <- gald(nbEachClusters=2)

### Tranformation of part into a Clusterization
clusterization(part,traj1)
# Calinski criterion is arround 0.50...



### Some other trajectories
traj2 <- gald(nbEachClusters=4, functionClusters=list(function(t){5-t},function(t){5+t}))

### Tranformation of part into a Clusterization
clusterization(part,traj2)
# Calinski criterion is arround 0.15...

# part is probably a good partition for traj1, but not for traj2...



cleanEx()
nameEx("exportClusterization")
### * exportClusterization

flush(stderr()); flush(stdout())

### Name: exportClusterization
### Title: ~ Function: exportClusterization ~
### Aliases: exportClusterization exportClusterization,ClusterizLongData
###   exportClusterization,ClusterizLongData-method

### ** Examples

#############
### Creating a ClusterizLongData object, with 3 cluterizations (5 clusters each)
dn <- as.cld(gald())
kml(dn,5,3)

### Exporting the second clusterization in pdf format
try(exportClusterization(dn,c(5,2),typeGraph="pdf"))



cleanEx()
nameEx("kml-package")
### * kml-package

flush(stderr()); flush(stdout())

### Name: kml-package
### Title: ~ Overview: K-means for Longitudinal data ~
### Aliases: kml-package
### Keywords: package dplot iplot chron spatial classif cluster
###   nonparametric ts robust models

### ** Examples

### 1. Data Preparation
myCld <- as.clusterizLongData(generateArtificialLongData())

### 2. Building "optimal" clusterization (with only 3 redrawings)
kml(myCld,nbRedrawing=3,print.cal=TRUE,print.traj=TRUE)

### 3. Exporting results
try(choice(myCld))



cleanEx()
nameEx("kml")
### * kml

flush(stderr()); flush(stdout())

### Name: kml
### Title: ~ Algorithm kml: K-means for Longitidinal data ~
### Aliases: kml kml-method kml,ClusterizLongData-method
### Keywords: dplot chron spatial classif cluster nonparametric ts robust

### ** Examples

### Generation of some data
cld1 <- as.cld(generateArtificialLongData())

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
#     We want to "see" what happen (so printCal and printTraj are TRUE)
kml(cld1,2:6,3,printCal=TRUE,printTraj=TRUE)

### 4 seems to be the best. But to be sure, we try more redrawing 4 or 6 only.
#     We don't want to see again, we want to get the result as fast as possible.
kml(cld1,c(4,6),10)



cleanEx()
nameEx("partitionInitialise")
### * partitionInitialise

flush(stderr()); flush(stdout())

### Name: partitionInitialise
### Title: ~ Function: partitionInitialise ~
### Aliases: partitionInitialise partitionInitialise,numeric,numeric
###   partitionInitialise,numeric,numeric-method

### ** Examples

par(ask=TRUE)
###################
### Constrution of some longitudinal data
dn <- as.cld(gald())
plot(dn,type.mean="n",col=1)

###################
### partition using randamAll
pa1a <- partitionInitialise(3,lengthPart=200,method="randomAll")
plot(dn,pa1a)
pa1b <- partitionInitialise(3,lengthPart=200,method="randomAll")
plot(dn,pa1b)

###################
### partition using randamAll
pa2a <- partitionInitialise(3,lengthPart=200,method="randomK")
plot(dn,pa2a)
pa2b <- partitionInitialise(3,lengthPart=200,method="randomK")
plot(dn,pa2b)

###################
### partition using maxDist
pa3 <- partitionInitialise(3,lengthPart=200,method="maxDist",
    matrixDist=as.matrix(dist(dn["traj"])))
plot(dn,pa3)
### maxDist is deterministic, so no need for a second example

###################
### Example to illustrate "maxDist" method on classical clusters
point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))
plot(points,main="Some points")

paInit <- partitionInitialise(2,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points,main="Two farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points,main="Three farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

paInit <- partitionInitialise(4,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points, main="Four farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot,ClusterizLongData
### Title: ~ Function: plot for ClusterizLongData ~
### Aliases: plot plot-method plot,ClusterizLongData
###   plot,ClusterizLongData,ANY-method
###   plot,ClusterizLongData,missing-method
###   plot,ClusterizLongData,numeric-method
### Keywords: dplot iplot chron spatial classif cluster ts

### ** Examples

clusLd <- as.cld(gald())
kml(clusLd,,1)
par(ask=TRUE)

### Default ploting
plot(clusLd)

### Only the trajectories in black
plot(clusLd,type="n",col.mean="1",type.mean="l")

### Only the mean trajectories, with letters (for publication ?)
plot(clusLd,type="n",col.mean="1",type.mean="b",cex=2)

### All at once.
plot(clusLd,col="clusters",col.mean="clusters",type.mean="l",legend=FALSE)

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plotAll")
### * plotAll

flush(stderr()); flush(stdout())

### Name: plotAll,ClusterizLongData
### Title: ~ Function: plotAll for ClusterizLongData ~
### Aliases: plotAll plotAll,ClusterizLongData
###   plotAll,ClusterizLongData-method
### Keywords: dplot iplot chron spatial classif cluster ts

### ** Examples

##################
### Data construction
dn <- as.cld(gald())
kml(dn,2:5,5)
kml(dn,16,5)
par(ask=TRUE)

### Default ploting
plotAll(dn)
plotAll(dn,legend=FALSE)

### Only the calinski criterion (same effect than plotCalinski(ld))
plotAll(dn,print.cal=TRUE,print.traj=FALSE,print.sub=FALSE)

### Groups and sub-groups
plotAll(dn,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE,legend=FALSE)
plotAll(dn,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE,col="black",type.mean="n",legend=FALSE)

### All at once
plotAll(dn,print.cal=TRUE,print.traj=TRUE,print.sub=TRUE)

### To see the clusterization with only 2 clusters
plotAll(dn,2,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE)

### To see the third clusterization with 16 clusters
plotAll(dn,c(16,3),print.cal=FALSE,type.mean="b",type="n")

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plotAllCriterion")
### * plotAllCriterion

flush(stderr()); flush(stdout())

### Name: plotAllCriterion
### Title: ~ Function: plotAllCriterion ~
### Aliases: plotAllCriterion plotAllCriterion-method
###   plotAllCriterion,ClusterizLongData
###   plotAllCriterion,ClusterizLongData-method

### ** Examples

#################
### Data generation
dn <- as.cld(gald())

### Trying several clusters number and several starting condition
kml(dn)

### Display the quality criterion, both way :
plotAllCriterion(dn)



cleanEx()
nameEx("plotCriterion")
### * plotCriterion

flush(stderr()); flush(stdout())

### Name: plotCriterion
### Title: ~ Function: plotCriterion ~
### Aliases: plotCriterion plotCriterion-method
###   plotCriterion,ClusterizLongData
###   plotCriterion,ClusterizLongData-method

### ** Examples

#################
### Data generation
dn <- as.cld(gald())

### Trying several clusters number and several starting condition
kml(dn)

### Display the quality criterion, both way :
par(mfrow=c(1,2))
plotCriterion(dn)
plotCriterion(dn,allCrit=TRUE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("updateClusterizLongData")
### * updateClusterizLongData

flush(stderr()); flush(stdout())

### Name: updateClusterizLongData
### Title: ~ Function: updateClusterizLongData ~
### Aliases: updateClusterizLongData
### Keywords: methods

### ** Examples

####################
### Not executable
### if oldObject is your old data :
###
#
# newObject <- updateClusterizLongData(oldObject)
#



cleanEx()
nameEx("updateClusterization")
### * updateClusterization

flush(stderr()); flush(stdout())

### Name: updateClusterization
### Title: ~ Function: updateClusterization ~
### Aliases: updateClusterization
### Keywords: methods

### ** Examples

####################
### Not executable
### if oldObject is your old data :
###
#
# newObject <- updateClusterization(oldObject)
#



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
