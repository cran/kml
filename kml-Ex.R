pkgname <- "kml"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('kml')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ClusterLongData-class")
### * ClusterLongData-class

flush(stderr()); flush(stdout())

### Name: ClusterLongData-class
### Title: ~ Class: ClusterLongData ~
### Aliases: ClusterLongData-class [,ClusterLongData-method
###   [<-,ClusterLongData-method show,ClusterLongData-method
### Keywords: classes chron spatial classif cluster nonparametric ts

### ** Examples

################
### Creation of some trajectories

traj <- matrix(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),6)

myCld <- clusterLongData(
    traj=traj,
    idAll=as.character(c(100,102,103,109,115,123)),
    time=c(1,2,4,8,15),
    varNames="P",
    maxNA=3
)

################
### get and set
myCld["idAll"]
myCld["varNames"]
myCld["traj"]

################
### Creation of a Partition
part2 <- partition(clusters=rep(1:2,3),myCld)
part3 <- partition(clusters=rep(1:3,2),myCld)


################
### Adding a clusterization to a clusterizLongData
myCld["add"] <- part2
myCld["add"] <- part3
myCld



cleanEx()
nameEx("ParKml-class")
### * ParKml-class

flush(stderr()); flush(stdout())

### Name: ParKml-class
### Title: ~ Class: "ParKml" ~
### Aliases: ParKml-class [,ParKml-method [<-,ParKml-method
###   show,ParKml-method
### Keywords: classes

### ** Examples

### Building data
myCld <- gald(15)

### Standard kml
kml(myCld,,2,toPlot="both")

### Using median instead of mean
parWithMedian <- parALGO(centerMethod=function(x){median(x,na.rm=TRUE)})
kml(myCld,,2,toPlot="both",parAlgo=parWithMedian)

### Using distance max
parWithMax <- parALGO(distanceName="maximum")
kml(myCld,,2,toPlot="both",parAlgo=parWithMax)



cleanEx()
nameEx("affectFuzzyIndiv")
### * affectFuzzyIndiv

flush(stderr()); flush(stdout())

### Name: affectFuzzyIndiv
### Title: ~ Function: affectFuzzyIndiv ~
### Aliases: affectFuzzyIndiv

### ** Examples

#######################
### affectFuzzyIndiv

### Some LongitudinalData
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
affectFuzzyIndiv(traj,center)



cleanEx()
nameEx("affectIndiv")
### * affectIndiv

flush(stderr()); flush(stdout())

### Name: affectIndiv
### Title: ~ Functions: affectIndiv & affectIndivC ~
### Aliases: affectIndiv affectIndivC

### ** Examples

#######################
### affectIndiv

### Some trajectories
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
system.time(part <- affectIndiv(traj,center))
system.time(part <- affectIndivC(traj,center))



cleanEx()
nameEx("calculTrajFuzzyMean")
### * calculTrajFuzzyMean

flush(stderr()); flush(stdout())

### Name: calculTrajFuzzyMean
### Title: ~ Function: calculTrajFuzzyMean ~
### Aliases: calculTrajFuzzyMean

### ** Examples

#######################
### calculTrajFuzzyMean

### Some LongitudinalData
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Affectation of each individual
membership <- affectFuzzyIndiv(traj,center)

### Computation of the mean's trajectories
calculTrajFuzzyMean(traj,membership)



cleanEx()
nameEx("calculTrajMean")
### * calculTrajMean

flush(stderr()); flush(stdout())

### Name: calculTrajMean
### Title: ~ Functions: calculTrajMean & calculTrajMeanC ~
### Aliases: calculTrajMean calculTrajMeanC

### ** Examples

#######################
### calculMean

### Some trajectories
traj <- gald()["traj"]

### A cluster affectation
clust <- initializePartition(3,200,"randomAll")

### Computation of the cluster's centers
system.time(centers <- calculTrajMean(traj,clust))
system.time(centers <- calculTrajMeanC(traj,clust))



cleanEx()
nameEx("choice")
### * choice

flush(stderr()); flush(stdout())

### Name: choice
### Title: ~ Function: choice ~
### Aliases: choice choice-methods choice,ClusterLongData-method
### Keywords: iplot chron spatial classif cluster nonparametric ts

### ** Examples

### Creation of articficial data
cld1 <- gald()

### Clusterisation
kml(cld1,nbRedrawing=3,toPlot='both')

### Selection of the clustering we want
#     (note that "try" is for compatibility with CRAN only,
#     you probably can use "choice(cld1)")
try(choice(cld1))



cleanEx()
nameEx("clusterLongData")
### * clusterLongData

flush(stderr()); flush(stdout())

### Name: clusterLongData
### Title: ~ Function: clusterLongData (or cld) ~
### Aliases: cld clusterLongData clusterLongData,ANY,ANY,ANY,ANY,ANY,ANY
###   clusterLongData,ANY,ANY,ANY,ANY,ANY,ANY-method
###   clusterLongData,missing,missing,missing,missing,missing,missing
###   clusterLongData,missing,missing,missing,missing,missing,missing-method
### Keywords: classes

### ** Examples

#####################
### From matrix

### Small data
mat <- matrix(c(1,NA,3,2,3,6,1,8,10),3,3,dimnames=list(c(101,102,104),c("T2","T4","T8")))
clusterLongData(mat)
(ld1 <- clusterLongData(traj=mat,idAll=as.character(c(101,102,104)),time=c(2,4,8),varNames="V"))
plot(ld1)

### Big data
mat <- matrix(runif(1051*325),1051,325)
(ld2 <- clusterLongData(traj=mat,idAll=paste("I-",1:1051,sep=""),time=(1:325)+0.5,varNames="Random"))

####################
### From data.frame

dn <- data.frame(id=1:3,v1=c(NA,2,1),v2=c(NA,1,0),v3=c(3,2,2),v4=c(4,2,NA))

### Basic
clusterLongData(dn)

### Selecting some times
(ld3 <- clusterLongData(dn,timeInData=c(1,2,4),varNames=c("Hyp")))

### Excluding trajectories with more than 1 NA
(ld3 <- clusterLongData(dn,maxNA=1))



cleanEx()
nameEx("fuzzyKmlSlow")
### * fuzzyKmlSlow

flush(stderr()); flush(stdout())

### Name: fuzzyKmlSlow
### Title: ~ Algorithm fuzzy kml: Fuzzy k-means for Longitidinal data ~
### Aliases: fuzzyKmlSlow
### Keywords: classif cluster nonparametric ts robust

### ** Examples

### Data generation
traj <- gald()["traj"]
partInit <- initializePartition(3,200,"kmeans--",traj)

### fuzzy Kml
partResult <- fuzzyKmlSlow(traj,partInit)



cleanEx()
nameEx("generateArtificialLongData")
### * generateArtificialLongData

flush(stderr()); flush(stdout())

### Name: generateArtificialLongData
### Title: ~ Function: generateArtificialLongData (or gald) ~
### Aliases: gald generateArtificialLongData
### Keywords: datagen cluster ts

### ** Examples

par(ask=TRUE)


#####################
### Default example

(ex1 <- generateArtificialLongData())
plot(ex1)
part1 <- partition(rep(1:4,each=50))
plot(ex1,part1)


#####################
### Three diverging lines

ex2 <- generateArtificialLongData(meanTrajectories=list(function(t)0,function(t)-t,function(t)t))
part2 <- partition(rep(1:3,each=50))
plot(ex2,part2)


#####################
### Three diverging lines with high variance, unbalance groups and missing value

ex3 <- generateArtificialLongData(
   meanTrajectories=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(100,30,10),
   residualVariation=function(t){rnorm(1,0,3)},
   percentOfMissing=c(0.25,0.5,0.25)
)
part3 <- partition(rep(1:3,c(100,30,10)))
plot(ex3,part3)


#####################
### Four strange functions

ex4 <- generateArtificialLongData(
    nbEachClusters=c(300,200,100,100),
    meanTrajectories=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    residualVariation=function(t){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0.3)
part4 <- partition(rep(1:4,c(300,200,100,100)))
plot(ex4,part4)


#####################
### To get only longData (if you want some artificial longData
###    to deal with another algorithm), use the getteur ["traj"]

ex5 <- gald(nbEachCluster=3,time=1:3)
ex5["traj"]

par(ask=FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("getBestPostProba")
### * getBestPostProba

flush(stderr()); flush(stdout())

### Name: getBestPostProba
### Title: ~ Function: getBestPostProba ~
### Aliases: getBestPostProba

### ** Examples

### Creation of an object ClusterLongData
myCld <- gald(20)

### Computation of some partition
kml(myCld,2:4,3)

### Extraction the best posterior probabilities
### form the list of partition with 3 clusters of the second clustering
getBestPostProba(myCld,3,2)



cleanEx()
nameEx("getCluster")
### * getCluster

flush(stderr()); flush(stdout())

### Name: getClusters
### Title: ~ Function: getClusters ~
### Aliases: getClusters

### ** Examples

### Creation of an object ClusterLongData
myCld <- gald(20)

### Computation of some partition
kml(myCld,2:4,3)

### Extraction form the list of partition with 3 clusters
###   of the second clustering
getClusters(myCld,3,2)



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
data(pregnandiol)
names(pregnandiol)
cldPregnan <- cld(pregnandiol,timeInData=1:30*2+1)

### 2. Building "optimal" clusteration (with only 3 redrawings)
kml(cldPregnan,nbRedrawing=3,toPlot="both")

### 3. Exporting results
try(choice(cldPregnan))



cleanEx()
nameEx("kml")
### * kml

flush(stderr()); flush(stdout())

### Name: kml
### Title: ~ Algorithm kml: K-means for Longitidinal data ~
### Aliases: kml kml-method kml,ClusterLongData-method
### Keywords: dplot chron spatial classif cluster nonparametric ts robust

### ** Examples

### Generation of some data
cld1 <- generateArtificialLongData()

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
#     We want to "see" what happen (so printCal and printTraj are TRUE)
kml(cld1,2:6,3,toPlot='both')

### 4 seems to be the best. But to be sure, we try more redrawing 4 or 6 only.
#     We don't want to see again, we want to get the result as fast as possible.
kml(cld1,c(4,6),10)



cleanEx()
nameEx("parKml")
### * parKml

flush(stderr()); flush(stdout())

### Name: parKml
### Title: ~ Function: parKml ~
### Aliases: parKml parALGO

### ** Examples

### Generation of some data
cld1 <- generateArtificialLongData(15)

### Setting two different set of option :
(option1 <- parALGO())
(option2 <- parALGO(distanceName="maximum",centerMethod=function(x)median(x,na.rm=TRUE)))

### Running kml We suspect 3, 4 or 5 clusters, we want 2 redrawing.
kml(cld1,3:5,2,toPlot="both",parAlgo=option1)
kml(cld1,3:5,2,toPlot="both",parAlgo=option2)



cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot,ClusterLongData
### Title: ~ Function: plot for ClusterLongData ~
### Aliases: plot plot-method plot,ClusterLongData
###   plot,ClusterLongData,ANY-method plot,ClusterLongData,missing-method
###   plot,ClusterLongData,numeric-method
###   plot,ClusterLongData,Partition-method
### Keywords: dplot iplot chron spatial classif cluster ts

### ** Examples

##################
### Construction of the data

ld <- gald()
part <- partition(rep(1:4,each=50))

### Basic plotting
plot(ld)
plot(ld,part)


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plot(ld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(ld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(ld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(ld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(ld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))




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
