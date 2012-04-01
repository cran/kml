#library(rgl)
#library(misc3d)
## library(longitudinalData)

## setwd("C:/Users/GENOLLINI/Documents/cgenolini/packages/kml/testsDev")
## source("testFunction.R")
## source("fromLongitudinalData.r")
## source("testClusterLongData.R")
## source("testParKml.r")
## source("testParChoice.R")

cat("\n####################################################################
################################ kml ###############################
############################### Tests ##############################
####################################################################\n")


source("../R/kml.r")
dyn.load("../src/kml") # dyn.load("../src-i386/kml")


################################
### fuzzy K-means par étapes ###
################################

############################
### Fonctions simples
cleanProg(calculTrajFuzzyMean,,,1) # tapply
fuzMatrice <- matrix(runif(15,0,1),5,3)
cent2a <- calculTrajFuzzyMean(ld2["traj"],fuzMatrice)
cent2b <- calculTrajFuzzyMean(ld2["traj"],fuzMatrice)

cent2c <- calculTrajFuzzyMean(ld2["traj"],fuzMatrice)

fuzMatrice <- matrix(runif(24,0,1),8,3)
cent3a <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)
cent3b <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)
cent3c <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)
cent3d <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)
cent3e <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)
cent3f <- calculTrajFuzzyMean(ld3["traj"],fuzMatrice)

cleanProg(affectFuzzyIndiv,,,1)
aC <- affectFuzzyIndiv(ld2["traj"],cent2a)
bC <- affectFuzzyIndiv(ld2["traj"],cent2b)
cC <- affectFuzzyIndiv(ld2["traj"],cent2c)


##############################
### K-means par étapes (times = 2 pour les graphes)

### Initialisation
tra <- ld3["traj"][,3:4]
plot(tra)

### Initialisation, exemple 2
tra <- ld4["traj"][,3:4]
plot(tra)

partInit <- initializePartition(3,nrow(tra),method="kmeans-",tra)
lines(tra,col=partInit+1,cex=2,type="p",lwd=3)
cent <- tra[!is.na(partInit),]

### Boucle 1
affInd <- affectFuzzyIndiv(tra,cent,fuzzyfier=1.5)
appartient <- apply(affInd,1,which.max)
plot(tra,col=appartient+1,pch=16)

cent <- calculTrajFuzzyMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)

### Boucle 2
affInd <- affectFuzzyIndiv(tra,cent,fuzzyfier=1.5)
appartient <- apply(affInd,1,which.max)
plot(tra,col=appartient+1,pch=16)

cent <- calculTrajFuzzyMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)

### Boucle 3
affInd <- affectFuzzyIndiv(tra,cent,fuzzyfier=1.5)
appartient <- apply(affInd,1,which.max)
plot(tra,col=appartient+1,pch=16)

cent <- calculTrajFuzzyMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)
### Fin boucle


### Fuzzy k-means slow

cleanProg(fuzzyKmlSlow,,,1)    ### which.max
partInit <- initializePartition(3,8,method="randomK")
system.time(kmlSlow(ld3['traj'],partInit))
partInit <- initializePartition(6,200,method="kmeans--",ld4["traj"])
system.time(kmlSlow(ld4['traj'],partInit))




##########################
### K-means par étapes ###
##########################

############################
### Fonctions simples
cleanProg(calculTrajMean,,,1) # tapply
cent2a <- calculTrajMean(ld2["traj"],p2a['clusters'])
cent2b <- calculTrajMean(ld2["traj"],p2b['clusters'])
cent2c <- calculTrajMean(ld2["traj"],p2c['clusters'],medianNA)

(cent3a <- calculTrajMean(ld3["traj"],p3a['clusters']))
(cent3b <- calculTrajMean(ld3["traj"],p3b['clusters']))
(cent3c <- calculTrajMean(ld3["traj"],p3c['clusters']))
cent3d <- calculTrajMean(ld3["traj"],p3d['clusters'])
cent3e <- calculTrajMean(ld3["traj"],p3e['clusters'])
cent3f <- calculTrajMean(ld3["traj"],p3f['clusters'])


(cent2a <- calculTrajMeanC(ld2["traj"],p2a['clustersAsInteger']))
(cent2b <- calculTrajMeanC(ld2["traj"],p2b['clustersAsInteger']))
(cent2c <- calculTrajMeanC(ld2["traj"],p2c['clustersAsInteger']))

cent3a <- calculTrajMeanC(ld3["traj"],p3a['clustersAsInteger'])
cent3b <- calculTrajMeanC(ld3["traj"],p3b['clustersAsInteger'])
cent3c <- calculTrajMeanC(ld3["traj"],p3c['clustersAsInteger'])
cent3d <- calculTrajMeanC(ld3["traj"],p3d['clustersAsInteger'])
cent3e <- calculTrajMeanC(ld3["traj"],p3e['clustersAsInteger'])
cent3f <- calculTrajMeanC(ld3["traj"],p3f['clustersAsInteger'])

cleanProg(affectIndiv,,,1)
(aC <- affectIndiv(ld2["traj"],cent2a))
(bC <- affectIndiv(ld2["traj"],cent2b))
(cC <- affectIndiv(ld2["traj"],cent2c))

(aC <- affectIndivC(ld2["traj"],cent2a))
(bC <- affectIndivC(ld2["traj"],cent2b))
(cC <- affectIndivC(ld2["traj"],cent2c))
(dC <- affectIndivC(ld3["traj"],cent3a))
(eC <- affectIndivC(ld3["traj"],cent3b))
(fC <- affectIndivC(ld3["traj"],cent3c))


##############################
### K-means par étapes (times = 2 pour les graphes)

### Initialisation
tra <- ld3["traj"][,3:4]
plot(tra)

### Initialisation, exemple 2
tra <- ld4["traj"][,3:4]
plot(tra)

partInit <- initializePartition(3,nrow(tra),method="kmeans-",tra)
lines(tra,col=partInit+1,cex=2,type="p",lwd=3)
cent <- calculTrajMean(tra,partInit)

### Boucle 1
affInd <- affectIndiv(tra,cent)
plot(tra,col=affInd+1,pch=16)

cent <- calculTrajMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)

### Boucle 2
affInd <- affectIndiv(tra,cent)
plot(tra,col=affInd+1,pch=16)

cent <- calculTrajMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)

### Boucle 3
affInd <- affectIndiv(tra,cent)
plot(tra,col=affInd+1,pch=16)

cent <- calculTrajMean(tra,affInd)
lines(cent,col=2:4,cex=2,type="p",lwd=3)
### Fin boucle






cleanProg(kmlSlow)
partInit <- initializePartition(3,8,method="randomK")
system.time(kmlSlow(ld3['traj'],partInit))
partInit <- initializePartition(6,200,method="kmeans--",ld4["traj"])
system.time(kmlSlow(ld4['traj'],partInit))
#partInit <- initializePartition(6,2000,method="randomK")
#system.time(kmlSlow(ld5['traj'],partInit))


cleanProg(kmlFast)
partInit <- initializePartition(3,8,method="randomK")
system.time(a <- kmlFast(ld3['traj'],partInit))
system.time(b <- kmlSlow(ld3['traj'],partInit))

partInit <- initializePartition(3,200,method="randomK")
system.time(a <- kmlFast(ld4['traj'],partInit))
system.time(b <- kmlSlow(ld4['traj'],partInit))
identical(a,b)

partInit <- initializePartition(6,2000,method="randomK")
system.time(a <- kmlFast(ld5['traj'],partInit))
system.time(a <- kmlFast(ld5n['traj'],partInit))


#partInit <- initializePartition(6,200,method="randomK")
#system.time(kmlFast(LD4['traj'],partInit))
#partInit <- initializePartition(6,2000,method="randomK")
#system.time(kmlFast(LD5['traj'],partInit))


cleanProg(expandStartingCond)
expandStartingCond(startingCond="all",10,"")
expandStartingCond(startingCond="all",10,"maxDist")
expandStartingCond(startingCond="all",10,"kmeans-")
expandStartingCond(startingCond="all",10,c("maxDist","kmeans-"))

expandStartingCond(startingCond="nearlyAll",10,"")
expandStartingCond(startingCond="nearlyAll",10,"maxDist")
expandStartingCond(startingCond="nearlyAll",10,"kmeans-")
expandStartingCond(startingCond="nearlyAll",10,c("maxDist","kmeans-"))

expandStartingCond(startingCond="kmeans-",10,"")
expandStartingCond(startingCond=c("kmeans-","randomK"),10,"randomK")

cleanProg(cutScreen)
cutScreen("both")
cutScreen("boteeh")

cleanProg(fastOrSlow,,,1) #DISTANCE_METHODS
fastOrSlow("both","euclidean")
fastOrSlow("none","euclidean")
fastOrSlow("none","manhattan")


cleanProg(kml)
kml(cld3)
dev.off()
kml(cld4,,1,toPlot="both")


cleanProg(exportPartition)
exportPartition(cld4,3,1,"testPart")

#choice(cld3)
cleanProg(choiceChangeParam,,,2) # choiceChangeParam
cleanProg(choice,,,0) # choiceChangeParam



cat("\n--------------------------------------------------------------------
------------------------------ Fin kml -----------------------------
------------------------------- Tests ------------------------------
--------------------------------------------------------------------\n")
