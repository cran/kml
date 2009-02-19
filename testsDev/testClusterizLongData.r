source("../R/clusterizLongData.r")

cat("\n#######################################################################
######################## Test  ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

new("ClusterizLongData")
new("ClusterizLongData",id=c("1","2","3"),time=c(2,4,8),varName="Age",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),trajSizeMin=2)
new("ClusterizLongData",ld1) # ???


cat("\n###################################################################
##################### Test  ClusterizLongData #####################
########################### Constructeur ##########################
###################################################################\n")

clusterizLongData(id=c(1,2,3),time=c(2,4,8),varName="T",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))

cld1  <- as.clusterizLongData(data=ld1)
cld1n  <- as.clusterizLongData(data=ld1n)
cld2  <- as.clusterizLongData(dn2)
cld2n  <- as.clusterizLongData(data=ld2n)
cld3  <- as.clusterizLongData(data=ld3)
cld3n  <- as.clusterizLongData(data=ld3n)
cld4  <- as.clusterizLongData(data=ld4)
cld4n  <- as.clusterizLongData(data=ld4n)
cld5  <- as.clusterizLongData(data=ld5)
cld5n  <- as.clusterizLongData(cbind(id=1:8,as.data.frame(data5)))



cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################ Accesseurs ###########################
################################################################## #\n")

cld1["id"]
cld2n["id"]
cld3["id"]
cld4n["id"]
cld5n["id"]

cld1n["time"]
cld2["time"]
cld3n["time"]
cld4["time"]
cld5["time"]

cld1["varName"]
cld2n["varName"]
cld3["varName"]
cld4n["varName"]
cld5n["varName"]

cld1["trajSizeMin"]
cld2n["trajSizeMin"]
cld3["trajSizeMin"]
cld4n["trajSizeMin"]
cld5n["trajSizeMin"]

cld1["calinski"]
cld2n["calinski"]
cld3["calinski"]
cld4n["calinski"]
cld5n["calinski"]

cld1["traj"]
cld2n["traj"]
cld3["traj"]
cld4n["traj"]
cld5n["traj"]

cld3["id"] <- paste("I-",1:243,sep="")
cld4["time"] <- cld4["time"]*2
cld2n["varName"] <- "Age"
cld3["trajSizeMin"] <- 5
cld3["traj"] <- cld3["traj"]*12
try(cld3["calinski"] <- 2)

(cld2)
cld2["clusters"] <- c2a
(cld2)
cld2["clusters"] <- c2c
cld2["clusters"] <- c2d
cld2["clusters"] <- c2e
(cld2)
cld2["clusters"] <- c2a
cld2["clusters"] <- c2b
p1a["id"] <- cld2["id"]
cld2["clusters"] <- clusterization(cld2,p1a)
(cld2)


cld2n["clusters"] <- c2a
cld2n["clusters"] <- c2c
cld2n["clusters"] <- c2d
cld2n["clusters"] <- c2e
cld2n["clusters"] <- c2a
cld2n["clusters"] <- c2b
p1a["id"] <- cld2n["id"]
cld2n["clusters"] <- clusterization(cld2n,p1a)
(cld2)


cld3["clusters"] <- c3a
cld3["clusters"] <- c3c
cld3["clusters"] <- c3a
cld3["clusters"] <- c3b
cld3["clusters"] <- c3a
cld3["clusters"] <- c3b
cld3["clusters"] <- c3d
cld3["clusters"] <- c3d
cld3["clusters"] <- c3e
cld3["clusters"] <- c3d
cld3["clusters"] <- c3d
cld3["clusters"] <- c3e
(cld3)


cld3n["clusters"] <- c3a
cld3n["clusters"] <- c3c
cld3n["clusters"] <- c3b
cld3n["clusters"] <- c3a
cld3n["clusters"] <- c3a
cld3n["clusters"] <- c3b
cld3n["clusters"] <- c3d
cld3n["clusters"] <- c3d
cld3n["clusters"] <- c3e
(cld3n)

cld4n["clusters"] <- c4a
cld4n["clusters"] <- c4b
cld4n["clusters"] <- c4c
cld4n["clusters"] <- c4d
cld4n["clusters"] <- c4e
(cld4n)

cld5n["clusters"] <- c5a
cld5n["clusters"] <- c5b
cld5n["clusters"] <- c5c
cld5n["clusters"] <- c5a
cld5n["clusters"] <- c5b
(cld5n)

cld2["clusters"]
cld2["clusters","c3"]
cld2["clusters",c("c3",4)]


cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################ Affichage ############################
################################################################## #\n")

cld1
cld1n
cld2
cld2n
cld3
cld3n
cld4
cld4n
cld5
cld5n

par(mfrow=c(2,3))
plot(cld1["calinski"])
plot(cld2n["calinski"])
plot(cld3["calinski"])
plot(cld3n["calinski"])
plot(cld4["calinski"])
plot(cld5n["calinski"])

######## plot
### Si y n'a qu'une coordonnée, c'est le nombre de cluster. Dans ce cas, on lui donne '1' pour deuxième coordonnée
### Si y est absent
###   - S'il n'y a aucun Calinski, on affiche longData avec moyenne si moyenne=black ou color.
###   - S'il y a des Calinski, on lui donne le plus grand

dev.off()
plot(cld3,colorTraj="color",subGroups="A",colorMean="both")
plot(cld3,3,colorTraj="color",subGroups="B",colorMean="both",point="letters")
plot(cld3,5,colorTraj="color",subGroups="C",colorMean="both",point="symbols")
plot(cld3,3,colorTraj="no",subGroups="C",colorMean="both")
plot(cld3,3,colorTraj="color",subGroups=c("A","C"),colorMean="both")
plot(cld3,c(3,2),colorTraj="color",subGroups=c("A","C"),colorMean="both")
plot(cld3,5,colorTraj="no",subGroups=c("A","C"),colorMean="no")
plot(cld3,5,colorTraj="no",subGroups=c("A","C"),colorMean="both")
plot(cld3,3,colorTraj="no",subGroups=c("A","C"),colorMean="no",point="letters")
plot(cld3,5,colorMean="no",subGroups=c("A","C","B"))
plot(cld3,3,colorMean="no")

#.ClusterizLongData.plotSubGroups <- function(x,y,colorTraj="color",colorMean="both",main="",point="no",size=1,ylim=NA,...){}
dev.off()
#close.screen(all=TRUE)
#plotSubGroups(cld3,colorTraj="color",colorMean="both")
plotSubGroups(cld3,3,colorTraj="color",colorMean="both",point="letters")
plotSubGroups(cld3,5,colorTraj="color",colorMean="both",point="symbols")
plotSubGroups(cld3,3,colorTraj="no",colorMean="both")
plotSubGroups(cld3,3,colorTraj="color",colorMean="both")
plotSubGroups(cld3,c(3,2),colorTraj="color",colorMean="both")
plotSubGroups(cld3,3,colorTraj="no",colorMean="no")
plotSubGroups(cld3,3,colorTraj="no",colorMean="both")
plotSubGroups(cld3,3,colorTraj="no",colorMean="no",point="letters")
plotSubGroups(cld3,3,colorMean="no")

dev.off()
#close.screen(all=TRUE)
plotAll(cld1)
plotAll(cld2)
plotAll(cld2n)
plotAll(cld2n,printCal=TRUE)
plotAll(cld2n,printCal=FALSE)
plotAll(cld3n,printCal=TRUE)
plotAll(cld3n,printCal=FALSE)
plotAll(cld3n,3,printCal=FALSE)
plotAll(cld3n,3,printTraj=FALSE,printSub=TRUE)
plotAll(cld3n,3,printCal=FALSE)
plotAll(cld3n,3,printCal=FALSE,printTraj=TRUE,printSub=TRUE)

plotAll(cld4n,3,printTraj=FALSE,printSub=TRUE)
plotAll(cld4n,c(3,2),printTraj=FALSE,printSub=TRUE)
plotAll(cld5)
plotAll(cld5n)

cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")

#kml1(cld3@traj,nbId=243,nbTime=27,nbClusters=4,clusters=floor(runif(243,1,4+1)))
kml(cld3,2:4,5)

choice(cld2)



#exportClusterization(c5a ,"essaiExport1")
#exportClusterization(c4en,"essaiExport2")
#exportClusterization(c5a ,"essaiExport3")
#exportClusterization(c4en,"essaiExport4")

### These are tested after exportClusterization even if they are LongData method.
#importClusterization(ld5,"essaiExport1")
#importClusterization(ld4n,"essaiExport2")
#importClusterization(ld5,"essaiExport3")
#importClusterization(ld4n,"essaiExport4")


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++ Fin Test ClusterizLongData ++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


