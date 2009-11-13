source("../R/clusterizLongData.r")

cat("\n#######################################################################
######################## Test  ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

new("ClusterizLongData")
new("ClusterizLongData",id=c("1","2","3"),time=c(2,4,8),varName="Age",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
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

cld3["id"] <- paste("I-",1:243,sep="")
cld4["time"] <- cld4["time"]*2
cld2n["varName"] <- "Age"
cld3["trajMinSize"] <- 5
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
cld2["clusters"] <- clusterization(p1a,cld2)
(cld2)


c2aA <- c2a
c2aA@criterionName <- "A"
c2aA@criterionValue <- 2

c2dA <- c2d
c2dA@criterionName <- "A"
cld2["clusters"] <- c2aA
cld2["clusters"] <- c2dA
cld2["clusters"] <- c2dA

cld2n["clusters"] <- c2a
cld2n["clusters"] <- c2c
cld2n["clusters"] <- c2d
cld2n["clusters"] <- c2e
cld2n["clusters"] <- c2a
cld2n["clusters"] <- c2b
cld2n["clusters"] <- clusterization(p1a,cld2n)
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

cld1["trajMinSize"]
cld2n["trajMinSize"]
cld3["trajMinSize"]
cld4n["trajMinSize"]
cld5n["trajMinSize"]

try(cld1["calinski"])
cld2n["calinski"]
cld3["calinski"]
cld4n["calinski"]
cld5n["calinski"]
cld2["A"]

cld1["traj"]
cld2n["traj"]
cld3["traj"]
cld4n["traj"]
cld5n["traj"]

cld2["clusters"]
cld2["clusters",1]
cld2["clusters",2]
cld2["clusters",3]
cld2["clusters",c(3,2)]
cld2["clusters",4]
try(cld2["clusters",c(4,2)])
cld2["c3"]
cld2["c3",3]
try(cld2["clusters",53])

cld2["criterionValue"]
cld2["criterionValue",1]
cld2["criterionValue",2]
cld2["criterionValue",3]
cld2["criterionValue",c(3,2)]
cld2["criterionValue",4]
cld2["criterionValue",c(4,2)]
try(cld2["criterionValue",53])

cld2["criterionName"]
cld2["criterionName",1]
cld2["criterionName",2]
cld2["criterionName",3]
cld2["criterionName",c(3,2)]
cld2["criterionName",4]
cld2["criterionName",c(4,2)]
try(cld2["criterionName",53])

cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################ Affichage ############################
###################################################################\n")

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

par(mfrow=c(3,3))
plotCriterion(cld1)
plotCriterion(cld2)
plotCriterion(cld2,all=TRUE)
plotCriterion(cld2,all=TRUE,nbCriterion=2)
plotCriterion(cld3)
plotCriterion(cld3,all=TRUE)
plotCriterion(cld3,all=TRUE,nbCriterion=2)
plotCriterion(cld4)


######## plot
### Si y n'a qu'une coordonnée, c'est le nombre de cluster. Dans ce cas, on lui donne '1' pour deuxième coordonnée
### Si y est absent
###   - S'il n'y a aucun Calinski, on affiche longData avec moyenne si moyenne=black ou color.
###   - S'il y a des Calinski, on lui donne le plus grand

dev.off()
plot(cld3)
plot(cld3,col=1,subGroups="A",col.mean=2)
plot(cld3,col=1,subGroups="B",col.mean=2)
plot(cld3,3,col="clusters",subGroups="B",col.mean="clusters",pch="letters")
plot(cld3,5,col.mean="clusters",pch.mean="symbols")
plot(cld3,3,type="n",subGroups="C")
plot(cld3,3,type="n",subGroups=c("A","C"))
plot(cld3,c(3,2),subGroups=c("A","C"))
plot(cld3,5,type="n",subGroups=c("A","C"),type.mean="n")
plot(cld3,5,type="b",type.mean="n")
plot(cld3,5,type="b",type.mean="n",pch="letters")
plot(cld3,5,type="n",subGroups=c("A","C"),col.mean="clusters")
plot(cld3,3,type="n",subGroups=c("A","C"),col.mean="n",type.mean="p")
plot(cld3,3,type="n",subGroups=c("A","C"),col.mean="n",type.mean="p",pch.time=c(0,7,13,16,19,25))
plot(cld3,5,type="n",subGroups=c("A","C","B"))
plot(cld3,25,type="n")

#.ClusterizLongData.plotSubGroups <- function(x,y,colorTraj="color",colorMean="both",main="",point="no",size=1,ylim=NA,...){}
dev.off()
plotSubGroups(cld2)
plotSubGroups(cld2,col=1,subGroups="A",col.mean=2)
plotSubGroups(cld2,col=1,subGroups="B",col.mean=2)
plotSubGroups(cld3,3,col="clusters",subGroups="B",col.mean="clusters",pch="letters")
plotSubGroups(cld3,5,col.mean="clusters",pch.mean="symbols")
plotSubGroups(cld3,3,type="n",subGroups="C")
plotSubGroups(cld3,3,type="n",subGroups=c("A","C"))
plotSubGroups(cld3,c(3,2),subGroups=c("A","C"))
plotSubGroups(cld3,5,type="n",subGroups=c("A","C"),type.mean="n")
plotSubGroups(cld3,5,type="b",type.mean="n")
plotSubGroups(cld3,5,type="b",type.mean="n",pch="letters")
plotSubGroups(cld3,5,type="n",subGroups=c("A","C"),col.mean="clusters")
plotSubGroups(cld3,3,type="n",subGroups=c("A","C"),col.mean="n",type.mean="p")
plotSubGroups(cld3,3,type="n",subGroups=c("A","C"),col.mean="n",type.mean="p",pch.time=c(0,7,13,16,19,25))
plotSubGroups(cld3,5,type="n",subGroups=c("A","C","B"))
try(plotSubGroups(cld3,25,type="n"))


dev.off()
#close.screen(all=TRUE)
plotAll(cld2)
plotAll(cld2n)
plotAll(cld2n,print.cal=TRUE)
plotAll(cld2n,print.cal=FALSE)
plotAll(cld3n,print.cal=TRUE)
plotAll(cld3n,print.cal=FALSE)
plotAll(cld3n,c(3,2),print.cal=FALSE)
plotAll(cld3n,3,print.traj=FALSE,print.sub=TRUE)
plotAll(cld3n,3,print.cal=FALSE)
plotAll(cld3n,3,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE)

plotAll(cld4n,3,print.traj=FALSE,print.sub=TRUE)
plotAll(cld4n,c(3,2),print.traj=FALSE,print.sub=TRUE)
plotAll(cld5)
plotAll(cld5n)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++ Fin Test ClusterizLongData ++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


