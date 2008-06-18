source("../R/clusterizLongData.r")

cat("\n#######################################################################
######################## Test  ClusterizLongData #######################
############################### Creation ###############################
####################################################################\n")

new("ClusterizLongData")
new("ClusterizLongData",longData(id=c(1,2,3),time=c(2,4,8),varName="Age",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3))))
new("ClusterizLongData",ld1)


cat("\n###################################################################
##################### Test  ClusterizLongData #####################
########################### Constructeur ##########################
###################################################################\n")

clusterizLongData(values=longData(id=c(1,2,3),time=c(2,4,8),varName="T",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3))))
clusterizLongData(values=ld1)

cld1  <- clusterizLongData(values=ld1)
cld1n  <- clusterizLongData(values=ld1n)
cld2  <- as.clusterizLongData(dn2)
cld2n  <- clusterizLongData(values=ld2n)
cld3  <- clusterizLongData(values=ld3)
cld3n  <- clusterizLongData(values=ld3n)
cld4  <- clusterizLongData(values=ld4)
cld4n  <- clusterizLongData(values=ld4n)
cld5  <- clusterizLongData(values=ld5)
cld5n  <- as.clusterizLongData(data5)



cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################ Accesseurs ###########################
################################################################## #\n")


addClusterization(cld2,c2a)
(cld2)
addClusterization(cld2,c2b)
addClusterization(cld2,c2c)
addClusterization(cld2,c2d)
addClusterization(cld2,c2e)
(cld2)


addPartition(cld2,p1a)
addPartition(cld2,p1b)
addPartition(cld2,p1c)
addPartition(cld2,p2a)
addPartition(cld2,p2b)
addPartition(cld2,p2c)
(cld2)

addPartition(cld2n,p1a)
addPartition(cld2n,p1b)
addPartition(cld2n,p1c)
addPartition(cld2n,p2a)
addPartition(cld2n,p2b)
addPartition(cld2n,p2c)
(cld2n)


addPartition(cld3,p3a)
addPartition(cld3,p3b)
addPartition(cld3,p3c)
(cld3)

addPartition(cld3n,p3a)
addPartition(cld3n,p3b)
addPartition(cld3n,p3c)
(cld3n)

addPartition(cld4,p4a)
addPartition(cld4,p4b)
addPartition(cld4,p4c)
addPartition(cld4,p4d)
addPartition(cld4,p4e)
(cld4)

addPartition(cld4n,p4an)
addPartition(cld4n,p4b)
addPartition(cld4n,p4cn)
addPartition(cld4n,p4d)
addPartition(cld4n,p4en)
(cld4n)

addPartition(cld5,p5a)
addPartition(cld5,p5b)
addPartition(cld5,p5c)
(cld5)

addPartition(cld5n,p5a)
addPartition(cld5n,p5b)
addPartition(cld5n,p5c)
(cld5n)

getClusterizList(cld1)
getClusterizList(cld2n)
getClusterizList(cld3)
getClusterizList(cld4n)
getClusterizList(cld5)

getClusterizList(cld2n,3)
getClusterizList(cld3,4)
getClusterizList(cld4n,11)
getClusterizList(cld5,3)

getClusterizList(cld2n,3,3)
getClusterizList(cld2n,3,1)
try(getClusterizList(cld2n,3,15))
try(getClusterizList(cld3,4,1))


getCalinskiCriterion(cld1n)
getCalinskiCriterion(cld2)
getCalinskiCriterion(cld3n)
getCalinskiCriterion(cld4)
getCalinskiCriterion(cld5n)



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

print(cld1)
print(cld1n)
print(cld2)
print(cld2n)
print(cld3)
print(cld3n)
print(cld4)
print(cld4n)
print(cld5)
print(cld5n)

par(mfrow=c(2,3))
plot(getCalinskiCriterion(cld1))
plot(getCalinskiCriterion(cld2n))
plot(getCalinskiCriterion(cld3))
plot(getCalinskiCriterion(cld3n))
plot(getCalinskiCriterion(cld4))
plot(getCalinskiCriterion(cld5n))

######## plot
### Si y n'a qu'une coordonnée, c'est le nombre de cluster. Dans ce cas, on lui donne '1' pour deuxième coordonnée
### Si y est absent
###   - S'il n'y a aucun Calinski, on affiche longData avec moyenne si moyenne=black ou color.
###   - S'il y a des Calinski, on lui donne le plus grand

plot(cld1)
plot(cld2)
plot(cld2n)
plot(cld2n,calinski=TRUE)
plot(cld3,calinski=TRUE)
plot(cld3n,3)
try(plot(cld3n,c(3,3)))
plot(cld4)
plot(cld4n)
plot(cld4,c(3,2),calinski=TRUE)
plot(cld5)
plot(cld5n)



cat("\n###################################################################
##################### Test  ClusterizLongData #####################
############################# Autres ##############################
###################################################################\n")

choice(cld2)

kml1(cld3@traj,nbId=243,nbTime=27,nbClusters=4,clusters=floor(runif(243,1,4+1)))
kml(cld2,2:4,5,print="all")



exportClusterization(c5a ,"essaiExport1")
exportClusterization(c4en,"essaiExport2")
exportClusterization(c5a ,"essaiExport3")
exportClusterization(c4en,"essaiExport4")

### These are tested after exportClusterization even if they are LongData method.
importClusterization(ld5,"essaiExport1")
importClusterization(ld4n,"essaiExport2")
importClusterization(ld5,"essaiExport3")
importClusterization(ld4n,"essaiExport4")


