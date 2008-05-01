### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld

source("../R/clusterization.r")

cat("####################################################################
######################## Test  Clusterization ######################
############################## Création ############################
####################################################################\n")

new("Clusterization")
new("Clusterization",xLongData=ld2,yPartition=p2a)


cat("####################################################################
######################## Test  Clusterization ######################
############################ Constructeur ##########################
####################################################################\n")

c2a <- clusterization(xLongData=ld2,yPartition=p2a)
c2b <- clusterization(xLongData=ld2,yPartition=p2b)
c2c <- clusterization(xLongData=ld2,yPartition=p2c)
c2d <- clusterization(xLongData=ld2,yPartition=p1a)
c2e <- clusterization(xLongData=ld2,yPartition=p1b)
c2f <- clusterization(xLongData=ld2,yPartition=p1c)

c2an <- clusterization(xLongData=ld2n,yPartition=p2a)#,trajSizeMin=3)
c2bn <- clusterization(xLongData=ld2n,yPartition=p2b)#,trajSizeMin=3)
c2cn <- clusterization(xLongData=ld2n,yPartition=p2c)#,trajSizeMin=3)
c2dn <- clusterization(xLongData=ld2n,yPartition=p1a)#,trajSizeMin=3)
c2en <- clusterization(xLongData=ld2n,yPartition=p1b)#,trajSizeMin=3)
c2fn <- clusterization(xLongData=ld2n,yPartition=p1c)#,trajSizeMin=3)

c3a <- clusterization(xLongData=ld3,yPartition=p3a)
c3b <- clusterization(xLongData=ld3,yPartition=p3b)
c3c <- clusterization(xLongData=ld3,yPartition=p3c)

c3an <- clusterization(xLongData=ld3n,yPartition=p3a)
c3bn <- clusterization(xLongData=ld3n,yPartition=p3b)
c3cn <- clusterization(xLongData=ld3n,yPartition=p3c)

c4a <- clusterization(xLongData=ld4,yPartition=p4a)
c4b <- clusterization(xLongData=ld4,yPartition=p4b)
c4c <- clusterization(xLongData=ld4,yPartition=p4c)
c4d <- clusterization(xLongData=ld4,yPartition=p4d)
c4e <- clusterization(xLongData=ld4,yPartition=p4e)

c4an <- clusterization(xLongData=ld4n,yPartition=p4an)
c4bn <- clusterization(xLongData=ld4n,yPartition=p4bn)
c4cn <- clusterization(xLongData=ld4n,yPartition=p4cn)
c4dn <- clusterization(xLongData=ld4n,yPartition=p4dn)
c4en <- clusterization(xLongData=ld4n,yPartition=p4en)

c5a <- clusterization(xLongData=ld5,yPartition=p5a)
c5b <- clusterization(xLongData=ld5,yPartition=p5b)
c5c <- clusterization(xLongData=ld5,yPartition=p5c)

c5an <- clusterization(xLongData=ld5n,yPartition=p5a)
c5bn <- clusterization(xLongData=ld5n,yPartition=p5b)
c5cn <- clusterization(xLongData=ld5n,yPartition=p5c)



cat("\n####################################################################
######################## Test  Clusterization ######################
############################# Accesseurs ###########################
####################################################################\n")

# Héritage
getId(c2a)
getNbClusters(c2a)
getClusters(c2a)

# Autres
#getTrajSizeMin(c2a)
getCalinskiCriterion(c2a)
getPercentEachCluster(c4a)
getTraceBetween(c4b)
getTraceWithin(c4c)
getDetWithin(c3a)
getConvergenceTime(c2b)
getVarBetween(c2b)
getVarWithin(c5c)



cat("####################################################################
######################## Test  Clusterization ######################
############################## Affichage ###########################
####################################################################\n")

c2a
c4d

print(c2a)
print(c2bn)
print(c2c)
print(c3an)
print(c3b)
print(c4cn)
print(c5a)
print(c5bn)



cat("\n####################################################################
######################## Test  Clusterization ######################
############################### Autres #############################
####################################################################\n")

cat("### Fin test clusteriation ***\n")

