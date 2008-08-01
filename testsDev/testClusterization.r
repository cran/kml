### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld

source("../R/clusterization.r")

cat("####################################################################
######################## Test  Clusterization ######################
############################## Création ############################
####################################################################\n")

new("Clusterization")


cat("####################################################################
######################## Test  Clusterization ######################
############################ Constructeur ##########################
####################################################################\n")
ld2["id"]<-1:3
c2a <- clusterization(xLongData=ld2,yPartition=p2a)
c2b <- clusterization(xLongData=ld2,yPartition=p2b)
c2c <- clusterization(xLongData=ld2,yPartition=p2c)
p1a["id"] <- 1:3
c2d <- clusterization(xLongData=ld2,yPartition=p1a)
p1b["id"] <- 1:3
c2e <- clusterization(xLongData=ld2,yPartition=p1b)
c2f <- clusterization(xLongData=ld2,yPartition=p1c)

ld2n["id"]<-1:3
c2an <- clusterization(xLongData=ld2n,yPartition=p2a)#,trajSizeMin=3)
c2bn <- clusterization(xLongData=ld2n,yPartition=p2b)#,trajSizeMin=3)
c2cn <- clusterization(xLongData=ld2n,yPartition=p2c)#,trajSizeMin=3)
c2dn <- clusterization(xLongData=ld2n,yPartition=p1a)#,trajSizeMin=3)
c2en <- clusterization(xLongData=ld2n,yPartition=p1b)#,trajSizeMin=3)
c2fn <- clusterization(xLongData=ld2n,yPartition=p1c)#,trajSizeMin=3)

ld3["id"] <- p3a["id"]
c3a <- clusterization(xLongData=ld3,yPartition=p3a)
c3b <- clusterization(xLongData=ld3,yPartition=p3b)
c3c <- clusterization(xLongData=ld3,yPartition=p3c)
c3d <- clusterization(xLongData=ld3,yPartition=p3d)
c3e <- clusterization(xLongData=ld3,yPartition=p3e)
c3f <- clusterization(xLongData=ld3,yPartition=p3f)

ld3n["id"] <- p3a["id"]
c3an <- clusterization(xLongData=ld3n,yPartition=p3a)
c3bn <- clusterization(xLongData=ld3n,yPartition=p3b)
c3cn <- clusterization(xLongData=ld3n,yPartition=p3c)
c3dn <- clusterization(xLongData=ld3n,yPartition=p3d)
c3en <- clusterization(xLongData=ld3n,yPartition=p3e)
c3fn <- clusterization(xLongData=ld3n,yPartition=p3f)

ld4["id"] <- p4a["id"]
c4a <- clusterization(xLongData=ld4,yPartition=p4a)
c4b <- clusterization(xLongData=ld4,yPartition=p4b)
c4c <- clusterization(xLongData=ld4,yPartition=p4c)
c4d <- clusterization(xLongData=ld4,yPartition=p4d)
c4e <- clusterization(xLongData=ld4,yPartition=p4e)

ld4n["id"] <- p4a["id"]
c4an <- clusterization(xLongData=ld4n,yPartition=p4an)
c4bn <- clusterization(xLongData=ld4n,yPartition=p4bn)
c4cn <- clusterization(xLongData=ld4n,yPartition=p4cn)
c4dn <- clusterization(xLongData=ld4n,yPartition=p4dn)
c4en <- clusterization(xLongData=ld4n,yPartition=p4en)

ld5["id"] <- p5a["id"]
c5a <- clusterization(xLongData=ld5,yPartition=p5a)
c5b <- clusterization(xLongData=ld5,yPartition=p5b)
c5c <- clusterization(xLongData=ld5,yPartition=p5c)

ld5n["id"] <- p5a["id"]
c5an <- clusterization(xLongData=ld5n,yPartition=p5a)
c5bn <- clusterization(xLongData=ld5n,yPartition=p5b)
c5cn <- clusterization(xLongData=ld5n,yPartition=p5c)



cat("\n####################################################################
######################## Test  Clusterization ######################
############################# Accesseurs ###########################
####################################################################\n")

# Héritage
c2a["id"]
c2an["nbClusters"]
c2a["clusters"]
c3an["id"]
c3d["nbClusters"]
c3en["clusters"]

# Autres
#getTrajSizeMin(c2a)
c2an["calinski"]
c2a["percentEachCluster"]
c2an["convergenceTime"]
c3a["calinski"]
c3dn["percentEachCluster"]
c3en["convergenceTime"]


cat("####################################################################
######################## Test  Clusterization ######################
############################## Affichage ###########################
####################################################################\n")

c2a
c4dn

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++ Fin Test  Clusterization ++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

