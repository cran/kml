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
c2a <- clusterization(yLongData=ld2,xPartition=p2a)
c2b <- clusterization(yLongData=ld2,xPartition=p2b)
c2c <- clusterization(yLongData=ld2,xPartition=p2c)
c2d <- clusterization(yLongData=ld2,xPartition=p1a)
c2e <- clusterization(yLongData=ld2,xPartition=p1b)
c2f <- clusterization(yLongData=ld2,xPartition=p1c)

c2an <- clusterization(yLongData=ld2n,xPartition=p2a,imputationMethod="LOCF")#,trajSizeMin=3)
c2bn <- clusterization(yLongData=ld2n,xPartition=p2b,imputationMethod="LOCB")#,trajSizeMin=3)
c2cn <- clusterization(yLongData=ld2n,xPartition=p2c,imputationMethod="linearInterpolation")#,trajSizeMin=3)
c2dn <- clusterization(yLongData=ld2n,xPartition=p1a,imputationMethod="linearInterpolation2")#,trajSizeMin=3)
c2en <- clusterization(yLongData=ld2n,xPartition=p1b,imputationMethod="linearInterpolation3")#,trajSizeMin=3)
c2fn <- clusterization(yLongData=ld2n,xPartition=p1c,imputationMethod="copyMean")#,trajSizeMin=3)

c3a <- clusterization(yLongData=ld3,xPartition=p3a)
c3b <- clusterization(yLongData=ld3,xPartition=p3b)
c3c <- clusterization(yLongData=ld3,xPartition=p3c)
c3d <- clusterization(yLongData=ld3,xPartition=p3d)
c3e <- clusterization(yLongData=ld3,xPartition=p3e)
c3f <- clusterization(yLongData=ld3,xPartition=p3f)

c3an <- clusterization(yLongData=ld3n,xPartition=p3a,imputationMethod="copyMean")
c3bn <- clusterization(yLongData=ld3n,xPartition=p3b,imputationMethod="copyMean")
c3cn <- clusterization(yLongData=ld3n,xPartition=p3c,imputationMethod="copyMean")
c3dn <- clusterization(yLongData=ld3n,xPartition=p3d,imputationMethod="copyMean")
c3en <- clusterization(yLongData=ld3n,xPartition=p3e,imputationMethod="copyMean")
c3fn <- clusterization(yLongData=ld3n,xPartition=p3f,imputationMethod="copyMean")

c4a <- clusterization(yLongData=ld4,xPartition=p4a)
c4b <- clusterization(yLongData=ld4,xPartition=p4b)
c4c <- clusterization(yLongData=ld4,xPartition=p4c)
c4d <- clusterization(yLongData=ld4,xPartition=p4d)
c4e <- clusterization(yLongData=ld4,xPartition=p4e)

c4an <- clusterization(yLongData=ld4n,xPartition=p4an,imputationMethod="copyMean")
c4bn <- clusterization(yLongData=ld4n,xPartition=p4bn,imputationMethod="copyMean")
c4cn <- clusterization(yLongData=ld4n,xPartition=p4cn,imputationMethod="copyMean")
c4dn <- clusterization(yLongData=ld4n,xPartition=p4dn,imputationMethod="copyMean")
c4en <- clusterization(yLongData=ld4n,xPartition=p4en,imputationMethod="copyMean")

c5a <- clusterization(yLongData=ld5,xPartition=p5a)
c5b <- clusterization(yLongData=ld5,xPartition=p5b)
c5c <- clusterization(yLongData=ld5,xPartition=p5c)

c5an <- clusterization(yLongData=ld5n,xPartition=p5a,imputationMethod="copyMean")
c5bn <- clusterization(yLongData=ld5n,xPartition=p5b,imputationMethod="copyMean")
c5cn <- clusterization(yLongData=ld5n,xPartition=p5c,imputationMethod="copyMean")



cat("\n####################################################################
######################## Test  Clusterization ######################
############################# Accesseurs ###########################
####################################################################\n")

# Héritage
c2an["nbClusters"]
c2a["clusters"]
c3d["nbClusters"]
c3en["clusters"]

# Autres
c2an["calinski"]
c2a["percentEachCluster"]
c2an["convergenceTime"]
c2an["criterionName"]
c2an["criterionValue"]
c2a["startingCondition"]
c2an["algorithmUsed"]
try(c2an["calinksi"])
c3an["calinski"]
c3a["percentEachCluster"]
c3an["convergenceTime"]
c3an["criterionName"]
c3an["criterionValue"]
c3a["startingCondition"]
c3an["algorithmUsed"]



cat("####################################################################
######################## Test  Clusterization ######################
############################## Affichage ###########################
####################################################################\n")

c2a
c4dn

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++ Fin Test  Clusterization ++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

