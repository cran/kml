source("../R/clusterLongData.r")

cat("\n####################################################################
######################## Test ClusterLongData ######################
####################################################################\n")

cleanProg(.ClusterLongData.validity,,,0)

### Constructeurs
cleanProg(clusterLongData)
cleanProg(.clusterLongData.constructor,,,1) # all

new("ClusterLongData")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varName="T")

clusterLongData(
    traj=tr1[,,1],
    idAll=c(100,102,104),
    time=c(1,2,4,8,16),
    varNames="PA",
    maxNA=3
    )

new("ClusterLongData",
    traj=tr2[,,1],
    idFewNA=c("i1","i2","i3","i4"),
    idAll=c("i1","i2","i3","i4"),
    time=c(1,2,4),
    varNames="P",
    maxNA=2
    )

new("ClusterLongData",
    traj=tr3n[,,2],
    idAll=c("i1","i2","i3"),
    idFewNA=c("i1","i2","i3"),
    time=c(1,2,4),
    varNames="E",
    maxNA=2
    )


new("ClusterLongData",
    traj=tr4n[,,1],
    idAll=as.character(c(101,102,103,105,106,107)),
    idFewNA=as.character(c(101,102,103,105,106,107)),
    time=c(1,2,4),
    varNames="P",
    maxNA=1
    )


cat("\n###################################################################
####################### Test ClusterLongData ######################
########################### Constructeur ##########################
###################################################################\n")

cleanProg(.clusterLongData.constructor,,,1) #all

clusterLongData()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))

clusterLongData(traj=tr1[,,1],idAll=as.character(c(101,102,104)),time=c(1,2,4,8,16),varNames="P",maxNA=3)
cld(traj=tr2[,,1],idAll=as.character(c(1,2,3,4)),time=c(1,2,4),varNames="P",maxNA=2)
cld(traj=tr3n[,,2],idAll=as.character(c(1,2,3)),time=c(1,2,4),varNames="P",maxNA=2)
cld(traj=tr3n[,,2],idAll=as.character(c(1,2,3)),time=c(1,2,4),varNames="P",maxNA=1)
cld(traj=tr4n[,,2],idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames="A",maxNA=2)

### Vérification de l'exclusion des manquantes
clusterLongData(traj=tr4n[,,2],idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames="P",maxNA=1)
clusterLongData(traj=tr4n[,,2],idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames="P",maxNA=2)



### Base de données
cleanProg(.ClusterLongData.show,,,0)

cld0 <- clusterLongData()
cld1 <- clusterLongData(traj=matrix(c(NA,2,3,NA,4,6,NA,8,10),3),idAll=1:3,time=1:3)
cld1 <- clusterLongData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),idAll=c(11,12,13),time=c(2,4,8),varNames="T")
cld1n <- clusterLongData(idAll=c(1,2,3),time=c(2,4,8),varNames="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(idAll=c(10,17,28,29,31),t1=c(5,4,2,1,0),t2=c(5,4,3,2,1),t4=c(4,2,3,1,1),t5=c(5,6,2,1,0))
cld2 <- clusterLongData(dn2[,-1],idAll=dn2[,1],time=c(1,2,5,6))
cld2 <- clusterLongData(dn2,time=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA;dn2[4,3]<-NA;dn2[4,4]<-NA
cld2n <- clusterLongData(dn2,time=c(1,2,5,6),maxNA=1)



cld3n <- clusterLongData(data3,timeInData=c(1,3,4))
cld3n <- clusterLongData(data3)
cld3 <- clusterLongData(data3Imp)


cld4 <- clusterLongData(dn4[,-1],idAll=(1:200)*2,time=c(0,1,2,3,4,6,8,10,12,16,20))
cld4n <- clusterLongData(dn4n[,-1],time=c(0,1,2,3,4,6,8,10,12,16,20),idAll=(1:200)*2)
cld5 <- clusterLongData(dn5[,-1],idAll=1:2000)
cld5n <- clusterLongData(dn5n[,-1],idAll=1:2000)




cat("
############# Set ##############
### Héritage direct de partition
")

cleanProg(.ClusterLongData.get,,,2) # CLUSTER_NAMES CRITERION_NAMES
cld3['add'] <- p3a
cld3['add'] <- p3b
cld3['add'] <- p3c
cld3['add'] <- p3d
cld3['add'] <- p3e
cld3['add'] <- p3f
cld3['add'] <- p3g
cld3['add'] <- p3h
cld3['add'] <- p3i
cld3['add'] <- p3j
cld3['c3']

############# Get #############
cld3["varNames"]
cld3["idAll"]
cld3n["idFewNA"]
cld4n["idFewNA"]
cld3["c2"]
tryBug(cld3[2])

getClusters(cld3,3,1,FALSE)
getClusters(cld3,4,2)
getClusters(cld3,3,1,TRUE)


cleanProg(.plot.clusterLongData.num)
cleanProg(.plot.clusterLongData.missingY)
cleanProg(.plotAll)

plot(cld3)
plot(cld3,2)
plot(cld3,c(3,1),toPlot="traj")
plot(cld3,c(2,1),toPlot="criterion")
plot(cld3,c(3,1))
plot(cld3,c(3,2))
tryBug(plot(cld3,c("c4",1))) ### Avant c'était possible



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++ Fin Test ClusterLongData +++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
