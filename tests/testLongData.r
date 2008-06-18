source("../R/longData.r")
### LongData contient des trajectoires (que l'on doit analyser) et leur identifiant
### Une trajectoire est une suite de numeric.
### 'Des' trajectoires est une matrice de numeric, chaque ligne est une trajectoire.
###  - Id est l'identifiant (indispensable)
###  - time est le temps ou les mesures ont été faite.
###  - varName est le nom de la variable.
###  - traj est la matrice des trajectoires
# id       : identifiant of the individual (or lines).
# time     : real time
# varName : nom of the variable (single now, several in the futur)
# traj    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)


cat("\n####################################################################
############################ Test  LongData ############################
############################### Creation ###############################
####################################################################\n")

### Constructeurs
new("LongData")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varName="T")
new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T",trajSizeMin=3)
new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T",trajSizeMin=1)
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T",trajSizeMin=0))
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102,104)),time=c(2,4,8),varName="T",trajSizeMin=5))
try(new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2",NA,"T8"))),id=as.character(c(101,102,104)),time=c(2,NA,8),varName="T"))
try(new("LongData",traj=array(c(1,NA,3,NA,NA,6,1,8,10),dim=c(3,3),dimnames=list(c(101,102,104),c("T2","T4","T8"))),id=as.character(c(101,102)),time=c(2,4,8),varName="T"))


cat("\n###################################################################
########################## Test  LongData #########################
########################### Constructeur ##########################
###################################################################\n")


longData()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=c(1,2,3),time=c(2,4,8),varName="T")

### Base de données
ld0 <- longData()
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=1:3,time=1:3)
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=c(11,12,13),time=c(2,4,8),varName="T")
ld1n <- longData(id=c(1,2,3),time=c(2,4,8),varName="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(id=c(10,17,28),T1=c(5,4,2),T2=c(5,4,3),T4=c(4,2,3),T5=c(5,6,2))
ld2 <- as.longData(dn2,timeReal=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA
ld2n <- as.longData(dn2,timeReal=c(1,2,5,6),trajSizeMin=3)

dn3 <- read.csv2("../../../../Anorexie Tamara/trajectoires de soins.csv")[,-29]
ld3 <- as.longData(dn3)

for(i in 1:2400){dn3[floor(runif(1,1,244)),floor(runif(1,2,29))]<-NA}
ld3n <- as.longData(dn3)

dn4 <- read.csv2("../../divergingLines.csv")
ld4 <-as.longData(dn4,id=(1:180)*2,timeReal=c(0,1,2,3,4,6,8,10,12,16,20))

for(i in 1:600){dn4[floor(runif(1,1,181)),floor(runif(1,2,13))]<-NA}
dn4[5,]<-NA
dn4[,6]<-NA
ld4n <-as.longData(dn4,id=(1:180)*2,timeReal=c(0,1,2,3,4,6,8,10,12,16,20))

data5 <- rbind(c(1,2 ,NA,4 ),
               c(1,1 ,NA,1 ),
               c(2,3 ,4 ,5 ),
               c(2,2 ,2 ,2 ),
               c(3,NA,NA,6 ),
               c(3,NA,NA,3 ),
               c(2,4 ,4 ,NA),
               c(2,3 ,2 ,NA))
dim(data5) <- c(8,4)
ld5n <- as.longData(data5)
data5Imp <- rbind(c(1,2,3,4),
                  c(1,1,1,1),
                  c(2,3,4,5),
                  c(2,2,2,2),
                  c(3,4,5,6),
                  c(3,3,3,3),
                  c(2,4,4,5),
                  c(2,3,2,2))
dim(data5Imp) <- c(8,4)
ld5 <- as.longData(data5Imp)



cat("\n####################################################################
########################### Test  LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

getId(ld0)
getId(ld1)
getId(ld1n)
getId(ld2)
getId(ld2n)
getId(ld3)
getId(ld3n)
getId(ld4)
getId(ld4n)
getId(ld5)
getId(ld5n)

getTime(ld0)
getTime(ld1)
getTime(ld1n)
getTime(ld2)
getTime(ld2n)
getTime(ld3)
getTime(ld3n)
getTime(ld4)
getTime(ld4n)
getTime(ld5)
getTime(ld5n)

getVarName(ld0)
getVarName(ld1)
getVarName(ld1n)
getVarName(ld2)
getVarName(ld2n)
getVarName(ld3)
getVarName(ld3n)
getVarName(ld4)
getVarName(ld4n)
getVarName(ld5)
getVarName(ld5n)

getTraj(ld0)
getTraj(ld1)
getTraj(ld1n)
getTraj(ld2)
getTraj(ld2n)
getTraj(ld3)
getTraj(ld3n)
getTraj(ld4)
getTraj(ld4n)
getTraj(ld5)
getTraj(ld5n)

getTrajSizeMin(ld0)
getTrajSizeMin(ld1)
getTrajSizeMin(ld1n)
getTrajSizeMin(ld2)
getTrajSizeMin(ld2n)
getTrajSizeMin(ld3)
getTrajSizeMin(ld3n)
getTrajSizeMin(ld4)
getTrajSizeMin(ld4n)
getTrajSizeMin(ld5)
getTrajSizeMin(ld5n)


try(setId(ld1)<-1:4)
try(setId(ld1)<-1:2)
setId(ld1)<-c(2,4,7)

try(setTime(ld1)<-1:4)
setTime(ld1)<-c(2,4,9)

setVarName(ld1)<-c("T")
setVarName(ld1)<-c("E")

try(setTraj(ld1)<-array(1:12,dim=c(3,4,1)))
setTraj(ld1)<-array(1:9,dim=c(3,3))


cat("\n###################################################################
########################## Test  LongData #########################
############################# Affichage ###########################
###################################################################\n")

ld1
ld4

print(ld1)
print(ld2n)
print(ld3)
print(ld4n)
print(ld5)

#length(ld1n)
#length(ld2)
#length(ld3n)
#length(ld4)
#length(ld5n)

#summary(ld1)
#summary(ld2n)
#summary(ld3)
#summary(ld4n)
#summary(ld5)



par(mfrow=c(2,2))

.LongData.plotCurve(ld4,p4a,main="",color="no")
.LongData.plotMean(ld4,p4b,color2="black",symbol="leers")

.LongData.plotCurve(ld4,p4a,main="",color="no")
.LongData.plotMean(ld4,p4b,color2="black",symbol="letters")

.LongData.plotCurve(ld4,p4a,main="",color="no")
.LongData.plotMean(ld4,p4b,color2="color",symbol="leers")

.LongData.plotCurve(ld4,p4a,main="",color="no")
.LongData.plotMean(ld4,p4b,color2="color",symbol="letters")


plot(ld4n)

par(mfrow=c(4,3))
plot(ld1)
plot(ld1,mean=FALSE)
plot(ld2)
plot(ld2n)
plot(ld3)
plot(ld3n,mean=FALSE)
plot(ld4,mean=TRUE)
plot(ld4n,mean=TRUE)
plot(ld5)
plot(ld5n)



cat("###################################################################
#################### Test  LongData & Partition ###################
############################### plot ##############################
###################################################################\n")


selectTrajNoNA(ld1n)
selectTrajNoNA(ld2n)
selectTrajNoNA(ld3n)
selectTrajNoNA(ld3n)
selectTrajNoNA(ld3n)
selectTrajNoNA(ld3n)
try(selectTrajNoNA(ld4n))
selectTrajNoNA(ld4n)
selectTrajNoNA(ld5n)
selectTrajNoNA(ld5n)




choicePlot(ld4,p4a)

cat("######### Fin testLongData ########\n")
