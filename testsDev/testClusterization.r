library(codetools)
library(longitudinalData)

cleanProg <- function(realResult,theoResult="",result=TRUE,tolerance=0){
  functionNames <- strsplit(deparse(substitute(realResult)),"\\(")[[1]][1]
  if(identical(theoResult,"")==FALSE){
    if( isTRUE(all.equal( realResult , theoResult ))!=result ){
      cat("WARNING(PreTest2) in    ",functionNames,":",deparse(substitute(realResult)), " == ",theoResult," is not ",result,"\a\n\a")
    }
  }else{}
  if(length(findGlobals(get(functionNames),FALSE)$variables)  > tolerance){
    cat("WARNIGS(detectGlobal) in ",functionNames,": These are the globals:",findGlobals(get(functionNames),FALSE)$variables,"\a\n")
  }else{}
}

### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld

source("../R/global.r")
source("../R/clusterization.r")

cat("####################################################################
######################## Test  Clusterization ######################
############################## Création ############################
####################################################################\n")

new("Clusterization")
cleanProg(.Clusterization.validity,,,)
cleanProg(.Clusterization.show,,,1) #LETTERS


ld0 <- longData()
ld1 <- longData(traj=matrix(c(NA,2,3,NA,4,6,NA,8,10),3),id=1:3,time=1:3)
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),id=c(11,12,13),time=c(2,4,8),varName="T")
ld1n <- longData(id=c(1,2,3),time=c(2,4,8),varName="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(id=c(10,17,28),T1=c(5,4,2),T2=c(5,4,3),T4=c(4,2,3),T5=c(5,6,2))
ld2 <- as.longData(dn2,timeReal=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA
ld2n <- as.longData(dn2,timeReal=c(1,2,5,6))

dn3 <- read.csv2("../../../../Anorexie Tamara/trajectoires de soins.csv")[,-29]
ld3 <- as.longData(dn3)

for(i in 1:2400){dn3[floor(runif(1,1,244)),floor(runif(1,2,29))]<-NA}
ld3n <- as.longData(dn3)

dn4 <- read.csv2("../../divergingLines.csv")
ld4 <-as.longData(dn4,id=(1:180)*2,timeReal=c(0,1,2,3,4,6,8,10,12,16,20))

for(i in 1:600){dn4[floor(runif(1,1,181)),floor(runif(1,2,13))]<-NA}
#dn4[5,]<-NA
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
ld5n <- as.longData(as.data.frame(cbind(1:8,data5)))
data5Imp <- rbind(c(1,2,3,4),
                  c(1,3,1,1),
                  c(2,3,4,5),
                  c(2,2,2,12),
                  c(3,4,4,6),
                  c(3,3,3,3),
                  c(2,4,8,5),
                  c(2,3,4,1))
dim(data5Imp) <- c(8,4)
ld5 <- as.longData(as.data.frame(cbind(1:8,data5Imp)))

p0a <- p0b <- partition()

p1a <- partition(clusters=c("A","B","B"))
p1a <- ordered(p1a)
p1b <- partition(clusters=c("A","B","A"))
p1c <- partition(nbClusters=2,clusters=c("A","B","B")) # Réarrangement pour avoir le plus gros cluster en A

p2a <- partition(nbClusters=3,clusters=c("A","A","B"))
p2b <- partition(nbClusters=3,clusters=c("A","B","A"))
p2c <- partition(nbClusters=3,clusters=c("A","C","B"))

p2an <- partition(nbClusters=3,clusters=c("A",NA,"B"))
p2bn <- partition(nbClusters=3,clusters=c("A",NA,NA))
p2cn <- partition(nbClusters=3,clusters=c(NA,NA,NA))

p3a <- partition(nbClusters=9,clusters=rep(LETTERS[1:9],27))
p3b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],81))
p3b["clusters"][1:6]<-"B"
p3b["clusters"][7:9]<-"C"
p3b <- ordered(p3b)
p3c <- partition(nbClusters=5,clusters=rep(LETTERS[1:3],81))
p3d <- partition(nbClusters=25,clusters=rep(LETTERS[1:3],81))
p3d <- partition(nbClusters=25,clusters=rep(LETTERS[1:3],81))
p3e <- partition(nbClusters=21,clusters=rep(LETTERS[c(1:20,1:4,1:2,1)],9))
p3f <- partition(nbClusters=18,clusters=rep(LETTERS[c(1:18,1:9)],9))

p4a <- partition(nbClusters=2,clusters=rep(LETTERS[1:2],c(80,100)))
p4b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(50,30,100)))
p4c <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(80,100,0)))
p4d <- partition(nbClusters=4,clusters=rep(LETTERS[1:4],c(50,30,50,50)))
p4e <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(60,60,60)))

p4an <- p4a
for(i in 1:20){p4an@clusters[round(runif(1,1,180))] <- NA}
p4an <- partition(nbClusters=p4an["nbClusters"],clusters=p4an["clusters"])
validObject(p4an)

p4bn <- p4b
for(i in 1:30){p4bn@clusters[round(runif(1,1,180))] <- NA}
p4bn <- partition(nbClusters=p4bn["nbClusters"],clusters=p4bn["clusters"])
validObject(p4bn)

p4cn <- p4c
for(i in 1:60){p4cn@clusters[round(runif(1,1,180))] <- NA}
p4cn <- partition(nbClusters=p4cn["nbClusters"],clusters=p4cn["clusters"])
validObject(p4cn)

p4dn <- p4d
for(i in 1:90){p4dn@clusters[round(runif(1,1,180))] <- NA}
p4dn <- partition(nbClusters=p4dn["nbClusters"],clusters=p4dn["clusters"])
validObject(p4dn)

p4en <- p4e
for(i in 1:160){p4en@clusters[round(runif(1,1,180))] <- NA}
p4en <- partition(nbClusters=p4en["nbClusters"],clusters=p4en["clusters"])
validObject(p4en)

p5a <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1,2,1,2,1,2)])
p5b <- partition(nbClusters=3,clusters=LETTERS[c(1,2,3,1,2,3,1,2)])
p5c <- partition(nbClusters=4,clusters=c("A","A","B","A","C","D","D","C"))
p5cn <- partition(nbClusters=4,clusters=c("A","A","B","A",NA,NA,"D","C"))






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

