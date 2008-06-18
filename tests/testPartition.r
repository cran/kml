source("../R/partition.r")

cat("####################################################################
########################## Test  Partition #########################
############################# Creation #############################
####################################################################\n")

new("Partition") # Doit marcher meme apres recompilation
new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=2)
new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=4)
new("Partition",clusters=as.factor(c("A","B","C")),id=as.character(c(101,108,3)),nbClusters=4)
new("Partition",clusters=as.factor(c("A","B",NA,"A")),id=as.character(c(101,108,4,3)),nbClusters=3)
new("Partition",clusters=as.factor(c(NA,NA)),id=as.character(c(101,108)),nbClusters=3)

#try(new("Partition",clusters=as.factor(c("A","B")),id=as.character(c(101,108))))
try(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),id=as.character(c(101,108,2)),nbClusters=3))
try(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),id=as.character(c(101,108,2)),nbClusters=2))
try(new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=25))
try(new("Partition",clusters=as.factor(c("A","B","D")),id=as.character(c(101,3)),nbClusters=4))
try(new("Partition",clusters=as.factor(c("A","C",NA,"A")),id=as.character(c(NA,108,4,3)),nbClusters=3))
try(new("Partition",clusters=as.factor(c("A","C",NA,"A")),id=as.character(c(3,108,4,3)),nbClusters=20))
try(new("Partition",clusters=as.factor(c("A","C",NA,"M")),id=as.character(c(1,108,4,3)),nbClusters=3))
try(new("Partition",clusters=factor(c("A","C",NA,"A"),levels=LETTERS[1:3]),id=as.character(c(1,108,4,3)),nbClusters=3))

partition()
partition(clusters=c("A","B"),id=1:2)
partition(clusters=c("A",NA),id=12:13)
try(partition(nbClusters=3))
partition(clusters=c("C","B","C"),nbClusters=4,id=c("A1","A3","A33"))
partition(clusters=c(NA,NA),id=c(1,5))
partition(clusters=c("A","C","A"),nbClusters=4,id=c(3,5,7))


cat("### Jeux de données ###")

p0a <- p0b <- partition()

p1a <- partition(clusters=c("A","A","B"),id=1:3)
p1b <- partition(clusters=c("A","B","A"),id=c(2,4,6))
p1c <- partition(nbClusters=2,clusters=c("A","B","B"),id=1:3) # Réarrangement pour avoir le plus gros cluster en A

p2a <- partition(nbClusters=3,clusters=c("A","A","B"),id=1:3)
p2b <- partition(nbClusters=3,clusters=c("A","B","A"),id=1:3)
p2c <- partition(nbClusters=3,clusters=c("A","C","B"),id=1:3)

p2an <- partition(nbClusters=3,clusters=c("A",NA,"B"),id=1:3)
p2bn <- partition(nbClusters=3,clusters=c("A",NA,NA),id=1:3)
p2cn <- partition(nbClusters=3,clusters=c(NA,NA,NA),id=1:3)

p3a <- partition(nbClusters=9,clusters=rep(LETTERS[1:9],27),id=1:243)
p3b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],81),id=1:243)
p3b@clusters[1:6]<-"A"
p3b@clusters[7:9]<-"B"
p3c <- partition(nbClusters=5,clusters=rep(LETTERS[1:3],81),id=1:243)

p4a <- partition(nbClusters=2,clusters=rep(LETTERS[1:2],c(80,100)),id=1:180)
p4b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(50,30,100)),id=1:180)
p4c <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(80,100,0)),id=1:180)
p4d <- partition(nbClusters=4,clusters=rep(LETTERS[1:4],c(50,30,50,50)),id=1:180)
p4e <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(60,60,60)),id=1:180)

p4an <- p4a
for(i in 1:20){p4an@clusters[round(runif(1,1,180))] <- NA}
p4bn <- p4b
for(i in 1:30){p4bn@clusters[round(runif(1,1,180))] <- NA}
p4cn <- p4c
for(i in 1:60){p4cn@clusters[round(runif(1,1,180))] <- NA}
p4dn <- p4d
for(i in 1:90){p4dn@clusters[round(runif(1,1,180))] <- NA}
p4en <- p4e
for(i in 1:160){p4en@clusters[round(runif(1,1,180))] <- NA}


p5a <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1,2,1,2,1,2)],id=101:108)
p5b <- partition(nbClusters=3,clusters=LETTERS[c(1,2,3,1,2,3,1,2)],id=101:108)
p5c <- partition(nbClusters=4,clusters=c("A","A","B","A","C","D","D","C"),id=101:108)
p5cn <- partition(nbClusters=4,clusters=c("A","A","B","A",NA,NA,"D","C"),id=101:108)


cat("\n####################################################################
########################## Test  Partition #########################
############################# Accesseurs ###########################
####################################################################\n")

getNbClusters(p0a)
getNbClusters(p1a)
getNbClusters(p1b)
getNbClusters(p1c)
getNbClusters(p2a)
getNbClusters(p2b)
getNbClusters(p2c)
getNbClusters(p3a)
getNbClusters(p3b)
getNbClusters(p3c)
getNbClusters(p4a)
getNbClusters(p4b)
getNbClusters(p4c)
getNbClusters(p4d)
getNbClusters(p4e)
getNbClusters(p5a)
getNbClusters(p5b)
getNbClusters(p5c)

getClusters(p0a)
getClusters(p1a)
getClusters(p1b)
getClusters(p1c)
getClusters(p2a)
getClusters(p2b)
getClusters(p2c)
getClusters(p3a)
getClusters(p3b)
getClusters(p3c)
getClusters(p4a)
getClusters(p4b)
getClusters(p4c)
getClusters(p4d)
getClusters(p4e)
getClusters(p5a)
getClusters(p5b)
getClusters(p5c)

getId(p0a)
getId(p1a)
getId(p1b)
getId(p1c)
getId(p2a)
getId(p2b)
getId(p2c)
getId(p3a)
getId(p3b)
getId(p3c)
getId(p4a)
getId(p4b)
getId(p4c)
getId(p4d)
getId(p4e)
getId(p5a)
getId(p5b)
getId(p5c)


setNbClusters(p1a)<-4
try(setNbClusters(p1a)<-0)
setNbClusters(p1b)<-2
try(setNbClusters(p0a)<-4)
try(setNbClusters(p0a)<-0)
setNbClusters(p1a)<-4
setId(p1a)<-4:6
try(setId(p1a)<-4:7)
try(setId(p1a)<-c(4,5,NA))
try(setNbClusters(p1a)<-1)

setClusters(p1a)<-c("A","B","A")
setNbClusters(p1a)<-2
try(setClusters(p1a)<-c("A","B","C"))
setNbClusters(p1a)<-3
setClusters(p1a)<-c("A","B","C")
try(setClusters(p0a)<-c("A","B","C"))
p1a <- partition(clusters=c("A","A","B"),id=c("A2","A3","B1"))



cat("\n####################################################################
########################## Test  Partition #########################
############################# Affichage ############################
####################################################################\n")

p0a
p1a
p4d


print(p0a)
print(p1a)
print(p1b)
print(p1c)
print(p2a)
print(p2b)
print(p2c)
print(p3a)
print(p3b)
print(p3c)
print(p4a)


cat("\n####################################################################
########################## Test  Partition #########################
############################### Autre ##############################
####################################################################\n")

expandPartition(p1b,listId=1:6)
