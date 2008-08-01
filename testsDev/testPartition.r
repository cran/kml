source("../R/partition.r")

cat("####################################################################
########################## Test  Partition #########################
############################# Creation #############################
####################################################################\n")

new("Partition") # Doit marcher meme apres recompilation
new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=2)
new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=4)
new("Partition",clusters=as.factor(c("A","M","C")),id=as.character(c(101,108,3)),nbClusters=20)
new("Partition",clusters=as.factor(c("A","B",NA,"A")),id=as.character(c(101,108,4,3)),nbClusters=3)
new("Partition",clusters=as.factor(c(NA,NA)),id=as.character(c(101,108)),nbClusters=3)
new("Partition",clusters=as.factor(c("A","B","A")),id=as.character(c(101,108,2)),nbClusters=25)

#try(new("Partition",clusters=as.factor(c("A","B")),id=as.character(c(101,108))))
try(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),id=as.character(c(101,108,2)),nbClusters=3))
try(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),id=as.character(c(101,108,2)),nbClusters=2))
try(new("Partition",clusters=as.factor(c("A","n","D")),id=as.character(c(101,3)),nbClusters=4))
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
p3d <- partition(nbClusters=25,clusters=rep(LETTERS[1:3],81),id=1:243)
p3d <- partition(nbClusters=25,clusters=rep(LETTERS[1:3],81),id=1:243)
p3e <- partition(nbClusters=21,clusters=rep(LETTERS[c(1:20,1:4,1:2,1)],9),id=1:243)
p3f <- partition(nbClusters=18,clusters=rep(LETTERS[c(1:18,1:9)],9),id=1:243)

p4a <- partition(nbClusters=2,clusters=rep(LETTERS[1:2],c(80,100)),id=1:180)
p4b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(50,30,100)),id=1:180)
p4c <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(80,100,0)),id=1:180)
p4d <- partition(nbClusters=4,clusters=rep(LETTERS[1:4],c(50,30,50,50)),id=1:180)
p4e <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(60,60,60)),id=1:180)

p4an <- p4a
for(i in 1:20){p4an@clusters[round(runif(1,1,180))] <- NA}
p4an <- partition(id=p4an["id"],nbClusters=p4an["nbClusters"],clusters=p4an["clusters"])
validObject(p4an)

p4bn <- p4b
for(i in 1:30){p4bn@clusters[round(runif(1,1,180))] <- NA}
p4bn <- partition(id=p4bn["id"],nbClusters=p4bn["nbClusters"],clusters=p4bn["clusters"])
validObject(p4bn)

p4cn <- p4c
for(i in 1:60){p4cn@clusters[round(runif(1,1,180))] <- NA}
p4cn <- partition(id=p4cn["id"],nbClusters=p4cn["nbClusters"],clusters=p4cn["clusters"])
validObject(p4cn)

p4dn <- p4d
for(i in 1:90){p4dn@clusters[round(runif(1,1,180))] <- NA}
p4dn <- partition(id=p4dn["id"],nbClusters=p4dn["nbClusters"],clusters=p4dn["clusters"])
validObject(p4dn)

p4en <- p4e
for(i in 1:160){p4en@clusters[round(runif(1,1,180))] <- NA}
p4en <- partition(id=p4en["id"],nbClusters=p4en["nbClusters"],clusters=p4en["clusters"])
validObject(p4en)

p5a <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1,2,1,2,1,2)],id=101:108)
p5b <- partition(nbClusters=3,clusters=LETTERS[c(1,2,3,1,2,3,1,2)],id=101:108)
p5c <- partition(nbClusters=4,clusters=c("A","A","B","A","C","D","D","C"),id=101:108)
p5cn <- partition(nbClusters=4,clusters=c("A","A","B","A",NA,NA,"D","C"),id=101:108)


cat("\n####################################################################
########################## Test  Partition #########################
############################# Accesseurs ###########################
####################################################################\n")

p0a["nbClusters"]
p1a["nbClusters"]
p1b["nbClusters"]
p1c["nbClusters"]
p2a["nbClusters"]
p2bn["nbClusters"]
p2c["nbClusters"]
p3a["nbClusters"]
p3b["nbClusters"]
p3c["nbClusters"]
p4a["nbClusters"]
p4bn["nbClusters"]
p4c["nbClusters"]
p4dn["nbClusters"]
p4e["nbClusters"]
p5a["nbClusters"]
p5b["nbClusters"]
p5cn["nbClusters"]

p0a["clusters"]
p1a["clusters"]
p1b["clusters"]
p1c["clusters"]
p2an["clusters"]
p2bn["clusters"]
p2c["clusters"]
p3a["clusters"]
p3b["clusters"]
p3c["clusters"]
p4an["clusters"]
p4b["clusters"]
p4cn["clusters"]
p4dn["clusters"]
p4e["clusters"]
p5a["clusters"]
p5b["clusters"]
p5cn["clusters"]

p0a["id"]
p1a["id"]
p1b["id"]
p1c["id"]
p2an["id"]
p2bn["id"]
p2c["id"]
p3a["id"]
p3b["id"]
p3c["id"]
p4an["id"]
p4b["id"]
p4cn["id"]
p4dn["id"]
p4e["id"]
p5a["id"]
p5b["id"]
p5c["id"]

p1a["nbClusters"]<-4
try(p1a["nbClusters"]<-0)
p1b["nbClusters"]<-2
try(p0a["nbClusters"]<-4)
p1a["id"]<-4:6
try(p1a["id"]<-4:7)
try(p1a["id"]<-c(4,5,NA))
try(p1a["nbClusters"]<-1)

p1a["clusters"]<-c("A","B","A")
p1a["nbClusters"]<-2
try(p1a["clusters"]<-c("A","B","C"))
p1a["nbClusters"]<-3
p1a["clusters"]<-c("A","B","C")
p1a <- partition(clusters=c("A","A","B"),id=c("A2","A3","B1"))



cat("\n####################################################################
########################## Test  Partition #########################
############################# Affichage ############################
####################################################################\n")

p0a
p1a
p4dn


cat("\n####################################################################
########################## Test  Partition #########################
############################### Autre ##############################
####################################################################\n")

expandPartition(p1b,listId=1:6)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test Partition ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
