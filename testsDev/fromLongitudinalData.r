cat("### Jeux de données de longitudinalData ###\n")

ld0 <- longData()
ld1 <- longData(traj=matrix(c(NA,2,3,NA,4,6,NA,8,10),3),idAll=1:3,time=1:3)
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),idAll=c(11,12,13),time=c(2,4,8),varNames="T")
ld1 <- longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),idAll=c(11,12,13),time=c(2,4,8),varNames="T")
ld1n <- longData(idAll=c(1,2,3),time=c(2,4,8),varNames="T",traj=array(c(1,NA,3,1,4,NA,NA,6,10),dim=c(3,3)))

dn2 <- data.frame(idAll=c(10,17,28,29,31),t1=c(5,4,2,1,0),t2=c(5,4,3,2,1),t4=c(4,2,3,1,1),t5=c(5,6,2,1,0))
ld2 <- longData(dn2[,-1],idAll=dn2[,1],time=c(1,2,5,6))
ld2 <- longData(dn2,time=c(1,2,5,6))
dn2[1,2]<- NA;dn2[3,3]<- NA;dn2[2,2]<- NA;dn2[2,5]<- NA;dn2[4,3]<-NA;dn2[4,4]<-NA
ld2n <- longData(dn2,time=c(1,2,5,6),maxNA=1)

data3 <- rbind(c(1,2 ,NA,4 ),
               c(1,1 ,NA,1 ),
               c(2,3 ,4 ,5 ),
               c(2,2 ,2 ,2 ),
               c(3,NA,NA,6 ),
               c(3,NA,NA,3 ),
               c(2,4 ,4 ,NA),
               c(2,3 ,2 ,NA))
dim(data3) <- c(8,4)

ld3n <- longData(data3,timeInData=c(1,3,4))

ld3n <- longData(data3)

dim(data3) <- c(8,4)
ld3n <- longData(data3)

data3Imp <- rbind(c(1,2,3,4),
                  c(1,3,1,1),
                  c(2,3,4,5),
                  c(2,2,2,12),
                  c(3,4,4,6),
                  c(3,3,3,3),
                  c(2,4,8,5),
                  c(2,3,4,1))
dim(data3Imp) <- c(8,4)
ld3 <- longData(data3Imp)


dn4 <- dn4n <- read.csv2("../../longitudinalData/testsDev/divergingLines.csv")[c(1:180,101:120),]
ld4 <- longData(dn4[,-1],idAll=(1:200)*2,time=c(0,1,2,3,4,6,8,10,12,16,20))

for(i in 1:600){dn4n[floor(runif(1,1,201)),floor(runif(1,2,13))]<-NA}
dn4n[2,-1]<-NA
dn4n[,6]<-NA
ld4n <- longData(dn4n[,-1],time=c(0,1,2,3,4,6,8,10,12,16,20),idAll=(1:200)*2)



dn5 <- dn5n <- read.csv2("../../../Recherche/Anorexie Tamara/trajectoires de soins.csv")[rep(1:200,10),-29]
ld5 <- longData(dn5[,-1],idAll=1:2000)

for(i in 1:1000){dn5n[floor(runif(1,1,2000)),floor(runif(1,2,29))]<-NA}
ld5n <- longData(dn5n[,-1],idAll=1:2000)



tr1 <- tr1n <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2),
            dimnames=list(c(101,102,104),c("t1","t2","t4","t8","t16"),c("P","A"))
            )
tr1n[1,2,1] <- NA; tr1n[2,4,2] <- NA; tr1n[3,1,2] <- NA; tr1n[3,3,2] <- NA;

tr2 <- array(c(1,2,3, 1,4,3, 6,1,8, 10,1,2,
              6,1,8, 10,1,2, 1,3,2, 4,2,5,
              1,3,2, 4,2,5, 6,3,4, 3,4,4,
              4,7,6, 5,5,4,  4,7,6, 5,5,4),
            dim=c(4,3,4),
            dimnames=list(c("i1","i2","i3","i4"),c("t1","t2","t4"),c("P","A","E","R"))
            )

tr3n <- array(c(1,NA,NA, 1,4,3,
              NA,1,8, 10,NA,2,
              4,NA,6, NA,5,4),
            dim=c(3,3,2),
            dimnames=list(c("i1","i2","i3"),c("t1","t2","t4"),c("P","A"))
            )

tr4n <- array(c(NA,NA,2,
               NA,NA,1,
               NA,NA,NA,
               NA,3,2,
               2,NA,1,
               1,2,1,

               3,NA,NA,
               4,NA,6,
               5,2,4,
               2,NA,NA,
               NA,4,2,
               1,NA,2),
            dim=c(3,6,2),
            dimnames=list(c("t1","t2","t4"),c(101,102,103,105,106,107),c("P","A"))
            )
tr4n <- aperm(tr4n,c(2,1,3))


p0a <- p0b <- partition()

p1a <- partition(clusters=c("A","B","B"))
p1b <- partition(clusters=c("A","B","A"),ld1)
p1b <- partition(clusters=c("A","B","A"),ld1n)
p1c <- partition(clusters=c("A","C","B"),ld1)
p1d <- partition(clusters=c("A","C","A"),ld2n)
p1d <- partition(clusters=c("A","C","A"),ld2n)

p2a <- partition(clusters=c("A","A","B","A","B"))
(p2b <- partition(clusters=c("A","B","A","A","B"),ld2,details=c(convergenceTime="3",algorithm="kml",aze=4,multiplicity="4")))
p2c <- partition(clusters=c("A","C","B","A","B"),ld2)

p2an <- partition(clusters=c("A",NA,"B",NA,"B"))
p2bn <- partition(clusters=c("A",NA,NA,"B",NA))
tryBug(p2cn <- partition(clusters=c(NA,NA,NA,NA,NA)))

p3a <- partition(clusters=rep(LETTERS[1:2],4))
p3b <- partition(clusters=rep(LETTERS[c(1:3,1:3,1:2)]),ld3n,details=c(multiplicity="1"))
p3c <- partition(clusters=rep(LETTERS[1:4],each=2),ld3)
p3d <- partition(clusters=rep(c(1,2,1,3,2,3,3,2)),ld3)
p3e <- partition(clusters=rep(c(4,4,1,3,2,3,3,2)),ld3)
p3f <- partition(clusters=rep(c(1,1,1,3,3,3,3,2)),ld3)
p3g <- partition(clusters=rep(c(6,5,4,3,2,3,1,2)),ld3)
p3h <- partition(clusters=rep(c(2,2,1,2,2,3,3,2)),ld3)
p3i <- partition(clusters=rep(c(1,2,3,3,3,3,3,2)),ld3)
p3j <- partition(clusters=rep(c(1,2,1,3,4,3,4,2)),ld3)


p4a <- partition(clusters=c(rep(1:2,each=100)))
p4b <- partition(clusters=c(rep(1:3,c(50,30,120))),ld4)
tryBug(p4b["clusters"][1:6]<-"B")
tryBug(p4b["clusters"][7:9]<-"C")
p4c <- partition(clusters=c(rep(c(1:4,2:3,2:3),25)),ld4)
p4d <- partition(clusters=c(rep(c(3,2:4,1,3:5),25)),ld4)
p4e <- partition(clusters=c(rep(c(1:6,2,2),25)),ld4)
p4f <- partition(clusters=c(rep(c(1:7,4),25)),ld4)

p5a <- partition(clusters=LETTERS[rep(c(1,2),1000)])
p5b <- partition(clusters=LETTERS[c(rep(c(1,2,3),666),1:2)],ld5)
p5c <- partition(clusters=rep(1:4,500),ld5)
p5e <- partition(clusters=c(rep(1:6,333),1,2),ld5)
p5d <- partition(clusters=c(rep(1:8,250)),ld5)




cat("--- Fin jeux de données de longitudinalData ---\n")
