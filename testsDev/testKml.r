source("testClusterizLongData.r")
source("../R/kml.r")
dyn.load("./kml.dll")

cleanProg(.partitionInitialise,,,2) # LETTERSletters sums

point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))

paInit <- partitionInitialise(2,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(4,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="randomK")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="randomAll")
plot(points,col=as.integer(paInit["clusters"]))

par(mfrow=c(2,3))

pa <- partitionInitialise(3,243,as.matrix(dist(cld3["traj"])),method="randomK")
plot(ld3,pa)

pa <- partitionInitialise(3,243,as.matrix(dist(cld3["traj"])),method="randomAll")
plot(ld3,pa)

pa <- partitionInitialise(3,243,as.matrix(dist(cld3["traj"])),method="maxDist")
plot(ld3,pa)

pa <- partitionInitialise(3,180,as.matrix(dist(cld4["traj"])),method="randomK")
plot(ld4,pa)

pa <- partitionInitialise(3,180,as.matrix(dist(cld4["traj"])),method="randomAll")
plot(ld4,pa)

pa <- partitionInitialise(3,180,as.matrix(dist(cld4["traj"])),method="maxDist")
plot(ld4,pa)

pa <- partitionInitialise(4,243,as.matrix(dist(cld3["traj"])),method="randomK")
plot(ld3n,pa)

pa <- partitionInitialise(4,243,as.matrix(dist(cld3["traj"])),method="randomAll")
plot(ld3n,pa)

pa <- partitionInitialise(4,243,as.matrix(dist(cld3["traj"])),method="maxDist")
plot(ld3n,pa)

pa <- partitionInitialise(4,180,as.matrix(dist(cld4["traj"])),method="randomK")
plot(ld4n,pa)

pa <- partitionInitialise(4,180,as.matrix(dist(cld4["traj"])),method="randomAll")
plot(ld4n,pa)

pa <- partitionInitialise(4,180,as.matrix(dist(cld4["traj"])),method="maxDist")
plot(ld4n,pa)



###############################################################################
################################# Generalized #################################
###############################################################################

cent2a <- calculCenterGeneralized(ld2["traj"],p2a)
cent2b <- calculCenterGeneralized(ld2["traj"],p2b)
cent2c <- calculCenterGeneralized(ld2["traj"],p2c)
cent2aC <- calculMean(ld2["traj"],p2a)
cent2bC <- calculMean(ld2["traj"],p2b)
cent2cC <- calculMean(ld2["traj"],p2c)
if(identical(c(cent2a[-3,]),c(cent2aC[-3,]))&identical(c(cent2b[-3,]),c(cent2bC[-3,]))&identical(c(cent2c),c(cent2cC))){}else{stop("CalculMean 1\a")}

a <- affectIndivGeneralized(ld2["traj"],cent2a)
b <- affectIndivGeneralized(ld2["traj"],cent2b)
c <- affectIndivGeneralized(ld2["traj"],cent2c)
aC <- affectIndiv(ld2["traj"],cent2a)
bC <- affectIndiv(ld2["traj"],cent2b)
cC <- affectIndiv(ld2["traj"],cent2c)
if(identical(a,aC)&identical(b,bC)&identical(c,cC)){}else{stop("AffectIndiv 1\a")}


#calculCenterGeneralized(ld2["traj"],p2an,centerMethod=medianNA)
#calculCenterGeneralized(ld2["traj"],p2bn)
#try(calculCenterGeneralized(ld2["traj"],p2cn))

#cent2an <- calculCenterGeneralized(ld2n["traj"],p2a,centerMethod=medianNA)
#cent2bn <- calculCenterGeneralized(ld2n["traj"],p2b)
#cent2cn <- calculCenterGeneralized(ld2n["traj"],p2c)
#calculMean(ld2["traj"],p2an)
#calculMean(ld2["traj"],p2bn)
#calculMean(ld2["traj"],p2cn)


#affectIndivGeneralized(ld2["traj"],cent2an)
#affectIndivGeneralized(ld2["traj"],cent2bn)
#affectIndivGeneralized(ld2["traj"],cent2cn)
#affectIndiv(ld2["traj"],cent2an)
#affectIndiv(ld2["traj"],cent2bn)
#affectIndiv(ld2["traj"],cent2cn)

#cent2ann <- calculCenterGeneralized(ld2n["traj"],p2an)
#cent2bnn <- calculCenterGeneralized(ld2n["traj"],p2bn)

#affectIndivGeneralized(ld2["traj"],cent2ann)
#affectIndivGeneralized(ld2["traj"],cent2bnn)

cent3a <- calculCenterGeneralized(ld3["traj"],p3a)
cent3b <- calculCenterGeneralized(ld3["traj"],p3b)
cent3c <- calculCenterGeneralized(ld3["traj"],p3c)
cent3d <- calculCenterGeneralized(ld3["traj"],p3d)
cent3e <- calculCenterGeneralized(ld3["traj"],p3e)
cent3f <- calculCenterGeneralized(ld3["traj"],p3f)

cent3aC <- calculMean(ld3["traj"],p3a)
cent3bC <- calculMean(ld3["traj"],p3b)
cent3cC <- calculMean(ld3["traj"],p3c)
cent3dC <- calculMean(ld3["traj"],p3d)
cent3eC <- calculMean(ld3["traj"],p3e)
cent3fC <- calculMean(ld3["traj"],p3f)
if(all(c(cent3a)-c(cent3aC)<1e-15)&all(c(cent3b)-c(cent3bC)<1e-15)&all(c(cent3c[1:3,])-c(cent3cC[1:3,])<1e-15)&
   all(c(cent3d[1:3,])-c(cent3dC[1:3,])<1e-15)&all(c(cent3e[-21,])-c(cent3eC[-21,])<1e-15)&all(c(cent3f)-c(cent3fC)<1e-15)){
}else{stop("CalculMean 1\a")}

par(mfrow=c(1,2))
af1 <- affectIndivGeneralized(ld3["traj"],cent3a);plot(ld3,af1)
af1C <- affectIndiv(ld3["traj"],cent3a);plot(ld3,af1C)
if(identical(af1,af1C)){}else{stop("AffectIndiv 1\a")}

af2 <- affectIndivGeneralized(ld3["traj"],cent3b);plot(ld3,af2)
af2C <- affectIndiv(ld3["traj"],cent3b);plot(ld3,af2C)
if(identical(af2,af2C)){}else{stop("AffectIndiv 1\a")}

af3 <- affectIndivGeneralized(ld3["traj"],cent3c);plot(ld3,af3)
af3C <- affectIndiv(ld3["traj"],cent3c);plot(ld3,af3C)
if(identical(af3,af3C)){}else{stop("AffectIndiv 1\a")}

af4 <- affectIndivGeneralized(ld3["traj"],cent3d);plot(ld3,af4)
af4C <- affectIndiv(ld3["traj"],cent3d);plot(ld3,af4C)
if(identical(af4,af4C)){}else{stop("AffectIndiv 1\a")}

af5 <- affectIndivGeneralized(ld3["traj"],cent3e);plot(ld3,af5)
af5C <- affectIndiv(ld3["traj"],cent3e);plot(ld3,af5C)
if(identical(af5,af5C)){}else{stop("AffectIndiv 1\a")}

af6 <- affectIndivGeneralized(ld3["traj"],cent3f);plot(ld3,af6)
af6C <- affectIndiv(ld3["traj"],cent3f);plot(ld3,af6C)
if(identical(af6,af6C)){}else{stop("AffectIndiv 1\a")}


af1 <- affectIndivGeneralized(ld3n["traj"],cent3a);plot(ld3n,af1)
af1C <- affectIndiv(ld3n["traj"],cent3a);plot(ld3n,af1C)
if(identical(af1,af1C)){}else{stop("AffectIndiv 1\a")}

af2 <- affectIndivGeneralized(ld3n["traj"],cent3b);plot(ld3n,af2)
af2C <- affectIndiv(ld3n["traj"],cent3b);plot(ld3n,af2C)
if(identical(af2,af2C)){}else{stop("AffectIndiv 1\a")}

af3 <- affectIndivGeneralized(ld3n["traj"],cent3c);plot(ld3n,af3)
af3C <- affectIndiv(ld3n["traj"],cent3c);plot(ld3n,af3C)
if(identical(af3,af3C)){}else{stop("AffectIndiv 1\a")}

af4 <- affectIndivGeneralized(ld3n["traj"],cent3d);plot(ld3n,af4)
af4C <- affectIndiv(ld3n["traj"],cent3d);plot(ld3n,af4C)
if(identical(af4,af4C)){}else{stop("AffectIndiv 1\a")}

af5 <- affectIndivGeneralized(ld3n["traj"],cent3e);plot(ld3n,af5)
af5C <- affectIndiv(ld3n["traj"],cent3e);plot(ld3n,af5C)
if(identical(af5,af5C)){}else{stop("AffectIndiv 1\a")}

af6 <- affectIndivGeneralized(ld3n["traj"],cent3f);plot(ld3n,af6)
af6C <- affectIndiv(ld3n["traj"],cent3f);plot(ld3n,af6C)
if(identical(af6,af6C)){}else{stop("AffectIndiv 1\a")}



cent4a <- calculCenterGeneralized(ld4["traj"],p4a)
cent4b <- calculCenterGeneralized(ld4["traj"],p4b)
cent4c <- calculCenterGeneralized(ld4["traj"],p4c)
cent4d <- calculCenterGeneralized(ld4["traj"],p4d)
cent4e <- calculCenterGeneralized(ld4["traj"],p4e)

cent4aC <- calculMean(ld4["traj"],p4a)
cent4bC <- calculMean(ld4["traj"],p4b)
cent4cC <- calculMean(ld4["traj"],p4c)
cent4dC <- calculMean(ld4["traj"],p4d)
cent4eC <- calculMean(ld4["traj"],p4e)

if(all(c(cent4a)-c(cent4aC)<1e-14)&all(c(cent4b)-c(cent4bC)<1e-14)&all(c(cent4c[1:2,])-c(cent4cC[1:2,])<1e-14)&
   all(c(cent4d)-c(cent4dC)<1e-14)&all(c(cent4e)-c(cent4eC)<1e-15)){
}else{stop("CalculMean 1\a")}

par(mfrow=c(1,2))
af1 <- affectIndivGeneralized(ld4["traj"],cent4a);plot(ld4,af1)
af1C <- affectIndiv(ld4["traj"],cent4a);plot(ld4,af1C)
if(identical(af1,af1C)){}else{stop("AffectIndiv 1\a")}
af2 <- affectIndivGeneralized(ld4["traj"],cent4b);plot(ld4,af2)
af2C <- affectIndiv(ld4["traj"],cent4b);plot(ld4,af2C)
if(identical(af2,af2C)){}else{stop("AffectIndiv 1\a")}
af3 <- affectIndivGeneralized(ld4["traj"],cent4c);plot(ld4,af3)
af3C <- affectIndiv(ld4["traj"],cent4c);plot(ld4,af3C)
if(identical(af3,af3C)){}else{stop("AffectIndiv 1\a")}
af4 <- affectIndivGeneralized(ld4["traj"],cent4d);plot(ld4,af4)
af4C <- affectIndiv(ld4["traj"],cent4d);plot(ld4,af4C)
if(identical(af4,af4C)){}else{stop("AffectIndiv 1\a")}
af5 <- affectIndivGeneralized(ld4["traj"],cent4e);plot(ld4,af5)
af5C <- affectIndiv(ld4["traj"],cent4e);plot(ld4,af5C)
if(identical(af5,af5C)){}else{stop("AffectIndiv 1\a")}





cent4a <- calculCenterGeneralized(ld4n["traj"],p4an)
cent4b <- calculCenterGeneralized(ld4n["traj"],p4bn)
cent4c <- calculCenterGeneralized(ld4n["traj"],p4cn)
cent4d <- calculCenterGeneralized(ld4n["traj"],p4dn)
cent4e <- calculCenterGeneralized(ld4n["traj"],p4en)

cent4aC <- calculMean(ld4n["traj"],p4an)
cent4bC <- calculMean(ld4n["traj"],p4bn)
cent4cC <- calculMean(ld4n["traj"],p4cn)
cent4dC <- calculMean(ld4n["traj"],p4dn)
cent4eC <- calculMean(ld4n["traj"],p4en)

if(all(c(!is.na(cent4a))-c(!is.na(cent4aC))<1e-14)&all(c(!is.na(cent4b))-c(!is.na(cent4bC))<1e-14)&
   all(c(!is.na(cent4c[1:2,]))-c(!is.na(cent4cC[1:2,]))<1e-14)&all(c(!is.na(cent4d))-c(!is.na(cent4dC))<1e-14)&
   all(c(!is.na(cent4e))-c(!is.na(cent4eC))<1e-14)){
}else{stop("CalculMean 1\a")}

par(mfrow=c(1,2))
af1 <- affectIndivGeneralized(ld4n["traj"],cent4a);plot(ld4n,af1)
af1C <- affectIndiv(ld4n["traj"],cent4a);plot(ld4n,af1C)
if(identical(af1,af1C)){}else{stop("AffectIndiv 1\a")}
af2 <- affectIndivGeneralized(ld4n["traj"],cent4b);plot(ld4n,af2)
af2C <- affectIndiv(ld4n["traj"],cent4b);plot(ld4n,af2C)
if(identical(af2,af2C)){}else{stop("AffectIndiv 1\a")}
af3 <- affectIndivGeneralized(ld4n["traj"],cent4c);plot(ld4n,af3)
af3C <- affectIndiv(ld4n["traj"],cent4c);plot(ld4n,af3C)
if(identical(af3,af3C)){}else{stop("AffectIndiv 1\a")}
af4 <- affectIndivGeneralized(ld4n["traj"],cent4d);plot(ld4n,af4)
af4C <- affectIndiv(ld4n["traj"],cent4d);plot(ld4n,af4C)
if(identical(af4,af4C)){}else{stop("AffectIndiv 1\a")}
af5 <- affectIndivGeneralized(ld4n["traj"],cent4e);plot(ld4n,af5)
af5C <- affectIndiv(ld4n["traj"],cent4e);plot(ld4n,af5C)
if(identical(af5,af5C)){}else{stop("AffectIndiv 1\a")}

cent5a <- calculCenterGeneralized(ld5["traj"],p5a)
cent5b <- calculCenterGeneralized(ld5["traj"],p5b)
cent5c <- calculCenterGeneralized(ld5["traj"],p5c)
cent5an <- calculCenterGeneralized(ld5n["traj"],p5a)
cent5bn <- calculCenterGeneralized(ld5n["traj"],p5b)
cent5cn <- calculCenterGeneralized(ld5n["traj"],p5c)

cent5aC <- calculMean(ld5["traj"],p5a)
cent5bC <- calculMean(ld5["traj"],p5b)
cent5cC <- calculMean(ld5["traj"],p5c)
cent5anC <- calculMean(ld5n["traj"],p5a)
cent5bnC <- calculMean(ld5n["traj"],p5b)
cent5cnC <- calculMean(ld5n["traj"],p5c)

if(all(c(cent5a)-c(cent5aC)<1e-14)&all(c(cent5b)-c(cent5bC)<1e-14)&all(c(cent5c)-c(cent5cC)<1e-14)&
   all(c(cent5an)-c(cent5anC)<1e-14)&all(c(cent5bn)-c(cent5bnC)<1e-14)&all(c(cent5cn)-c(cent5cnC)<1e-14)){
}else{stop("CalculMean 1\a")}

af1 <- affectIndiv(ld5["traj"],cent5an);plot(ld5,af1)
af1C <- affectIndivGeneralized(ld5["traj"],cent5an);plot(ld5,af1C)
if(identical(af1,af1C)){}else{stop("AffectIndiv 1\a")}
af2 <- affectIndiv(ld5["traj"],cent5bn);plot(ld5,af2)
af2C <- affectIndivGeneralized(ld5["traj"],cent5bn);plot(ld5,af2C)
if(identical(af2,af2C)){}else{stop("AffectIndiv 1\a")}
af3 <- affectIndiv(ld5["traj"],cent5cn);plot(ld5,af3)
af3C <- affectIndivGeneralized(ld5["traj"],cent5cn);plot(ld5,af3C)
if(identical(af3,af3C)){}else{stop("AffectIndiv 1\a")}
af4 <- affectIndiv(ld5["traj"],cent5an);plot(ld5n,af4)
af4C <- affectIndivGeneralized(ld5["traj"],cent5an);plot(ld5n,af4C)
if(identical(af4,af4C)){}else{stop("AffectIndiv 1\a")}
af5 <- affectIndiv(ld5["traj"],cent5bn);plot(ld5n,af5)
af5C <- affectIndivGeneralized(ld5["traj"],cent5bn);plot(ld5n,af5C)
if(identical(af5,af5C)){}else{stop("AffectIndiv 1\a")}
af6 <- affectIndiv(ld5["traj"],cent5cn);plot(ld5n,af6)
af6C <- affectIndivGeneralized(ld5["traj"],cent5cn);plot(ld5n,af6C)
if(identical(af6,af6C)){}else{stop("AffectIndiv 1\a")}







cleanProg(trajKmlSlow,,,2)   # distEuclideGower meanNA
dev.off()
pi <- partitionInitialise(3,243)
pa1 <- trajKmlSlow(ld3["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld3["traj"],ld3["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[4]!=0){stop("trajKmlSlow \a")}

pi <- partitionInitialise(4,243)
pa1 <- trajKmlSlow(ld3["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld3["traj"],ld3["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[5]!=0){stop("trajKmlSlow \a")}

pi <- partitionInitialise(5,243)
pa1 <- trajKmlSlow(ld3["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld3["traj"],ld3["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[6]!=0){stop("trajKmlSlow \a")}


trajKmlSlow(ld3["traj"],partitionInitialise(4,243),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld3n["traj"],partitionInitialise(4,243),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld3["traj"],partitionInitialise(4,243),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)


pi <- partitionInitialise(2,180)
pa1 <- trajKmlSlow(ld4["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld4["traj"],ld4["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[3]!=0){stop("trajKmlSlow \a")}

pi <- partitionInitialise(3,180)
pa1 <- trajKmlSlow(ld4["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld4["traj"],ld4["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[4]!=0){stop("trajKmlSlow \a")}

pi <- partitionInitialise(4,180)
pa1 <- trajKmlSlow(ld4["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld4["traj"],ld4["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[5]!=0){stop("trajKmlSlow \a")}

pi <- partitionInitialise(5,180)
pa1 <- trajKmlSlow(ld4["traj"],pi,screenPlot=1)
pa2 <- kmeans(ld4["traj"],ld4["traj"][!is.na(pi["clusters"]),],algorithm="Lloyd",iter=100)
if(sort(c(table(pa1[[1]]["clusters"],pa2[[1]])),TRUE)[6]!=0){stop("trajKmlSlow \a")}


trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,centerMethod=medianNA)

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)


### slow, test des différentes options
cleanProg(.clusterizLongData.kml,,,2) # LETTERS meanNA
kml(cld3,3,2,print.traj=TRUE)
kml(cld3n,3,1,print.traj=TRUE,print.cal=TRUE)
kml(cld3n,2:4,3,print.traj=TRUE,print.cal=TRUE,saveFreq=3)
kml(cld3,5,2,print.traj=TRUE,print.cal=TRUE,distanceStartingCond="maximum")
dis <- function(x,y)min(x,y)
kml(cld3,3,1,print.traj=TRUE,print.cal=TRUE,distance=dis)
kml(cld3,3,1,print.traj=TRUE,print.cal=TRUE,startingCond="randomAll")

### fast, test des différentes options
kml(cld3n)
kml(cld3n,3,2,print.cal=TRUE,print.sub=TRUE)
kml(cld4n,2:4,3,print.cal=TRUE,saveFreq=3)
kml(cld4n,2:4,3,print.cal=TRUE,trajMinSize=10)
kml(cld3,5,2,print.cal=TRUE,distanceStartingCond="maximum")
dis <- function(x,y)min(x,y)
kml(cld3,3,1,print.cal=TRUE,distance=dis)
kml(cld3,2:3,1,print.traj=TRUE,print.cal=TRUE)

cleanProg(.Clusterization.export,,,2) # LETTERS, .GlobalEnv
cleanProg(.clusterizLongData.choice,,,0)
