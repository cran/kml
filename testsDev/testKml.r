source("../R/kml.r")

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

affectIndivGeneralized(ld2["traj"],cent2a)
affectIndivGeneralized(ld2["traj"],cent2b)
affectIndivGeneralized(ld2["traj"],cent2c)

calculCenterGeneralized(ld2["traj"],p2an,centerMethod=medianNA)
calculCenterGeneralized(ld2["traj"],p2bn)
try(calculCenterGeneralized(ld2["traj"],p2cn))

cent2an <- calculCenterGeneralized(ld2n["traj"],p2a,centerMethod=medianNA)
cent2bn <- calculCenterGeneralized(ld2n["traj"],p2b)
cent2cn <- calculCenterGeneralized(ld2n["traj"],p2c)

affectIndivGeneralized(ld2["traj"],cent2an)
affectIndivGeneralized(ld2["traj"],cent2bn)
affectIndivGeneralized(ld2["traj"],cent2cn)

cent2ann <- calculCenterGeneralized(ld2n["traj"],p2an)
cent2bnn <- calculCenterGeneralized(ld2n["traj"],p2bn)

affectIndivGeneralized(ld2["traj"],cent2ann)
affectIndivGeneralized(ld2["traj"],cent2bnn)

cent3a <- calculCenterGeneralized(ld3["traj"],p3a)
(cent3b <- calculCenterGeneralized(ld3["traj"],p3b,centerMethod=medianNA))
(cent3b <- calculCenterGeneralized(ld3["traj"],p3b))
cent3c <- calculCenterGeneralized(ld3["traj"],p3c)
cent3d <- calculCenterGeneralized(ld3["traj"],p3d,centerMethod=medianNA)
cent3e <- calculCenterGeneralized(ld3["traj"],p3e)
cent3f <- calculCenterGeneralized(ld3["traj"],p3f,centerMethod=medianNA)

af <- affectIndivGeneralized(ld3["traj"],cent3a)
plot(ld3,af)
af <- affectIndivGeneralized(ld3["traj"],cent3b)
plot(ld3,af)
af <- affectIndivGeneralized(ld3["traj"],cent3c)
plot(ld3,af)
af <- affectIndivGeneralized(ld3["traj"],cent3d)
plot(ld3,af)
af <- affectIndivGeneralized(ld3["traj"],cent3e)
plot(ld3,af)
af <- affectIndivGeneralized(ld3["traj"],cent3f)
plot(ld3,af)

cent3an <- calculCenterGeneralized(ld3n["traj"],p3a,centerMethod=medianNA)
cent3bn <- calculCenterGeneralized(ld3n["traj"],p3b)
cent3cn <- calculCenterGeneralized(ld3n["traj"],p3c,centerMethod=medianNA)
cent3dn <- calculCenterGeneralized(ld3n["traj"],p3d)
cent3en <- calculCenterGeneralized(ld3n["traj"],p3e,centerMethod=medianNA)
cent3fn <- calculCenterGeneralized(ld3n["traj"],p3f)

af <- affectIndivGeneralized(ld3n["traj"],cent3an)
plot(ld3n,af)
af <- affectIndivGeneralized(ld3n["traj"],cent3bn)
plot(ld3n,af)
af <- affectIndivGeneralized(ld3n["traj"],cent3cn)
plot(ld3n,af)
af <- affectIndivGeneralized(ld3n["traj"],cent3dn)
plot(ld3n,af)
af <- affectIndivGeneralized(ld3n["traj"],cent3en)
plot(ld3n,af)
af <- affectIndivGeneralized(ld3n["traj"],cent3fn)
plot(ld3n,af)


cent4a <- calculCenterGeneralized(ld4["traj"],p4a)
(cent4b <- calculCenterGeneralized(ld4["traj"],p4b))
(cent4b <- calculCenterGeneralized(ld4["traj"],p4b,centerMethod=medianNA))
cent4c <- calculCenterGeneralized(ld4["traj"],p4c)
cent4d <- calculCenterGeneralized(ld4["traj"],p4d,centerMethod=medianNA)
cent4e <- calculCenterGeneralized(ld4["traj"],p4e)

af <- affectIndivGeneralized(ld4["traj"],cent4a)
plot(ld4,af)
af <- affectIndivGeneralized(ld4["traj"],cent4b)
plot(ld4,af)
af <- affectIndivGeneralized(ld4["traj"],cent4c)
plot(ld4,af)
af <- affectIndivGeneralized(ld4["traj"],cent4d)
plot(ld4,af)
af <- affectIndivGeneralized(ld4["traj"],cent4e)
plot(ld4,af)



cent2a <- calculMean(ld2["traj"],p2a)
cent2b <- calculMean(ld2["traj"],p2b)
cent2c <- calculMean(ld2["traj"],p2c)

affectIndiv(ld2["traj"],cent2a)
affectIndiv(ld2["traj"],cent2b)
affectIndiv(ld2["traj"],cent2c)

calculMean(ld2["traj"],p2an)
calculMean(ld2["traj"],p2bn)
calculMean(ld2["traj"],p2cn)

affectIndiv(ld2["traj"],cent2an)
affectIndiv(ld2["traj"],cent2bn)
affectIndiv(ld2["traj"],cent2cn)

cent2an <- calculMean(ld2n["traj"],p2a)
cent2bn <- calculMean(ld2n["traj"],p2b)
cent2cn <- calculMean(ld2n["traj"],p2c)

cent2ann <- calculMean(ld2n["traj"],p2an)
cent2bnn <- calculMean(ld2n["traj"],p2bn)

cent3a <- calculMean(ld3["traj"],p3a)
cent3b <- calculMean(ld3["traj"],p3b)
cent3c <- calculMean(ld3["traj"],p3c)
cent3d <- calculMean(ld3["traj"],p3d)
cent3e <- calculMean(ld3["traj"],p3e)
cent3f <- calculMean(ld3["traj"],p3f)

af <- affectIndiv(ld3["traj"],cent3a)
plot(ld3,af)
af <- affectIndiv(ld3["traj"],cent3b)
plot(ld3,af)
af <- affectIndiv(ld3["traj"],cent3c)
plot(ld3,af)
af <- affectIndiv(ld3["traj"],cent3d)
plot(ld3,af)
af <- affectIndiv(ld3["traj"],cent3e)
plot(ld3,af)
af <- affectIndiv(ld3["traj"],cent3f)
plot(ld3,af)

cent3an <- calculMean(ld3n["traj"],p3a)
cent3bn <- calculMean(ld3n["traj"],p3b)
cent3cn <- calculMean(ld3n["traj"],p3c)
cent3dn <- calculMean(ld3n["traj"],p3d)
cent3en <- calculMean(ld3n["traj"],p3e)
cent3fn <- calculMean(ld3n["traj"],p3f)

af <- affectIndiv(ld3n["traj"],cent3an)
plot(ld3n,af)
af <- affectIndiv(ld3n["traj"],cent3bn)
plot(ld3n,af)
af <- affectIndiv(ld3n["traj"],cent3cn)
plot(ld3n,af)
af <- affectIndiv(ld3n["traj"],cent3dn)
plot(ld3n,af)
af <- affectIndiv(ld3n["traj"],cent3en)
plot(ld3n,af)
af <- affectIndiv(ld3n["traj"],cent3fn)
plot(ld3n,af)

cent4a <- calculMean(ld4["traj"],p4a)
cent4b <- calculMean(ld4["traj"],p4b)
cent4c <- calculMean(ld4["traj"],p4c)
cent4d <- calculMean(ld4["traj"],p4d)
cent4e <- calculMean(ld4["traj"],p4e)

af <- affectIndiv(ld4["traj"],cent4a)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4b)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4c)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4d)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4e)
plot(ld4,af)

cent4an <- calculMean(ld4n["traj"],p4an)
cent4bn <- calculMean(ld4n["traj"],p4bn)
cent4cn <- calculMean(ld4n["traj"],p4cn)
cent4dn <- calculMean(ld4n["traj"],p4dn)
cent4en <- calculMean(ld4n["traj"],p4en)

af <- affectIndiv(ld4["traj"],cent4an)
plot(ld4n,af)
af <- affectIndiv(ld4["traj"],cent4bn)
plot(ld4n,af)
af <- affectIndiv(ld4["traj"],cent4cn)
plot(ld4n,af)
af <- affectIndiv(ld4["traj"],cent4dn)
plot(ld4n,af)
af <- affectIndiv(ld4["traj"],cent4en)
plot(ld4n,af)

cent4ann <- calculMean(ld4n["traj"],p4an)
cent4bnn <- calculMean(ld4n["traj"],p4bn)
cent4cnn <- calculMean(ld4n["traj"],p4cn)
cent4dnn <- calculMean(ld4n["traj"],p4dn)
cent4enn <- calculMean(ld4n["traj"],p4en)

af <- affectIndiv(ld4["traj"],cent4an)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4bn)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4cn)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4dn)
plot(ld4,af)
af <- affectIndiv(ld4["traj"],cent4en)
plot(ld4,af)

cent5a <- calculMean(ld5["traj"],p5a)
cent5b <- calculMean(ld5["traj"],p5b)
cent5c <- calculMean(ld5["traj"],p5c)
cent5an <- calculMean(ld5n["traj"],p5a)
cent5bn <- calculMean(ld5n["traj"],p5b)
cent5cn <- calculMean(ld5n["traj"],p5c)

af <- affectIndiv(ld5["traj"],cent5an)
plot(ld5,af)
af <- affectIndiv(ld5["traj"],cent5bn)
plot(ld5,af)
af <- affectIndiv(ld5["traj"],cent5cn)
plot(ld5,af)
af <- affectIndiv(ld5["traj"],cent5an)
plot(ld5n,af)
af <- affectIndiv(ld5["traj"],cent5bn)
plot(ld5n,af)
af <- affectIndiv(ld5["traj"],cent5cn)
plot(ld5n,af)

