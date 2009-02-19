source("../R/imputation.r")

cat("###########################################################
########################## LOCF ###########################
")
### Sous fonctions
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- .LongData.LocfBegin(a)
a <- .LongData.LocfMiddleEnd(a)

a <- c(1,NA)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

a <- c(NA,-1)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

a <- c(1)
a <- .LongData.LocfBegin(a)
(a <- .LongData.LocfMiddleEnd(a))

### function complete
par(mfrow=c(4,5))
a <- c(NA,2,NA,4,NA,1,2,NA,NA,-5,NA,3,NA,NA)
a2 <- .LongData.Locf(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
a2 <- .LongData.Locf(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- .LongData.Locf(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
a2 <- .LongData.Locf(a)

a <- c(1,NA)
a2 <- .LongData.Locf(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,-1)
a2 <- .LongData.Locf(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
.LongData.Locf(a)


###################################################################
###################### Interpolation Lineraire ####################

### sous fonction
a <- c(NA,2,NA,4,NA,1,NA,NA,NA,-5,NA,3,NA,NA)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))

a <- c(NA,NA,4,NA,NA,NA,NA)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))

a <- c(NA,NA,-4,5,NA,NA,NA)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))

a <- c(NA,NA,NA,NA,NA,NA)
a <- .LongData.InterpoLinBeginEnd(a)
try(a <- .LongData.InterpoLinMiddle(a))

a <- c(1,NA)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))

a <- c(NA,-1)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))

a <- c(1)
a <- .LongData.InterpoLinBeginEnd(a)
(a <- .LongData.InterpoLinMiddle(a))


### Fonction complete
a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,-5,NA,3,NA,NA)
a2 <- .LongData.InterpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
a2 <- .LongData.InterpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,5,NA,NA,NA)
a2 <- .LongData.InterpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,NA,NA,NA,NA)
try(.LongData.InterpoLin(a))

a <- c(1,NA)
a2 <- .LongData.InterpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,-1)
a2 <- .LongData.InterpoLin(a)
plot(a2,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
a2 <- .LongData.InterpoLin(a)




############################################################
######################## CopyBegin #########################

### Sous fonction
(a <- c(NA,-2,NA,4,NA,1,NA,NA,NA,5,NA,3,NA,NA))
(m <- rep(1,14))
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,-2,NA,4,NA,1,NA,NA,NA,5,NA,3,NA,NA))
(m <- (14:1)*2)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,2,NA,4,NA,1,NA,NA,NA,5,NA,-3,NA,NA))
(m <- (14:1)*2+10)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,NA,4,NA,NA,NA,NA))
(m <- 1:7)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,NA,-4,3,NA,NA,NA))
(m <- 1:7)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,NA,NA,NA,NA,NA))
(m <- 1:6)
try(a <- .LongData.copyBegin(a,m))
try(a <- .LongData.copyEnd(a,m))
try(a <- .LongData.copyMiddle(a,m))


(a <- c(1,NA))
(m <- 1:2)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,1))
(m <- 1:2)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(1))
(m <- 2)
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
(a <- .LongData.copyMiddle(a,m))

(a <- c(NA,NA,NA,4,3,6,NA,NA))
(m <- c(8, NA, 2,3,3,5,NA,4))
a <- .LongData.copyBegin(a,m)
a <- .LongData.copyEnd(a,m)
try(a <- .LongData.copyMiddle(a,m))


###Fonction complete
#par(mfrow=c(3,4))
a <- c(NA,-2,NA,4,NA,1,2,NA,NA,NA,5,NA,3,NA,NA)
(m <- rep(1,15))
a2 <- .LongData.copy(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,4,NA,NA,NA,NA)
(m <- 1:7)
a2 <- .LongData.copy(a,m)
plot(a2,type="o",ylim=c(1,10))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,NA,-4,3,NA,NA,NA)
(m <- 1:7)
a2 <- .LongData.copy(a,m)
plot(a2,type="o",ylim=c(-10,8))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

(a <- c(NA,NA,NA,NA,NA,NA))
(m <- 1:7)
try(.LongData.copy(a,m))

a <- c(1,NA)
(m <- 1:2)
a2 <- .LongData.copy(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,1)
(m <- 1:2)
a2 <- .LongData.copy(a,m)
plot(a2,type="o")
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(1)
(m <- 2)
.LongData.copy(a,m)

a <- c(NA,-2,NA,4,NA,1,2,NA,NA,NA,5,NA,3,NA,NA)
(m <- (15:1)*2)
a2 <- .LongData.copy(a,m)
plot(a2,type="o",ylim=c(-8,30))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")

a <- c(NA,2,NA,4,NA,1,2,NA,NA,NA,5,NA,-3,NA,NA)
(m <- (15:1)*2-10)
a2 <- .LongData.copy(a,m)
plot(a2,type="o",ylim=c(-8,20))
lines(m,lwd=2,col=3,type="o")
lines(a,lwd=3,col=2,type="o")


a <- c(NA,NA,NA,4,3,6,NA,NA)
(m <- c(8, NA, 2,3,3,5,NA,4))
try(.LongData.copy(a,m))


imputeLongData(ld2n,"LOCF")
imputeLongData(ld2n,"interpoLin")
imputeLongData(ld2n,method="copyMean",yPartition=p2a)


traj <- ld2n@traj
part <- p2a@clusters
imputeLongData(traj,"copyMean",part)
cat("### Fin test Impute ###\n")

Sys.time()->d;for(i in 1:100)imputeLongData(traj,"copyMean",part);Sys.time()->f;f-d
Sys.time()->d;for(i in 1:100)imputeLongData(ld2n,"copyMean",p2a);Sys.time()->f;f-d

# Pour la premiere ou la derniere, on copie la variation de la moyenne.
# Pour les manquantes du millieu, on fait une interpolation qui copie, a l'échelle pret, la forme de la moyenne.
# S'il n'y a que des manquantes, on retourne que des manquantes



### Calcule les ordonnées des points situés sur la droite qui relie line[FirstNA] a line[LastNA]

### Plus précisément : on calcule les variations de la moyenne par rapport à une ligne moyenne qui irait tout droit
###   On ajoute ces variations a la ligne reliant InfNA et SupNA
### Une autre méthode est possible : calculer les variations par rapport a meanLine[infNA]
###   puis les normaliser en divisant par (meanLine[SupNA]-meanLine[InfNA])
###   puis les adapter a line en multipliant par (line[SupNA]-line[InfNA])
###   et enfin ajouter a line[InfNA]

### Fonctions moyenne, ecart type et which.min résistante aux NA.
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++ Fin Test  imputation +++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


