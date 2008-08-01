cat("###################################################################
#################### Class LongData & Partition ###################
############################### plot ##############################
###################################################################\n")

### Definition of the layout. Principal draw is 1, the other are 2 to nbClusters. If there is more than 5 cluster, we use two lines
#x <- ld2;y <- p2b;color=c("color","color","color")
.LongData.partition.plot <- function(x,y,colorTraj="black",colorMean="no",main="",point="no",size=1,subGroups=LETTERS[1:y@nbClusters],ylim=NA,...){
#    cat("plot Longdata-Partition\n")
    if(colorTraj=="no"&colorMean=="no"&point=="no"){  # Rien a tracer, on sort au plus vite
        plot(0,axes=FALSE,type="n",xlab="",ylab="",...)
        return(invisible())
    }else{
        if(missing(y)){y <- partition(clusters=rep("A",length(x["id"])),id=x["id"],nbClusters=1)}else{}
        noNA <- selectSupTrajSizeMin(x) # Selection des trajectoires a tracer.
        trajNoNA <- x@traj[noNA,]
        partNoNA <- y@clusters[noNA]

        # Percent est calculé avant l'exclusion des trajectoires non affichées
        percent <- paste(": ",formatC(table(factor(partNoNA))/length(partNoNA)*100),"%",sep="")[LETTERS[1:25]%in%subGroups]

        trajNoNA <- trajNoNA[partNoNA %in% subGroups,,drop=FALSE]
        partNoNA <- partNoNA[partNoNA %in% subGroups,drop=FALSE]

        if(length(partNoNA)==0){
            if(identical(ylim,NA)){ylim <- c(0,1)}else{}
            plot(x@time,x@time,ylim=ylim,type="n",xaxt="n",ylab="",xlab="",main=main,...)
            axis(1,at=x@time)
            return(invisible())
        }else{}

        # si besoin, calcul des trajectoires moyennes
        if(colorMean!="no"|point!="no"){trajMean <- aggregate(trajNoNA,list(partNoNA),meanNA)[,-1]}else{}

        # calcul des limites de l'axe des y, selon qu'on trace les traj ou non.
        if(identical(ylim,NA)){
            if(colorTraj!="no"){
                ylim <- c(min(trajNoNA,na.rm=TRUE),max(trajNoNA,na.rm=TRUE))
            }else{
                ylim <- c(min(trajMean,na.rm=TRUE),max(trajMean,na.rm=TRUE))
            }
        }else{}

        # initialisation du graphe
        par("mar"=c(4,2,1,1))
        plot(x@time,x@time,ylim=ylim,type="n",xaxt="n",ylab="",xlab="",main=main,...)
        axis(1,at=x@time)
        par(xpd=TRUE)
        if(point=="symbols"){pch <- match(subGroups,LETTERS)}else{pch <- subGroups}
#        legend("top", legend=percent, pch=pch,inset=1.1,col=1,ncol=length(percent),bty="n",x.intersp=0.5)
        legend(grconvertX(0.5,'nfc'), grconvertY(0,'nfc'),
               xjust=0.42, yjust=0,bty="n",
               legend = percent,pch=pch,
               horiz = TRUE, xpd = NA)

        par(xpd=FALSE)

        # Tracé des trajectoires
        if(colorTraj=="black"){  # on imprime les lignes apres avoir défini les couleurs.
            matlines(x@time,t(trajNoNA),type="l",lty=1,col=1,pch=1)
        }else{
            if(colorTraj=="color"){  # on imprime les lignes apres avoir défini les couleurs.
                colorId <- as.integer(partNoNA)+1
                shuffle <- order(runif(nrow(trajNoNA)))
                matlines(x@time,t(trajNoNA[shuffle,,drop=FALSE]),type="l",lty=1,col=colorId[shuffle],pch=1)
            }else{
                if(colorTraj=="no"){
                }else{
                    stop("[LongData:plot] : unknow value for colorTraj")
                }
            }
        }

        # Tracé des moyennes
        if(colorMean=="black"){
            matlines(x@time,t(trajMean),col=1,lwd=4,lty=1)
        }else{
            if(colorMean=="color"){
                colorMoy <- match(subGroups,LETTERS)+1
                matlines(x@time,t(trajMean),col=colorMoy,lwd=4,lty=1)
            }else{
                if(colorMean=="both"){
                    matlines(x@time,t(trajMean),col=1,lwd=8,lty=1)
                    colorMoy <- match(subGroups,LETTERS)+1
                    matlines(x@time,t(trajMean),col=colorMoy,lwd=4,lty=1)
                }else{
                    if(colorMean=="no"){
                    }else{
                        stop("[LongData:plot] : unknow value for colorMean")
                    }
                }
            }
        }

        # Tracé des Points
        if(point=="point"){
            matlines(x@time,t(trajMean),col=1,lwd=8,lty=1,type="p",pch=1,cex=size)
        }else{
            if(point=="letters"){
                matlines(x@time,t(trajMean),col="white",lwd=10,lty=1,type="p",pch=16,cex=size*2)
                matlines(x@time,t(trajMean),col=1,lwd=2,lty=1,type="p",pch=subGroups,cex=size)
            }else{
                if(point=="symbols"){
                    matlines(x@time,t(trajMean),col="white",lwd=10,lty=1,type="p",pch=16,cex=size*2)
                    matlines(x@time,t(trajMean),col=1,lwd=2,lty=1,type="p",pch=match(subGroups,LETTERS),cex=size)
                }else{
                    if(point=="no"){
                    }else{
                        stop("[LongData:plot] : unknow value for point")
                    }
                }
            }
        }

    }
    return(invisible())
}

cleanProg(.LongData.partition.plot,,,2) # LETTERS meanNA
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotTraj",signature=c(x="LongData"),def=.LongData.partition.plot)
setMethod("plot",signature=c(x="LongData"),def=.LongData.partition.plot)
rm(.LongData.partition.plot)

#par("mar"=c(4,2,2,2))
#symboles <- c(3,4,5,6)
#dn <- rbind(matrix(rnorm(20),5))
#layout(matrix(c(1,1,1,2,2,3),3))
#
#
#for(i in 1:3){
#    matplot(dn,type="b",xlab="",pch=symboles,ylab="")
#    legend(grconvertX(0.5,'nfc'), grconvertY(0,'nfc'),
#           xjust=0.42, yjust=0,bty="n",
#           pch = unique(symboles),
#           legend = c("a","b","c","d"),
#           horiz = TRUE, xpd = NA)
#}


.LongData.plotSubGroups <- function(x,y,colorTraj="color",colorMean="both",main="",point="no",size=1,ylim=NA,...){
#!attention, ca ne marhce pas avec cld2 !!!
#    cat("plotSubGroups LongData\n")
    if(missing(y)){y <- partition(clusters=rep("A",length(x["id"])),id=x["id"],nbClusters=1)}else{}
    noNA <- selectSupTrajSizeMin(x)
    trajNoNA <- x@traj[noNA,]
    partNoNA <- y@clusters[noNA]

    if(colorTraj!="no"){
        ylim <- c(min(trajNoNA,na.rm=TRUE),max(trajNoNA,na.rm=TRUE))
    }else{
        trajMean <- aggregate(trajNoNA,list(partNoNA),meanNA)[,-1]
        ylim <- c(min(trajMean,na.rm=TRUE),max(trajMean,na.rm=TRUE))
    }

    nbLignes <- ceiling(sqrt(y@nbClusters))
    nbCol <- ceiling(y@nbClusters/nbLignes)
    scr <- split.screen(c(nbLignes,nbCol))
    for(iCluster in 1:y@nbClusters){
        screen(scr[iCluster])
        plot(x,y,colorTraj=colorTraj,colorMean=colorMean,main=main,point=point,size=size,ylim=ylim,subGroup=LETTERS[iCluster])#,...)
    }
    close.screen(scr)
    return(invisible())
}
cleanProg(.LongData.plotSubGroups,,,2) # LETTERS meanNA
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plotSubGroups",signature=c(x="LongData"),def=.LongData.plotSubGroups)
rm(.LongData.plotSubGroups)















