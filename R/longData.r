cat("\n####################################################################
############################ Class LongData ############################
############################### Creation ###############################
####################################################################\n")

cat("### Definition ###\n")
.LongData.validity <- function(object){
    cat("**** validity LongData ****\n")
    if(length(object@id)==0&length(object@time)==0&length(object@varName)==0&length(object@traj)==0){
    }else{
        if(any(c(length(object@id)==0,length(object@time)==0,length(object@varName)==0,length(object@traj)==0))){
            stop("[LongData:validity]: at least one slot is empty")}else{}
        if(length(object@id)!=dim(object@traj)[1]){
            stop("[LongData:validity] : The number of id does not fit with the number of trajectories")}else{}
        if(length(object@time)!=dim(object@traj)[2]){
            stop("[LongData:validity] : The number of time does not fit with the length of trajectories")}else{}
        if(length(object@varName)!=1){
            stop("[LongData:validity] : There is not exactly one variable name.")}else{}
        if(any(is.na(object@time))){
            stop("[LongData:validity] : There is some unknow times")}else{}
        if(!identical(object@time,sort(object@time))){
            stop("[LongData:validity] : time is not in the right order")}else{}
        if(any(duplicated(object@time))){
            stop("[Partition:validity]: Some time are duplicate")}else{}
        if(any(is.na(object@id))){
            stop("[Partition:validity]: Some id are NA")}else{}
        if(any(duplicated(object@id))){
            stop("[Partition:validity]: Some id are duplicate")}else{}
        if(any(dimnames(object@traj)[[1]]!=object@id,dimnames(object@traj)[[2]]!=paste(object@varName,object@time,sep=""))){
            stop("[Partition:validity]: dimnames of traj is not correct")}else{}
        if(object@trajSizeMin<1){
            stop("[Partition:validity]: trajectories with only NA are not trajectories")}else{}
        if(object@trajSizeMin>length(object@time)){
            stop("[Partition:validity]: the trajectories size is suppose to be longer than the number of mesurement")}else{}
    }
}
cleanProg(.LongData.validity,,,0)
setClass(
    Class="LongData",
    representation=representation(
        id="character",
        time="numeric",
        varName="character",
        traj="array",
        trajSizeMin="numeric"
    ),
    prototype=prototype(
        id=character(),
        time=numeric(),
        varName=character(),
        traj=array(dim=c(0,0)),
        trajSizeMin=numeric()
    ),
    validity=.LongData.validity
)
rm(.LongData.validity)


cat("\n###################################################################
########################## Class LongData #########################
########################### Constructeur ##########################
###################################################################\n")

setGenericVerif("longData",function(traj,id,time,varName,trajSizeMin){standardGeneric("longData")})
setMethod("longData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,id,time,varName,trajSizeMin){new("LongData")})
setMethod("longData",signature=c("ANY","ANY","ANY","ANY","ANY"),
    function(traj,id,time,varName,trajSizeMin){
        dimnames(traj) <- list(id,paste(varName,time,sep=""))
        new("LongData",id=as.character(id),time=time,varName=varName,traj=traj,trajSizeMin=trajSizeMin)
    }
)
setMethod("longData",signature=c("ANY","ANY","ANY","missing","ANY"),
    function(traj,id,time,varName,trajSizeMin){
        dimnames(traj) <- list(id,paste("V",time,sep=""))
        new("LongData",id=as.character(id),time=time,varName="V",traj=traj,trajSizeMin=trajSizeMin)
    }
)
setMethod("longData",signature=c("ANY","ANY","ANY","ANY","missing"),
    function(traj,id,time,varName,trajSizeMin){
        dimnames(traj) <- list(id,paste(varName,time,sep=""))
        new("LongData",id=as.character(id),time=time,varName=varName,traj=traj,trajSizeMin=2)
    }
)
setMethod("longData",signature=c("ANY","ANY","ANY","missing","missing"),
    function(traj,id,time,varName,trajSizeMin){
        dimnames(traj) <- list(id,paste("V",time,sep=""))
        new("LongData",id=as.character(id),time=time,varName="V",traj=traj,trajSizeMin=2)
    }
)


cat("### Conversion d'un data.frame en longData ###\n")

as.longData <- function(data,...){UseMethod("as.longData")}

as.longData.data.frame <- function(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),
   varName=sub("[[:digit:]]*$", "",names(data)[timeCol[1]]),trajSizeMin=2,...
){
   traj <- as.matrix(data[,timeCol])
   dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
   return(longData(id=id,time=timeReal,varName=varName,traj=traj,trajSizeMin=trajSizeMin))
}
cleanProg(as.longData.data.frame,,,0)


as.longData.matrix <- function(data,id=1:nrow(data),time=1:ncol(data),varName="V",trajSizeMin=2,...){
   return(longData(id=id,time=time,varName=varName,traj=data,trajSizeMin=2))
}
cleanProg(as.longData.matrix,,,0)

as.longData.ArtificialLongData <- function(data,...){
   return(as(data,"LongData"))
}
cleanProg(as.longData.ArtificialLongData,,,0)



cat("\n####################################################################
########################### Class LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur : 'id' ###\n")
setGenericVerif("getId",function(object){standardGeneric("getId")})
setMethod("getId","LongData",function(object){return(object@id)})

cat("### Getteur : 'time' ###\n")
setGenericVerif("getTime",function(object){standardGeneric("getTime")})
setMethod("getTime","LongData",function(object){return(object@time)})

cat("### Getteur : 'varName' ###\n")
setGenericVerif("getVarName",function(object){standardGeneric("getVarName")})
setMethod("getVarName","LongData",function(object){return(object@varName)})

cat("### Getteur : 'traj' ###\n")
setGenericVerif("getTraj",function(object){standardGeneric("getTraj")})
setMethod("getTraj","LongData",function(object){return(object@traj)})

cat("### Getteur : 'trajSizeMin' ###\n")
setGenericVerif("getTrajSizeMin",function(object){standardGeneric("getTrajSizeMin")})
setMethod("getTrajSizeMin","LongData",function(object){return(object@trajSizeMin)})

cat("### Setteur : 'id' ###\n")
setGenericVerif("setId<-",function(object,value){standardGeneric("setId<-")})
.LongData.setId <- function(object,value){
    object@id<-as.character(value)
    dimnames(object@traj) <- list(object@id,paste(object@varName,object@time,sep=""))
    validObject(object)
    return(object)
}
cleanProg(.LongData.setId,,,0)
setReplaceMethod("setId","LongData",.LongData.setId)
rm(.LongData.setId)


cat("### Setteur : 'time' ###\n")
setGenericVerif("setTime<-",function(object,value){standardGeneric("setTime<-")})
.LongData.setTime <- function(object,value){
    object@time<-value
    dimnames(object@traj) <- list(object@id,paste(object@varName,object@time,sep=""))
    validObject(object)
    return(object)
}
cleanProg(.LongData.setTime)
setReplaceMethod("setTime","LongData",.LongData.setTime)
rm(.LongData.setTime)


cat("### Setteur : 'varName' ###\n")
setGenericVerif("setVarName<-",function(object,value){standardGeneric("setVarName<-")})
.LongData.setVarName <- function(object,value){
    object@varName<-value
    dimnames(object@traj) <- list(object@id,paste(object@varName,object@time,sep=""))
    validObject(object)
    return(object)
}
cleanProg(.LongData.setVarName)
setReplaceMethod("setVarName","LongData",.LongData.setVarName)
rm(.LongData.setVarName)


cat("### Setteur : 'traj' ###\n")
setGenericVerif("setTraj<-",function(object,value){standardGeneric("setTraj<-")})
.LongData.setTraj <- function(object,value){
    object@traj<-value
    dimnames(object@traj) <- list(object@id,paste(object@varName,object@time,sep=""))
    validObject(object)
    return(object)
}
cleanProg(.LongData.setTraj)
setReplaceMethod("setTraj","LongData",.LongData.setTraj)
rm(.LongData.setTraj)

cat("### Setteur : 'trajSizeMin' ###\n")
setGenericVerif("setTrajSizeMin<-",function(object,value){standardGeneric("setTrajSizeMin<-")})
.LongData.setTrajSizeMin <- function(object,value){
    object@trajSizeMin<-value
    validObject(object)
    return(object)
}
cleanProg(.LongData.setTrajSizeMin)
setReplaceMethod("setTrajSizeMin","LongData",.LongData.setTrajSizeMin)
rm(.LongData.setTrajSizeMin)



cat("\n###################################################################
########################## Class LongData #########################
############################# Affichage ###########################
###################################################################\n")

cat("### Method: 'show' pour LongData ###\n")
.LongData.show <- function(object){
    cat("\n****** show(LongData) *******")
    cat("\n* Class :",class(object))
    cat("\n* id          = ");.catShort(object@id)
    cat("\n* trajSizeMin =",object@trajSizeMin)
    cat("\n* time        = ");.catShort(object@time)
    cat("\n* varName     =",object@varName)
    cat("\n* traj (limited to 10x10)  :\n")
    if(length(object@id)!=0){
        if(ncol(object@traj)>10){
            trajToShow <- as.data.frame(object@traj[,1:10])
            trajToShow$more <- "..."
        }else{
            trajToShow <- as.data.frame(object@traj)
        }
        if(nrow(object@traj)>10){
            print(trajToShow[1:10,])
            cat("... ...\n")
        }else{
            print(trajToShow)
        }
    }else{cat("   <no trajectories>\n")}
    cat("*** End of  show(longData) ***\n")
    return(invisible(object))
}
cleanProg(.LongData.show)
setMethod("show","LongData",.LongData.show)
rm(.LongData.show)

cat("### Method: 'print' pour LongData ###\n")
.LongData.print <- function(x,...){
    cat("******* print(LongData) *******\n")
    cat("* id          = ",x@id,"\n")
    cat("* trajSizeMin = ",x@trajSizeMin,"\n")
    cat("* time        = ",x@time,"\n")
    cat("* varName     = ",x@varName,"\n")
    cat("* Traj        = \n")
    print(x@traj)
    cat("*** End of  print(LongData) ***\n")
    return(invisible(x))
}
cleanProg(.LongData.print)
setMethod("print","LongData",.LongData.print)
rm(.LongData.print)


#.LongData.length <- function(x){
#    cat("******* length(longData) *******\n* length(Id)       = ",length(x@id))
#    cat("\n* length(time)     = ",length(x@time))
#    cat("\n* length(varName) = ",length(x@varName))
#    cat("\n* length(traj)   = ",dim(x@traj)[1],"x",dim(x@traj)[2],"x")
#    cat("\n*** End of  length(longData) ***\n")
#    return(invisible(x))
#}
#cleanProg(.LongData.length)
#cat("### Method: 'length' pour LongData ###\n")
#setMethod("length","LongData",definition=.LongData.length)
#rm(.LongData.length)


#cat("### Method: 'summary' pour LongData ###\n")
#object <- ld2
#.LongData.summary <- function(object,...){
#    id <- object@id
#    names(id)<-"id"
#     print(summary(data.frame(id)))
 #    print(summary(data.frame(object@traj)))
#    summary(data.frame(id,object@traj))
#}
#cleanProg(.LongData.summary)
#setMethod("summary","LongData",.LongData.summary)
#rm(.LongData.summary)


cat("### Method: 'plot' pour LongData ###\n")
.LongData.plot <- function(x,y,mean=TRUE,...){
#    layout(1)
    trajNoNA <- x@traj[selectTrajNoNA(x),]
    matplot(x@time,t(trajNoNA),type="l",col=1,xaxt="n",ylab=x@varName,xlab="Time",pch=1,lty=1)
    axis(1,at=x@time)
    if(mean){
        lines(x@time,apply(trajNoNA,2,.meanNA),lwd=8,col=1,type="b")
        lines(x@time,apply(trajNoNA,2,.meanNA),lwd=4,col=2,type="b")
    }else{}
}
cleanProg(.LongData.plot,,,1) #.meanNA
setMethod("plot",signature="LongData",.LongData.plot)
rm(.LongData.plot)




cat("###################################################################
#################### Class LongData & Partition ###################
############################### plot ##############################
###################################################################\n")

setGenericVerif("selectTrajNoNA",function(object){standardGeneric("selectTrajNoNA")})
.LongData.selectTrajNoNA <- function(object){
    return(apply(object@traj,1,function(x)sum(!is.na(x))>=object@trajSizeMin))
}
setMethod("selectTrajNoNA","LongData",.LongData.selectTrajNoNA)
rm(.LongData.selectTrajNoNA)

#########
### setMethod("importClusterization","LongData",.Clusterization.import)
### fait dans clusterizLongData.r


.LongData.plotCurve <- function(x,y,color1,symbol="without",main){
    noNA <- selectTrajNoNA(x)
    trajNoNA <- x@traj[noNA,]
    partNoNA <- y@clusters[noNA]
    graphAxe <- c(min(trajNoNA,na.rm=TRUE),max(trajNoNA,na.rm=TRUE))

    if(symbol=="symbol"){pch <- 1:y@nbClusters}else{pch <- LETTERS[1:y@nbClusters]}

    if(color1!="no"){  # on imprime les lignes apres avoir défini les couleurs.
        par(xpd=TRUE)
        if(color1=="black"){
            matplot(x@time,t(trajNoNA),type="l",lty=1,col=1,xaxt="n",ylab="",xlab="",pch=1,main=main)
            axis(1,at=x@time)
        }else{
            colorId <- match(partNoNA,LETTERS[1:10])+1
            shuffle <- order(runif(nrow(trajNoNA)))
            par(xpd=TRUE)
            matplot(x@time,t(trajNoNA[shuffle,]),type="l",lty=1,col=colorId[shuffle],xaxt="n",ylab="",xlab="",pch=1,main=main)
            axis(1,at=x@time)
        }
    }else{               # on initie le graphe sans rien imprimer.
        plot(x@time,x@time,ylim=graphAxe,type="n",xaxt="n",ylab="",xlab="",pch=1,lty=1,main=main)
        axis(1,at=x@time)
    }

    percent <- paste(": ",formatC(table(partNoNA)/length(partNoNA)*100),"%",sep="")
    legend("top", legend=percent, pch=pch,inset=1.1,col=1,ncol=length(percent),bty="n",x.intersp=0.5)
}
cleanProg(.LongData.plotCurve,,,1) # LETTERS

.LongData.plotMean <- function(x,y,color2,symbol="without",size=1){
    if(color2!="no"){  # on imprime les lignes apres avoir défini les couleurs.
        noNA <- selectTrajNoNA(x)
        trajNoNA <- x@traj[noNA,]
        partNoNA <- y@clusters[noNA]
        if(symbol!="without"){
            type <- "b"
            if(symbol=="letters"){
                pch <- LETTERS[1:y@nbClusters]
#                type2 <- "c"
            }else{
                pch <- 1:y@nbClusters
 #               type2 <- "b"
            }
        }else{
            type <- "l"
            pch <- 1
        }
        trajMean <- aggregate(trajNoNA,list(partNoNA),.meanNA)[,-1]

        if(color2=="black"){
            matlines(x@time,t(trajMean),col=1,lwd=4,lty=1,type="l")
            if(symbol!="without"){
                matlines(x@time,t(trajMean),col="white",lwd=4,lty=1,type="p",pch=16,cex=size*2)
                matlines(x@time,t(trajMean),col=1,lwd=2,lty=1,type="p",pch=pch,cex=size)
            }else{}
        }else{
            matlines(x@time,t(trajMean),col=1,lwd=8,lty=1,type="l")
            colorId <- (1:y@nbClusters)+1
            matlines(x@time,t(trajMean),col=colorId,lwd=4,lty=1,type="c")
            if(symbol!="without"){
                matlines(x@time,t(trajMean),col="white",lwd=8,lty=1,type="p",pch=16,cex=size*2)
                matlines(x@time,t(trajMean),col=1,lwd=2,lty=1,type="p",pch=pch,cex=size)
            }else{}
        }
    }else{}
}
cleanProg(.LongData.plotMean,,,2) # .meanNA LETTERS


.LongData.plotSubGroups <- function(x,y,color3){
    noNA <- selectTrajNoNA(x)
    trajNoNA <- x@traj[noNA,]
    partNoNA <- y@clusters[noNA]
    graphAxe <- c(min(trajNoNA,na.rm=TRUE),max(trajNoNA,na.rm=TRUE))
    percent <- paste(formatC(table(partNoNA)/length(partNoNA)*100),"%",sep="")
    names(percent) <- LETTERS[1:y@nbClusters]
    if(color3=="black"){
        colorId <- rep(1,y@nbClusters)
    }else{
        colorId <- (1:y@nbClusters)+1
    }
    names(colorId) <- LETTERS[(1:y@nbClusters)]
    for(iCluster in LETTERS[1:y@nbClusters]){
        data <- t(trajNoNA[partNoNA==iCluster,,drop=FALSE])
        if(length(data)==0){
            matplot(x@time,x@time,ylim=graphAxe,lty=1,                      type="n",xaxt="n",ylab="",xlab=percent[[iCluster]],pch=1)
        }else{
            matplot(x@time,data  ,ylim=graphAxe,lty=1,col=colorId[iCluster],type="l",xaxt="n",ylab="",xlab=percent[[iCluster]],pch=1)
        }
        axis(1,at=x@time)
    }
}
cleanProg(.LongData.plotSubGroups,,,1) # LETTERS

### Definition of the layout. Principal draw is 1, the other are 2 to nbClusters. If there is more than 5 cluster, we use two lines
#x <- ld2;y <- p2b;color=c("color","color","color")
.LongData.plotClusters <- function(x,y,color=c("color","color","color"),main="",symbol="without",size=1){
    color <-  match.arg(color,choices=c("color","no","black"),TRUE)
    if(color[3]=="no"){
#        layout(1)
        .LongData.plotCurve(x,y,color[1],symbol,main)
        .LongData.plotMean(x,y,color[2],symbol,size)
    }else{
        if(y@nbClusters<=5){
            layout(rbind(1,1,1,2:(y@nbClusters+1)))
        }else{
            lay <- floor(y@nbClusters/2+0.5)
            layout(rbind(1,1,1,2:(lay+1),(lay+2):(2*lay+1)))
        }
        .LongData.plotCurve(x,y,color[1],symbol,main)
        .LongData.plotMean(x,y,color[2],symbol,size)
        .LongData.plotSubGroups(x,y,color[3])
    }
}
cleanProg(.LongData.plotClusters)
#x <- ld1;y<-p1a;color=c("c","b","c");main="Exemple"
setMethod("plot",signature=c(x="LongData",y="Partition"),def=.LongData.plotClusters)
rm(.LongData.plotClusters)

setGenericVerif("choicePlot",function(x,y,color="",main="",symbol="",size=""){standardGeneric("choicePlot")})
.longData.choicePlot <- function(x,y,color,main="",symbol="without",size=1){
    #.keybd <- function(key) {if(key%in%c("E","R","T","S","D","F","G","H","J","X","C","V","ctrl-J")){return(key)}else{}}
    if(missing(color)){color<-c("color","color","color")}else{}
    plot(x,y,color,main,symbol,size)
    getGraphicsEvent("Options:
  - e,r,t : trajectories (none, black, color)
  - d,f,g : mean trajectories (none, black, color)
  - c,v,b : subgroups (none, black, color)
  - h,j,k : symbols (none, letters, symbol)
  - o,l   : change size of the symbols
Return when finish",
        onKeybd = function(key){
            if(key!="ctrl-J"){
                if(key=="e"){color[1]<<-"no"}else{}
                if(key=="r"){color[1]<<-"black"}else{}
                if(key=="t"){color[1]<<-"color"}else{}
                if(key=="d"){color[2]<<-"no"}else{}
                if(key=="f"){color[2]<<-"black"}else{}
                if(key=="g"){color[2]<<-"color"}else{}
                if(key=="c"){color[3]<<-"no";layout(1)}else{}
                if(key=="v"){color[3]<<-"black"}else{}
                if(key=="b"){color[3]<<-"color"}else{}
                if(key=="h"){symbol  <<-"without"}else{}
                if(key=="j"){symbol  <<-"letters"}else{}
                if(key=="k"){symbol  <<-"symbol"}else{}
                if(key=="o"){size    <<- size+0.1;cat("size=",size,"\n")}else{}
                if(key=="l"){size    <<- size-0.1;cat("size=",size,"\n")}else{}
                plot(x,y,color,main,symbol,size)
                return(invisible())
            }else{1}
        }
    )
    bringToTop(-1)
}
cleanProg(.longData.choicePlot,,,)
setMethod("choicePlot","LongData",.longData.choicePlot)
rm(.longData.choicePlot)

