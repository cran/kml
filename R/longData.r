cat("\n####################################################################
############################ Class LongData ############################
############################### Creation ###############################
####################################################################\n")

cat("### Definition ###\n")
.LongData.validity <- function(object){
#    cat("**** validity LongData ****\n")
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
            stop("[LongData:validity]: Some time are duplicate")}else{}
        if(any(is.na(object@id))){
            stop("[LongData:validity]: Some id are NA")}else{}
        if(any(duplicated(object@id))){
            stop("[LongData:validity]: Some id are duplicate")}else{}
        if(any(dimnames(object@traj)[[1]]!=object@id,dimnames(object@traj)[[2]]!=paste(object@varName,object@time,sep=""))){
            stop("[LongData:validity]: dimnames of traj is not correct")}else{}
        if(object@trajSizeMin<1){
            stop("[LongData:validity]: trajectories with only NA are not trajectories")}else{}
        if(object@trajSizeMin>length(object@time)){
            stop("[LongData:validity]: the trajectories size is suppose to be longer than the number of mesurement")}else{}
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

setMethod("longData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,id,time,varName,trajSizeMin){new("LongData")}
)
setMethod("longData",signature=c("ANY","ANY","ANY","ANY","ANY"),
    function(traj,id,time,varName="V",trajSizeMin=1){
        dimnames(traj) <- list(id,paste(varName,time,sep=""))
        new("LongData",id=as.character(id),time=time,varName=varName,traj=traj,trajSizeMin=trajSizeMin)
    }
)

cat("### Conversion d'un data.frame en longData ###\n")

setMethod("as.longData",signature=c("data.frame"),
    function(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),
        varName=sub("[[:digit:]]*$", "",names(data)[timeCol[1]]),trajSizeMin=1,...
    ){
        traj <- as.matrix(data[,timeCol])
        dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
        return(longData(id=id,time=timeReal,varName=varName,traj=traj,trajSizeMin=trajSizeMin))
    }
)

#as.longData.ArtificialLongData <- function(data,...){
#   return(as(data,"LongData"))
#}
#cleanProg(as.longData.ArtificialLongData,,,0)



cat("\n####################################################################
########################### Class LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur ###\n")
setMethod("[","LongData",
    function(x,i,j,drop){
        switch(EXPR=i,
            "id"={return(x@id)},
            "varName"={return(x@varName)},
            "time"={return(x@time)},
            "trajSizeMin"={return(x@trajSizeMin)},
            "traj"={
                if(missing(j)){return(x@traj)}else{return(x@traj[j,])}
            },
            stop("[LongData:getteur]: there is not such a slot in LongData")
        )
    }
)

cat("### Setteur ###\n")
setMethod("[<-","LongData",
    function(x,i,j,value){
        switch(EXPR=i,
            "id"={x@id<-as.character(value)},
            "varName"={x@varName<-value},
            "time"={x@time<-value},
            "trajSizeMin"={x@trajSizeMin<-value},
            "traj"={
                if(missing(j)){x@traj<-value}else{x@traj[j,]<-value}
            },
            stop("[LongData:getteur]: this is not a LongData slot")
        )
        dimnames(x@traj) <- list(x@id,paste(x@varName,x@time,sep=""))
        validObject(x)
        return(x)
    }
)


cat("\n###################################################################
########################## Class LongData #########################
############################# Affichage ###########################
###################################################################\n")

cat("### Method: 'show' pour LongData ###\n")
.LongData.show <- function(object){
#    cat("\n****** show(LongData) *******")
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n~ id   : [",length(object@id),"] ",sep="");catShort(object@id)
    cat("\n~ time : [",length(object@time),"] ",sep="");catShort(object@time)
    cat("\n~ varName     :",object@varName)
    cat("\n~ trajSizeMin :",object@trajSizeMin,"\n")
    cat("\n~ traj : [",length(object@id),"x",length(object@time),"] (limited to 10x10) :\n",sep="")
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
#    cat("*** End of  show(longData) ***\n")
    return(invisible(object))
}
cleanProg(.LongData.show)
setMethod("show","LongData",.LongData.show)
rm(.LongData.show)


.LongData.selectSupTrajSizeMin <- function(object){
    return(apply(object@traj,1,function(x)sum(!is.na(x))>=object@trajSizeMin))
}
cleanProg(.LongData.selectSupTrajSizeMin)
setMethod("selectSupTrajSizeMin","LongData",.LongData.selectSupTrajSizeMin)
rm(.LongData.selectSupTrajSizeMin)

