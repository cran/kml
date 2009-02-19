source("../R/artificialLongData.r")

par(mfrow=c(2,3))
aldB1 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,1)})
plot(aldB1,colorTraj="color",colorMean="both")

aldB3 <- generateArtificialLongData()
plot(aldB3,colorTraj="color",colorMean="both")

aldB7 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,7)})
plot(aldB7,colorTraj="color",colorMean="both")

#aldB9 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,9)})
#plot(aldB9,colorTraj="color",colorMean="both")

aldA3 <- generateArtificialLongData(time=0:7,nbEachClusters=c(50,50,50),clusterNames=c("Constant","Rising","Falling"),
    functionClusters=list(function(x){0},function(x){x},function(x){-x}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldA3,colorTraj="color",colorMean="both")

aldC3 <- generateArtificialLongData(name="CrossingLines",time=0:6,nbEachClusters=c(50,50,50),
    functionClusters=list(function(x){2},function(x){10},function(x){12-2*x}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldC3,colorTraj="color",colorMean="both")

aldD3 <- generateArtificialLongData(time=5:45,nbEachClusters=c(50,50,50,50),
    functionClusters=list(function(x){50*dnorm(x,20,2)},function(x){50*dnorm(x,25,2)},function(x){50*dnorm(x,30,2)},function(x){30*dnorm(x,25,5)}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldD3,colorTraj="color",colorMean="both")



