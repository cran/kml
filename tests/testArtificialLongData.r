source("../R/artificialLongData.r")

par(mfrow=c(2,3))
aldA1 <- generateArtificialLongData()
plot(aldA1,color=c("c","c","n"))

aldA5 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,5)})
plot(aldA5,color=c("c","c","n"))

aldA9 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,9)})
plot(aldA9,color=c("c","c","n"))

aldB3 <- generateArtificialLongData(name="CrossingLines",time=0:6,
    functionCluster=list(function(x){2},function(x){10},function(x){12-2*x}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldB3,color=c("c","c","n"))

aldC3 <- generateArtificialLongData(time=5:45,nbEachClusters=c(50,50,50,50),
    functionCluster=list(function(x){50*dnorm(x,20,2)},function(x){50*dnorm(x,25,2)},function(x){50*dnorm(x,30,2)},function(x){30*dnorm(x,25,5)}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldC3,color=c("c","c","n"))

aldD3 <- generateArtificialLongData(time=0:10,nbEachClusters=c(50,50,50,50),clusterNames=c("Constant","Rising","Falling","Curve"),
    functionCluster=list(function(x){0},function(x){x},function(x){10-x},function(x){-0.4*x^2+4*x}),
    functionNoise=function(t,sdSeq){rnorm(1,0,3)}
)
plot(aldD3,color=c("c","c","n"))

