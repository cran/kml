\name{trajKmlSlow}
\Rdversion{1.1}
\alias{trajKmlSlow}

\title{
 ~ Function:trajKmlSlow ~
}
\description{
Ben
}
\usage{
trajKmlSlow(traj, clusterAffectation, nbId = nrow(traj), nbTime = ncol(traj), maxIt = 200, screenPlot = NA, distance = function(x, y) dist(rbind(x, y)), centerMethod = meanNA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{
%%     ~~Describe \code{traj} here~~
}
  \item{clusterAffectation}{
%%     ~~Describe \code{clusterAffectation} here~~
}
  \item{nbId}{
%%     ~~Describe \code{nbId} here~~
}
  \item{nbTime}{
%%     ~~Describe \code{nbTime} here~~
}
  \item{maxIt}{
%%     ~~Describe \code{maxIt} here~~
}
  \item{screenPlot}{
%%     ~~Describe \code{screenPlot} here~~
}
  \item{distance}{
%%     ~~Describe \code{distance} here~~
}
  \item{centerMethod}{
%%     ~~Describe \code{centerMethod} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~

  aze
}
\value{
  aze
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(traj,clusterAffectation,nbId=nrow(traj),nbTime=ncol(traj),maxIt=200,screenPlot=NA,
   distance=function(x,y)dist(rbind(x,y)),centerMethod=meanNA,...
){
#    if (distance \%in\% METHODS){distanceFun <- function(x,y){return(dist(t(cbind(x,y)),method=distance))}}else{distanceFun <- distance}
 #   print(distanceFun)
    exClusterAffectation <- partition()
    for(iterations in 1:maxIt){
        clustersCenter <- calculCenterGeneralized(traj=traj,xPart=clusterAffectation,centerMethod=centerMethod)
        clusterAffectation <- affectIndivGeneralized(traj=traj,clustersCenter=clustersCenter,distance=distance)
        if(identical(clusterAffectation,exClusterAffectation)){
            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
        }else{
            exClusterAffectation <- clusterAffectation
        }
        if(!is.na(screenPlot)){
            ld <- longData(traj=traj,id=1:nrow(traj),time=1:ncol(traj))
            screen(screenPlot)
            plot(ld,clusterAffectation,col="clusters",col.mean="clusters",main=paste("Iteration =",iterations),...)
        }else{}
    }
    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
