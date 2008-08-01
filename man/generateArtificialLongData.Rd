\name{generateArtificialLongData}
\alias{gald}
\alias{generateArtificialLongData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ Function: generateArtificialLongData (or gald) ~}
\description{
  This function is used to builp up artificial data set of longitudinal data.
}
\usage{
gald(name="",clusterNames="",nbEachClusters=50,
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    functionNoise=function(t,sdSeq){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0)


generateArtificialLongData(name="",clusterNames="",nbEachClusters=50,
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    functionNoise=function(t,sdSeq){rnorm(1,0,3)},
    time=0:10,decimal=2,percentOfMissing=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{[character]: name of the data set.}
  \item{clusterNames}{[vector(character)]: name of each clusters.}
  \item{nbEachClusters}{[vector(numeric)]: number of trajectories that each
    cluster must contain. If a single
    number is given, it is duplicated for all groups (see detail).}
  \item{functionClusters}{[list(function)]: lists the functions
    defining the average trajectories of each cluster. If a single
    function is given, it is duplicated for all groups (see detail).}
  \item{functionNoise}{[list(function)]: lists the functions
    generating the noise of each trajectory within its own cluster. If
    a single function is given, it is duplicated for all groups (see detail).}
  \item{time}{[vector(numeric)]: time at which measures are made.}
  \item{decimal}{[numeric]: number of decimals used to round up values.}
  \item{percentOfMissing}{[numeric]: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups (see detail).}
}

\details{
  \code{generateArtificialLongData} (\code{gald} in short) is a
  function that enables the user to contruct artificial trajectories.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time. These functions (one per group) are given via the argument \code{functionClusters}.

  Within a group, the individual undergoes individal variations. Individual variations are given via the argument \code{functionNoise}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly striking the
  data thanks to \code{percentOfMissing}.

  Note that the number of cluster is define as the biggest length of
  variables \code{nbEachClusters}, \code{functionCluters},
  \code{functionNoise} and \code{percentOfMissing}. So at least one of
  these four variables should be define for each clusters.
}

\value{
  An object of class \code{\linkS4class{ArtificialLongData}}.
}

\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}

\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author: <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes: \code{\linkS4class{ArtificialLongData}} \cr
  Methods: \code{\link{kml}}, \code{\link{choice}}\cr
  Plot: \code{\link[=plotTraj]{plot(ClusterizLongData)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}


\examples{
par(ask=TRUE)

### Default example
ex1 <- generateArtificialLongData()
ex1
plot(ex1,colorTraj="black",colorMean="no")
plot(ex1)

### Three diverging lines
ex2 <- generateArtificialLongData(functionClusters=list(function(t)0,function(t)-t,function(t)t))
ex2
plot(ex2,colorTraj="black",colorMean="no")
plot(ex2)

### Three diverging lines with high variance, unbalance groups and missing value
ex3 <- generateArtificialLongData(
   functionClusters=list(function(t)0,function(t)-t,function(t)t),
   nbEachClusters=c(40,20,10),
   functionNoise=function(t){rnorm(1,0,3)},
   percentOfMissing=c(0.25,0.5,0.25)
)
ex3
plot(ex3,colorTraj="black",colorMean="no")
plot(ex3)

### Four strange functions
ex4 <- generateArtificialLongData(
    name="Four strange functions",
    clusterNames=c("Line","Poly2","Normal","Sinus"),
    nbEachClusters=c(100,300,200,100),
    functionClusters=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    functionNoise=function(t){rnorm(1,0,5)},
    time=0:10,decimal=2,percentOfMissing=0.3)
ex4
plot(ex4,colorTraj="black",colorMean="no")
plot(ex4)

### To get only trajectories (if you want some artificial trajectories
###    to deal with another algorithm), use the getteur ["traj"]
ex5 <- gald(nbEachCluster=3,time=1:3)
ex5["traj"]

par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series
