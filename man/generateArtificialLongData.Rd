\name{generateArtificialLongData}
\alias{gald}
\alias{generateArtificialLongData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ function: generateArtificialLongData ~}
\description{
  This function is used to builp up artificial data set of longitudinal data.
}
\usage{
gald(name = "", clusterNames = "", nbEachClusters = rep(50, 3),
    functionClusters = list(function(t){t}, function(t){0},function(t){-t}),
    functionNoise = function(t){rnorm(1, 0, 0.1)},
    time = 0:7, decimal = 2, percentOfMissing = 0)


generateArtificialLongData(name = "", clusterNames = "", nbEachClusters = rep(50, 3),
    functionClusters = list(function(t){t}, function(t){0},function(t){-t}),
    functionNoise = function(t){rnorm(1, 0, 0.1)},
    time = 0:7, decimal = 2, percentOfMissing = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{[character]: name of the data set.}
  \item{clusterNames}{[character]: name of each clusters.}
  \item{nbEachClusters}{[numeric]: number of trajectories that each
    cluster must contain. Note that the number of elements in
    \code{nbEachClusters} (the length of \code{nbEachClusters}) is used
    as a definition of the number of groups.}
  \item{time}{[numeric]: time at which measures are made.}
  \item{functionClusters}{[list(function)]: lists the functions
    generating the average trajectories of each cluster. If a single
    function is given, it is used for all groups. If several functions
    are given, the number of functions must correspond to the number of groups.}
  \item{functionNoise}{[list(function)]: list of functions
    generating the noise of each trajectory within its own cluster. If
    a single function is given, it is used for all groups. If several
    functions are given, the number of functions must correspond to the number of groups.}
  \item{decimal}{[numeric]: number of decimals used to round up values.}
  \item{percentOfMissing}{[numeric]: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is used for all groups. If several value are given, the number of values must correspond to the number of groups.}
}

\details{
  \code{generateArtificialLongData} (\code{gald} in short) is a
  function that enables the user to contruct artificial trajectories.
  Each individual is considered as belonging to a group. This group
  follows a certain theoretical trajectory, function of time. These functions (one per group) are given via the argument \code{functionClusters}.

  Within a group, the individual undergoes individal variations. Individual variations are given via the argument \code{functionNoise}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly striking the data thanks to \code{percentOfMissing}.
}

\value{
  An object of class \code{\linkS4class{ArtificialLongData}}.
}

\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Responsable : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}


\seealso{\link{kml-package},\code{link{ArtificialLongData}}}

\examples{
### Three diverging lines
ex1 <- generateArtificialLongData()
ex1
#plot(ex1)

### Three diverging lines with high variance
ex2 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,3)})
ex2
#plot(ex2)

### Three diverging lines with unbalance groups
ex3 <- generateArtificialLongData(nbEachClusters=c(120,10,40))
ex3
#plot(ex3)

### Three diverging lines with missing data
ex4 <- generateArtificialLongData(percentOfMissing=c(0.5,0,0.25))
ex4
#plot(ex4)

### Four strange functions
ex5 <- generateArtificialLongData(
    name="Four strange functions",
    clusterNames=c("Line","Poly2","Normal","Sinus"),
    nbEachClusters=rep(100,4),
    functionClusters=list(function(t){-10+2*t},function(t){-0.6*t^2+6*t-7.5},function(t){10*sin(t)},function(t){30*dnorm(t,2,1.5)}),
    functionNoise=function(t){rnorm(1,0,4)},
    time=0:10,decimal=2,percentOfMissing=0.3)
ex5
#plot(ex5)


### Here is a data set. Our objectif is to find some clusters...
#layout(1)
#plot(ex5,color=c("black","no","no"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series
