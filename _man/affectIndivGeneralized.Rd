\name{affectIndivGeneralized}
\Rdversion{1.1}
\alias{affectIndivGeneralized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~ Function: affectIndivGeneralized ~
}
\description{
Given some longitudinal data (trajectories) and k clusters centers, \code{affectIndiv}
affecte each individual to the cluster whose center is the closest.
}
\usage{
affectIndivGeneralized(traj, clustersCenter, distance = function(x, y) {
    return(dist(t(cbind(x, y))))
})
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each colomn is a time mesurement.}
  \item{clustersCenter}{[matrix]: clusters center. Each line is a
    cluster center,     each colomn is a time mesurement.}

  \item{distance}{[function]: function use to estimate the distance between an
    individual and a clusters center. It can be use to deal with non
    classical distance.}
}
\details{
  EM algorithm (like k-means) altern two phases : Esperance and Maximisation. During
  Maximisation, each individual is affected to the closest cluster.
  This is what \code{affectIndivGeneralized} does. In addition to
    \code{\link{affectIndiv}}, it also let the user to define a
    non-classical distance.

  Note that \code{affectIndiv} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndivGenerized} used with
  \code{\link{calculCenterGeneralized}} does simulate one step of EM-algorithm.

  This function is programmed in R, it is not expect to be as fast as \code{affectIndiv}.
}
\value{
  Object of class\code{\linkS4class[LongitudinalData]{Partition}}.
}


\examples{
#######################
### affectIndiv

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),]

### Distance unusual
distCor <- function(x,y){return(cor(x,y))}

### Affectation of each individual
affectIndivGeneralized(traj,center,distance=distCor)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
