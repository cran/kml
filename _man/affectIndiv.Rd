\name{affectIndiv}
\Rdversion{1.1}
\alias{affectIndiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~ Function: affectIndiv ~
}
\description{
Given some longitudinal data (trajectories) and k clusters centers, \code{affectIndiv}
affecte each individual to the cluster whose center is the closest.
}
\usage{
affectIndiv(traj, clustersCenter, distance = "euclidean", power = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each colomn is a time mesurement.}
  \item{clustersCenter}{[matrix]: clusters center. Each line is a
    cluster center,     each colomn is a time mesurement.}

  \item{distance}{[character]: use to estimate the distance between an
    individual and a clusters center. Should be one off "manhattan","euclidean","minkowski","maximum","canberra" or "binary".}
  \item{power}{[numeric]: if the distance is "minkowski", \code{power}
    give the power to use.}
}
\details{
  EM algorithm (like k-means) altern two phases : Esperance and Maximisation. During
  Maximisation, each individual is affected to the closest cluster.
  This is what \code{affectIndiv} does.

  Note that \code{affectIndiv} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndiv} used with \code{\link{calculMean}} does simulate one k-means step.

  This function is programmed in C, it is expect to be fast.
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

### Affectation of each individual
affectIndiv(traj,center)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

