\name{partitionInitialise}
\Rdversion{1.1}
\alias{partitionInitialise}
\alias{partitionInitialise,numeric,numeric}
\alias{partitionInitialise,numeric,numeric-method}

\title{
~ Function: partitionInitialise ~
}
\description{
  This function provide different way of setting the initial partition
  for an EM algoritm.
}
\usage{
partitionInitialise(nbClusters, lengthPart, method = "randomK", matrixDist)
}
\arguments{
  \item{nbClusters}{[numeric]: number of clusters of that the initial
    partition should have.}
  \item{lengthPart}{[numeric]: number of individual in the partition.}
  \item{method}{[character]: one off "randomAll", "randomK" and "maxDist".}
  \item{matrixDist}{[matrix]: if the method "maxDist" is used, the
    fonction need to know the matrix of the distance between each individual.}
}

\details{
  Before alterning the phase Esperance and Maximisation, the EM
  algorithm need to initialise a starting configuration. This initial
  partition has been prooved to have an important impact on the
  final result and the convergence time.

  This function provide different way of setting the initial partition.
  \itemize{
    \item randomAll: all the individual are randomly assign to a
    cluster with at least one individual in each clusters.
    \item randomK: K individuals are randomly assign to a
    cluster, all the other are not assign (each cluster has only one
    individual).
    \item maxDist: K indivuals are choose. The two formers are the
    individual separated by the highest distance. The latters are added one
    by one, they are the "farest" individual of those that are allready
    selected. "farest" is the individual with the highest distance (min)
    to the selected individuals (if "t" are the individual already selected, the
    next selected individual is "i" such that max_i(min_t(dist(IND_i,IND_t))) )
  }
}
\value{
Object of class \code{\linkS4class{Partition}}.
}
\examples{
par(ask=TRUE)
###################
### Constrution of some longitudinal data
dn <- as.cld(gald())
plot(dn,type.mean="n",col=1)

###################
### partition using randamAll
pa1a <- partitionInitialise(3,lengthPart=200,method="randomAll")
plot(dn,pa1a)
pa1b <- partitionInitialise(3,lengthPart=200,method="randomAll")
plot(dn,pa1b)

###################
### partition using randamAll
pa2a <- partitionInitialise(3,lengthPart=200,method="randomK")
plot(dn,pa2a)
pa2b <- partitionInitialise(3,lengthPart=200,method="randomK")
plot(dn,pa2b)

###################
### partition using maxDist
pa3 <- partitionInitialise(3,lengthPart=200,method="maxDist",
    matrixDist=as.matrix(dist(dn["traj"])))
plot(dn,pa3)
### maxDist is deterministic, so no need for a second example

###################
### Example to illustrate "maxDist" method on classical clusters
point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))
plot(points,main="Some points")

paInit <- partitionInitialise(2,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points,main="Two farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points,main="Three farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

paInit <- partitionInitialise(4,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points, main="Four farest points")
lines(points[!is.na(paInit["clusters"]),],col=2,type="p",lwd=3)

par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
