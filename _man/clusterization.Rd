\name{clusterization}
\Rdversion{1.1}
\alias{clusterization}

\title{
~ Function: clusterization ~
}
\description{
  \code{clusterization} is the constructor of the class \code{\linkS4class{Clusterization}}.
}
\usage{
clusterization(xPartition, yLongData, convergenceTime = 0, criterionName = "calinski", criterionValue = numeric(), imputationMethod = "", startingCondition = "", algorithmUsed = "")
}
\arguments{
  \item{xPartition}{[Partition]: object that will be turn into a \code{Clusterization}}
  \item{yLongData}{[LongData]: longitudinal data on which the
    clusterization has been run.}
  \item{convergenceTime}{[numeric]: if the clusterization has been
    obtain through an algorithm, number of steps of this algorithm
    before convergence.}
  \item{criterionName}{[character]: criterion use to evaluate the
    quality of the partitionnig.}
  \item{criterionValue}{[numeric]: value of the criterion use to
    evaluate the quality of the partitionning.}
  \item{imputationMethod}{[character]: name off the methods used to
    impute missing values. See \code{\link[longitudinalData]{imputation}}.}
  \item{startingCondition}{[character]: name off the methods used to
    define starting condition. See \code{\link{partitionInitialise}}.}
  \item{algorithmUsed}{[character]: algormith use to obtain the
    partition. Only k-means is available at this time.}
}
\details{
  In \code{KmL}, strickly speaking, a \code{\link[longitudinalData:Partition-class]{Partition}}  is just a sequence of
  letters (independant of any trajectories) ; a \code{Clusterization} is
  a \code{Partition} associate with a set of trajectories. So a
  \code{Clusterization}is a \code{Partition} with some additionnal
  information like some quality criterion, the name and convergence time
  off the algorithm use to clusterize the population...
}
\value{
  Object of class \code{\linkS4class{Clusterization}}.
}
\examples{
### Creation of a partition
part <- partition(rep(c(1,2),4),2)

### Some trajectories
traj1 <- gald(nbEachClusters=2)

### Tranformation of part into a Clusterization
clusterization(part,traj1)
# Calinski criterion is arround 0.50...



### Some other trajectories
traj2 <- gald(nbEachClusters=4, functionClusters=list(function(t){5-t},function(t){5+t}))

### Tranformation of part into a Clusterization
clusterization(part,traj2)
# Calinski criterion is arround 0.15...

# part is probably a good partition for traj1, but not for traj2...
}
