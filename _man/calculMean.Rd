\name{calculMean}
\Rdversion{1.1}
\alias{calculMean}
\title{
~ Function: calculMean ~
}
\description{
Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculMean} compute the mean trajectories of each clusters.
}
\usage{
calculMean(traj, xPart)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each colomn is a time mesurement.}
  \item{xPart}{[Partition]: affectation of each individual.}
}
\details{
EM algorithm (like k-means) altern two phases : Esperance and
Maximisation. During Esperance, the mean of each cluster is
evaluated. This is what \code{calculMean} does.

 Note that \code{calculMean} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndiv} used with \code{\link{calculMean}} does simulate one k-means step.

  This function is programmed in C, it is expect to be fast.
}

\value{
  A matrix with k line and t column containning k clusters centers. Each
  line is a center, each column is a time mesurement.
}

\examples{
#######################
### calculMean

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### A partition
part <- partition(floor(runif(200,1,5)),4)

### Clusters center
calculMean(traj,part)
}

