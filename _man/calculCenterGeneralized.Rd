\name{calculMean}
\Rdversion{1.1}
\alias{calculCenterGeneralized}
\title{
~ Function: calculCenterGeneralized ~
}
\description{
Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculCenterGeneralized} compute the center trajectories of each clusters.
}
\usage{
calculCenterGeneralized(traj, xPart, centerMethod = meanNA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each colomn is a time mesurement.}
  \item{xPart}{[Partition]: affectation of each individual.}
  \item{centerMethod}{[function]: function use to calculate the center
    of each cluster.}
}
\details{
EM algorithm (like k-means) altern two phases : Esperance and
Maximisation. During Esperance, the center of each cluster is
evaluated. This is what \code{calculCenterGeneralized} does.

 Note that \code{calculCenterGeneralized} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndivGenerized} used with
  \code{\link{calculCenterGeneralized}} does simulate one step of EM-algorithm.

  This function is programmed in R, it is not expect to be as fast as \code{calculMean}.
}

\value{
  A matrix with k line and t column containning k clusters centers. Each
  line is a center, each column is a time mesurement.
}


\examples{
#######################
### calculCenterGeneralized

### Some LongitudinalData
traj <- as.cld(gald())["traj"]

### A partition
part <- partition(floor(runif(200,1,5)),4)

### Clusters center
calculCenterGeneralized(traj,part,medianNA)
}

