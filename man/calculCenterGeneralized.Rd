\name{calculCenterGeneralized}
\Rdversion{1.1}
\alias{calculCenterGeneralized}
\title{
~ Function: calculCenterGeneralized ~
}
\description{
Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculCenterGeneralized} computes the center trajectories of each cluster.
}
\usage{
calculCenterGeneralized(traj, xPart, centerMethod = meanNA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{xPart}{[Partition]: affectation of each individual.}
  \item{centerMethod}{[function]: function used to calculate the center
    of each cluster.}
}
\details{
EM algorithm (like k-means) altern two phases : Esperance and
Maximisation. During Esperance, the center of each cluster is
evaluated. This is what \code{calculCenterGeneralized} does.

 Note that \code{calculCenterGeneralized} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndivGenerized} used with
  \code{\link{calculCenterGeneralized}} simulates one step of EM-algorithm.

  This function is programmed in R, it is not expected to be as fast as \code{calculMean}.
}




\references{Article "KmL: K-means for Longitudinal Data", in
  Computational Statistics (accepted on 11-11-2009) \cr
  Web site: \url{http://christophe.genolini.free.fr/kml}
}
\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : \email{genolini@u-paris10.fr}
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}







\value{
  A matrix with k line and t column containing k clusters centers. Each
  line is a center, each column is a time measurement.
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

