\name{calculMean}
\Rdversion{1.1}
\alias{calculMean}
\title{
~ Function: calculMean ~
}
\description{
Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculMean} computes the mean trajectories of each cluster.
}
\usage{
calculMean(traj, xPart)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{xPart}{[Partition]: affectation of each individual.}
}
\details{
EM algorithm (like k-means) alternates between two phases : Esperance and
Maximisation. During Esperance, the mean of each cluster is
evaluated. This is what \code{calculMean} does.

 Note that \code{calculMean} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndiv} used with \code{\link{calculMean}} simulates one k-means step.

  This function is programmed in C, it is expected to be fast.
}

\value{
  A matrix with k line and t column containing k clusters centers. Each
  line is a center, each column is a time measurement.
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

