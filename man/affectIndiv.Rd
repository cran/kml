\name{affectIndiv}
\Rdversion{1.1}
\alias{affectIndiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~ Function: affectIndiv ~
}
\description{
Given some longitudinal data (trajectories) and k clusters centres, \code{affectIndiv}
affect each individual to the cluster whose centre is the closest.
}
\usage{
affectIndiv(traj, clustersCenter, distance = "euclidean", power = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[matrix]: longitudinal data. Each line is an individual,
    each column is a time measurement.}
  \item{clustersCenter}{[matrix]: clusters centre. Each line is a
    cluster center,     each column is a time measurement.}

  \item{distance}{[character]: use to estimate the distance between an
    individual and a clusters centre. Should be one off "manhattan","euclidean","minkowski","maximum","canberra" or "binary".}
  \item{power}{[numeric]: if the distance is "minkowski", \code{power}
    give the power to use.}
}
\details{
  EM algorithm (like k-means) alternates between two phases : Esperance and Maximisation. During
  Maximisation, each individual is affected to the closest cluster.
  This is what \code{affectIndiv} does.

  Note that \code{affectIndiv} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndiv} used with \code{\link{calculMean}} simulates one k-means step.

  This function is programmed in C, it is expected to be fast.
}
\value{
  Object of class\code{\link[longitudinalData:Partition-class]{Partition}}.
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

