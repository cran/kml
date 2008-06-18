\name{ArtificialLongData-class}
\docType{class}
\alias{ArtificialLongData-class}
\alias{ArtificialLongData}
\alias{plot,ArtificialLongData,ANY-method}

\title{~ Class: ArtificialLongData~}
\description{\code{ArtificialLongData} is a class design to build up
  artificial data set in order to explore clusterization's algorithm
  properties.
}
\section{Construction}{Objects can be created by calling \code{\link{generateArtificialLongData}}}

\section{Slots}{
	 \describe{
    \item{\code{name}:}{[character] name of the data set.}
    \item{\code{nbClusters}:}{[numeric] number of clusters}
    \item{\code{clusterNames}:}{[character] names of the clusters.}
    \item{\code{nbEachClusters}:}{[numeric] number of trajectories that
	 compose each cluster.}
    \item{\code{time}:}{[numeric] time at which the measures are made.}
    \item{\code{functionClusters}:}{[list(function)] function list used to generate the average trajectory of each cluster.}
    \item{\code{functionNoise}:}{[list(function)] function list used to generate the noise of each trajectory within its own cluster.}
    \item{\code{trajMeanTheo}:}{[matrix] matrix containing the theoretical trajectories of each cluster, those that individuals would follow if they were not subject to individual variations.}
    \item{\code{trajMeanReal}:}{[matrix] matrix containing the average trajectories of each cluster, average trajectories that the individuals pursue indeed}
    \item{\code{id}:}{[character] single identifier for each individual.}
    \item{\code{varName}:}{[character] name of the variable which is being measured.}
    \item{\code{traj}:}{[array(numeric)] matrix of the trajectories of
      individuals. Each lign refers to an \code{id}, each column refers to a \code{time}.}
    \item{\code{percentOfMissing}:}{[numeric] percentage (between 0 and 1)
	of missing data generated in each cluster.}
  }
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "ArtificialLongData", y = "ANY")}:  }
  }
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

\seealso{\link{kml-package}, \code{\link{generateArtificialLongData}}}

\examples{
showClass("ArtificialLongData")
}

\keyword{classes}

