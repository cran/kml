\name{Clusterization-class}
\docType{class}
\alias{Clusterization}
\alias{Clusterization-class}
\alias{exportClusterization,Clusterization-method}
\alias{getCalinskiCriterion,Clusterization-method}
\alias{getConvergenceTime,Clusterization-method}
\alias{getDetWithin,Clusterization-method}
\alias{getPercentEachCluster,Clusterization-method}
\alias{getTraceBetween,Clusterization-method}
\alias{getTraceWithin,Clusterization-method}
\alias{getTrajSizeMin,Clusterization-method}
\alias{getVarBetween,Clusterization-method}
\alias{getVarWithin,Clusterization-method}
\alias{initialize,Clusterization-method}
\alias{print,Clusterization-method}
\alias{show,Clusterization-method}

\title{~ Class: Clusterization ~}
\description{
  An object of class \code{Clusterization} is a partition of
  trajectories in subgroups.
  The object also contains a certain number of information like the
  percentage of trajectories that each group contains or the Calinski criterion.\cr
}

\section{Objects from the Class}{
  Objects are not intend to be created by users. \code{Clusterization} are created
  by \code{kml} and directly added to a \code{"\linkS4class{ClusterizLongData}"} object.
}

\section{Slots}{
  \describe{
    \item{\code{id}}{[character]: single identifier
      for each individual.}
    \item{\code{nbClusters}}{[numeric]: number of groups}.
    \item{\code{clusters}}{[factor]: vector containing the affectation
      groups of each individual. The groups are in capital letters, from
      \code{A} to \code{LETTERS[nbClusters]}. Please note that a group might
      be empty. The groups are sort in reverse order (\code{A} is the
      biggest group, \code{B}is the next,...}
    \item{\code{percentEachCluster}}{[numeric]: percentage of
      trajectories contained in each group.}
    \item{\code{varBetween}}{[matrix]: Matrix of variance between}
    \item{\code{traceBetween}}{[numeric]: Trace of the matrix variance between}
    \item{\code{varWithin}}{[matrix]: Matrix of variance within}
    \item{\code{traceWithin}}{[numeric]: Trace of the matrix of variance within}
    \item{\code{detWithin}}{[numeric]: Determinant of the matrix of
      variance within}
    \item{\code{calinskiCriterion}}{[numeric]: Calinski criterion
      (formula : \code{traceBetween/traceWithin*(g-1)/(n-g)} with
      g=number of groups and n=number of individual.)}
    \item{\code{convergenceTime}:}{[numeric] number of iteration
      necessary to \code{kml} to find the \code{Clusterization}}
  }
}



\section{validation rules}{
  A class \code{Clusterization} object must follow some rules to be valid:
  \itemize{
    \item All the slot should be either empty, or non empty.
    \item \code{nbClusters} has to be lower or equal to 10 (ten clusters maximum).
    \item \code{clusters} and \code{id} have to have the same length.
    \item \code{clusters} has to be a factor in \code{LETTERS[1:nbCluster]} (with
    \code{nbClusters} lower than 10).
    \item \code{table(clusters)} has to be in the reverse order
    (clusters are sorted form the biggest to the smallest).
    \item \code{id} cannot be duplicated.
    \item an \code{id} cannot be missing.
  }
}

\section{Details}{
  The partitioning of an ensemble of trajectories is given by the couple
  \code{(id,clusters)}.
  \code{id} must correspond to \code{id} of an object of class \code{ClusterizLongData} ; the vector \code{clusters}
  then sets the group that each individual belongs to.
}

\seealso{\link{kml-package}}

\examples{showClass("Clusterization")}

\keyword{classes}
