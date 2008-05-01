\name{ClusterizLongData-class}
\docType{class}
\alias{ClusterizLongData}
\alias{ClusterizLongData-class}
\alias{addClusterization,ClusterizLongData-method}
\alias{addPartition,ClusterizLongData-method}
%\alias{choice,ClusterizLongData-method}
\alias{getCalinskiCriterion,ClusterizLongData-method}
\alias{getClusterizList,ClusterizLongData,missing,missing-method}
\alias{getClusterizList,ClusterizLongData,numeric,missing-method}
\alias{getClusterizList,ClusterizLongData,numeric,numeric-method}
\alias{KmL,ClusterizLongData-method}
\alias{plot,ClusterizLongData,ANY-method}
\alias{print,ClusterizLongData-method}
\alias{show,ClusterizLongData-method}

\title{~ Class: ClusterizLongData ~}
\description{\code{ClusterizLongData} is an objet containing trajectories and associated clusterizations.
}

\section{Objects from the Class}{
  \code{\link[=kml]{KmL}} is an algorithm that builds an ensemble of \code{\linkS4class{Clusterization}} from longitudinal data. \code{ClusterizLongData}
  is the object contaning the original longitudinal data, then all the \code{Clusterization} that \code{KmL} finds.\cr

  When created, a class \code{ClusterizLongData} object simply contains initial data (the trajectories). After the execution of \code{KmL}, it contains
  the original data as well as the \code{\linkS4class{Clusterization}} which has just been calculated by the \code{KmL}.

  Please note that if \code{KmL} is executed several times, the new \code{Clusterization}
  are added to the original ones, no pre-existing \code{Clusterization} is erased.
}
\section{Slots}{
  \describe{
    \item{\code{id}}{\code{[character]} : single identifier
     for each of the trajectories, which is to say  each individual.}
    \item{\code{time}}{\code{[numeric]}: time during which measures were made.}
    \item{\code{traj}}{\code{[array(numeric)]}: contains longitudinal data. Each line corresponds to the trajectory of an individual. The columns refer to the time during which measures were made.
The third dimension enables you to considerer more variables (joint trajetories, actuelly under development)}
    \item{\code{varName}}{\code{[character]}:
      Name of the variable measured.}
    \item{\code{trajSizeMin}}{\code{[numeric]}:
      Trajectories whose values are partially missing can either be excluded by treatment, or included. \code{trajSizeMin} sets the
      minimum number of values that a trajectory must contain not to be excluded. Please note that trajectories
      that are totally missing (i.e. 0 present values) must always be excluded.}
    \item{\code{clusterizList}:}{\code{[list(list(Clusterization))]}
      \code{clusterzList} contains the list of \code{\linkS4class{Clusterization}} found by
      \code{KmL}. More specifically, \code{clusterizList} contains nine items : c2, c3,
      c4 up to c10. Each item contains a list of \code{Clusterization}: c2 contains the \code{Clusterization} having 2 clusters, c3
      contains the \code{Clusterization} having 3 clusters and so on.}
  }
}

\section{Construction}{
  Class \code{ClusterizLongData} objects can be constructed
  via function \code{\link{as.cld}}. This fonction takes into account \code{matrix} (see \link{as.cld.matrix}),
  \code{data.frame} (voir \link{as.cld.data.frame}) and   \code{\linkS4class{ArtificialLongData}} (see
  \link{as.cld.artificialLongData}).
}

\section{Methods}{
  \describe{
    \item{KmL}{\code{signature(.Object = "ClusterizLongData")}:
      \code{KmL} is the algorithm that enables you to construct the new
      \code{Clusterization} based on longitudinal data contained in \code{.Object}.}
    \item{choice}{\code{signature(x = "ClusterizLongData")}:
      \code{choice} enables you to display all \code{Clusterization}
      found, to choose the one that seems fittest and to export its graphic representation.}
%    \item{addClusterization}{\code{signature(.Object = "ClusterizLongData")}: ... }
%    \item{addPartition}{\code{signature(.Object = "ClusterizLongData")}: ... }
%    \item{getCalinskiCriterion}{\code{signature(object = "ClusterizLongData")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "missing", clusterNumber = "missing")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "numeric", clusterNumber = "missing")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "numeric", clusterNumber = "numeric")}: ... }
%    \item{plot}{\code{signature(x = "ClusterizLongData", y = "ANY")}: ... }
%    \item{print}{\code{signature(x = "ClusterizLongData")}: ... }
%    \item{show}{\code{signature(object = "ClusterizLongData")}: ... }
  }
}

\references{Article submited at ``Statistics in Medicines''}

\section{Author(s)}{
  Christophe M. Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Responsable : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}



\examples{
showClass("ClusterizLongData")
}
\keyword{classes}
