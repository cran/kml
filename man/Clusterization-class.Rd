\name{Clusterization-class}
\docType{class}
\alias{Clusterization}
\alias{Clusterization-class}
\alias{[,Clusterization-method}
\alias{[,Clusterization}
%\alias{exportClusterization,Clusterization-method}
%\alias{getCalinskiCriterion,Clusterization-method}
%\alias{getConvergenceTime,Clusterization-method}
%\alias{getDetWithin,Clusterization-method}
%\alias{getPercentEachCluster,Clusterization-method}
%\alias{getTraceBetween,Clusterization-method}
%\alias{getTraceWithin,Clusterization-method}
%\alias{getTrajSizeMin,Clusterization-method}
%\alias{getVarBetween,Clusterization-method}
%\alias{getVarWithin,Clusterization-method}
%\alias{initialize,Clusterization-method}
%\alias{print,Clusterization-method}
%\alias{show,Clusterization-method}

\title{~ Class: Clusterization ~}
\description{
  An object of class \code{Clusterization} is a partition of
  trajectories in subgroups.
  The object also contains some information like the
  percentage of trajectories that each group contains or the Calinski criterion.\cr
}

\section{Objects from the Class}{
  Objects are not intend to be created by users. \code{Clusterization} are created
  by \code{kml} and directly added to a \code{"\linkS4class{ClusterizLongData}"} object.
}

\section{Slots}{
  \describe{
    \item{\code{id}}{[vector(character)]: single identifier
      for each individual.}
    \item{\code{nbClusters}}{[numeric]: number of groups, between 2 and 25}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. The groups are in capital letters, from
      \code{A} to \code{LETTERS[nbClusters]}. Please note that a group might
      be empty. The groups are sort by size: (\code{A} is the
      biggest group, \code{B} is the next,...)}
    \item{\code{percentEachCluster}}{[vector(numeric)]: percentage of
      trajectories contained in each group.}
    \item{\code{calinski}}{[numeric]: Calinski criterion
      (formula : \code{trace(B)/trace(W)*(g-1)/(n-g)} with
      B=matrice of variance Between, W=matrice of variance Within, g=number of groups and n=number of individual.)}
    \item{\code{convergenceTime}:}{[numeric] number of iteration
      necessary to \code{kml} to find the \code{Clusterization}}
  }
}

\section{validation rules}{
  A class \code{Clusterization} object must follow some rules to be valid:
  \itemize{
    \item The slot should be either all empty, or all non empty.
    \item \code{nbClusters} has to be lower or equal to 25 (twenty five clusters maximum).
    \item \code{clusters} and \code{id} have to have the same length.
    \item \code{clusters} has to be a factor in \code{LETTERS[1:nbCluster]} (with
    \code{nbClusters} lower than 25).
    \item \code{table(clusters)} has to be in the reverse order
    (clusters are sorted form the biggest to the smallest).
    \item \code{id} cannot be duplicated.
    \item an \code{id} cannot be missing.
  }
}


\section{Construction}{
  Class \code{Clusterization} objects are constructed through the
  \code{\link{kml}} procedure and are directy add to a
  \code{ClusterizLongData} object. They are not intend to be construct
  by the users.
}


\section{Getteur [}{
  \describe{
    \item{Object["id"]}{[vecteur(character)]: Gets each individual
      indentifiant (the value of the slot \code{id})}
    \item{Object["nbClusters"]}{[numeric]: Gets the number of clusters (the value of the slot \code{nbClusters})}
    \item{Object["clusters"]}{[vector(factor)]: Gets the cluster of each individual (the value of the slot \code{clusters})}
    \item{Object["percentEachClusters"]}{[numeric]: Gets the percent of individual present in each clusters.}
    \item{Object["calinski"]}{[numeric]: Gets the calinski criterion of the Clusterization.}
    \item{Object["convergenceTime"]}{[numeric]: Gets the convergence time of the Clusterization.}
  }
}

\section{Setteur [<-}{
  \describe{
    \item{Object["id"]<-value}{[vecteur(character)]: Sets the individual
      indentifiant to \code{value}.}
    \item{Object["nbClusters"]<-value}{[numeric]: Sets the number of
      clusters to value.}
    \item{Object["clusters"]<-value}{[vector(factor)]: Sets the cluster of each individual.}
  }
  \code{"percentEachClusters"}, \code{"calinski"} and
  \code{"convergenceTime"} are calculate by the creation function
  according to the clusterization. They are not
  suppose to be change by te user.
}


\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}
\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}}, \code{\linkS4class{ArtificialLongData}} \cr
  Methods : \code{\link{kml}}
}


\examples{showClass("Clusterization")}

\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
