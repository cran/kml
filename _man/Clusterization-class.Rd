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
  An object of class \code{Clusterization} is a \code{\link[longitudinalData:Partition-class]{Partition}} of
  trajectories in subgroups. The object also contains some information like the
  percentage of trajectories that each group contains or a quality criterion.\cr
}

\section{Objects from the Class}{
  Objects are not intend to be created by users. \code{Clusterization} are created
  by \code{kml} and directly added to a \code{\linkS4class{ClusterizLongData}} object.
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 1 and 52}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. The groups are in capital or small letters, from
      \code{A} to \code{LETTERSletters[nbClusters]}. Please note that a group might
      be empty.}
    \item{\code{percentEachCluster}}{[vector(numeric)]: percentage of
      trajectories contained in each group.}
    \item{\code{criterionName}}{[character]: Name of the quality
      criterion use to evaluate the quality of the clusterization (Calinski
      criterion is the only criterion available at this time)}
    \item{\code{criterionValue}}{[numeric]: Value of the quality
      criterion use to evaluate the quality of the clusterization.}
    \item{\code{imputationMethod}:}{[numeric] the calculation of quality
      criterion does not hold missing value. This variable save the name of
      the imputation method used. See \code{\link[longitudinalData]{imputation}.}}
    \item{\code{startingCondition}}{[character]: if the
      \code{Clusterization} has been obtain by running an algorithm, this
      variable save the way of defining the starting condition.}
    \item{\code{algorithmUsed}}{[character]: if the
      \code{Clusterization} has been obtain by running an algorithm, this
      variable save the name of the algorithm used.}
    \item{\code{convergenceTime}}{[numeric]:  if the
      \code{Clusterization} has been obtain by running an algorithm, this
      variable save the number of iteration
      necessary to converge.}
  }
}

\section{validation rules}{
  An object \code{Clusterization} must follow the same rules than a
  \code{\link[longitudinalData:Partition-class]{Partition}}.
}


\section{Construction}{
  Class \code{Clusterization} objects are constructed through the
  \code{\link{kml}} procedure and are directy add to a
  \code{ClusterizLongData} object. They can also be construct
  by the users using \code{\link{clusterization}}.
}


\section{Getteur [}{
  \describe{
    \item{Object["clusters"]}{[vector(factor)]: Gets the cluster of each individual (the value of the slot \code{clusters})}
    \item{Object["nbClusters"]}{[numeric]: Gets the number of clusters (the value of the slot \code{nbClusters})}
    \item{Object["percentEachClusters"]}{[numeric]: Gets the percent of individual present in each clusters.}
    \item{Object["convergenceTime"]}{[numeric]: Gets the convergence time of the Clusterization.}
    \item{Object["criterionName"]}{[character]: Gets the name of criterion used to estimate the Clusterization quality.}
    \item{Object["criterionValue"]}{[numeric]: Gets the value of criterion used to estimate the Clusterization quality.}
    \item{Object["imputationMethod"]}{[character]: Gets the name of the imputation method.}
    \item{Object["startingCondition"]}{[character]: Gets the name of starting condition.}
    \item{Object["algorithmUsed"]}{[character]: Gets the name of the algorithm use to find the clusterization.}
    \item{Object["myCriterion"]}{[numeric]: If "myCriterion" is the name of a quality criterion, gets its value.}
  }
}

\section{Setteur}{
  \code{Clusterization} are not intend to be modified, no setteur is define.
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
  Classes : \code{\linkS4class{ClusterizLongData}},
  \code{\linkS4class{Clusterization}} \cr
  Methods : \code{\link{kml}}
}


\examples{showClass("Clusterization")}

\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
