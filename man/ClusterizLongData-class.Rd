\name{ClusterizLongData-class}
\docType{class}
\alias{ClusterizLongData}
\alias{ClusterizLongData-class}
\alias{[,ClusterizLongData-method}
\alias{[<-,ClusterizLongData-method}
%\alias{addClusterization,ClusterizLongData-method}
%\alias{addPartition,ClusterizLongData-method}
%\alias{choice,ClusterizLongData-method}
%\alias{getCalinskiCriterion,ClusterizLongData-method}
%\alias{getClusterizList,ClusterizLongData,missing,missing-method}
%\alias{getClusterizList,ClusterizLongData,numeric,missing-method}
%\alias{getClusterizList,ClusterizLongData,numeric,numeric-method}
%\alias{KmL,ClusterizLongData-method}
%\alias{plot,ClusterizLongData,ANY-method}
%\alias{print,ClusterizLongData-method}
%\alias{show,ClusterizLongData-method}

\title{~ Class: ClusterizLongData ~}
\description{
  \code{ClusterizLongData} is an object containing trajectories and associated clusterizations.
}

\section{Objects from the Class}{
  \code{\link{kml}} is an algorithm that builds a set of \code{\linkS4class{Clusterization}}
  from longitudinal data. \code{ClusterizLongData}
  is the object containing the original longitudinal data and all the \code{Clusterization} that \code{kml} finds.\cr

  When created, an \code{ClusterizLongData} object simply contains initial data (the trajectories). After the execution of \code{kml}, it contains
  the original data and the \code{\linkS4class{Clusterization}} which has just been calculated by \code{kml}.

  Please note that if \code{kml} is executed several times, every new \code{Clusterization}
  is added to the original ones, no pre-existing \code{Clusterization} is erased.
}

\section{Slots}{
  \describe{
    \item{\code{id}}{\code{[character]}: single identifier
      for each of the trajectories (each individual).}
    \item{\code{time}}{\code{[numeric]}: time at which measures were made.}
    \item{\code{traj}}{\code{[array(numeric)]}: contains longitudinal data. Each line corresponds to the trajectory of an individual. The columns refer to the time during which measures were made.}
    \item{\code{varName}}{\code{[character]}: Name of the variable measured.}
    \item{\code{trajMinSize}}{\code{[numeric]}:
      Trajectories whose values are partially missing can either be
      excluded or included in the computation. \code{trajMinSize} sets the
      minimum number of values that a trajectory must contain to not be
      excluded. For example, if the trajectories have 7 measurements
      (\code{time=7}) and \code{trajMinSize} is set to 3, the trajectory
      \code{(5,3,NA,4,NA,NA,NA)} will be included in the calculation while
      \code{(2,NA,NA,NA,4,NA,NA)} will be excluded. Please note that trajectories
      that are totally missing (i.e. 0 present values) are always excluded.}
    \item{\code{clusters}}{\code{[list(list(Clusterization))]:}
      \code{clusters} contains the list of \code{\linkS4class{Clusterization}} found by
      \code{kml}. More specifically, \code{clusterizList} contains
      52 items : c1, c2, c3,
      c4 up to c52. Each item contains a list of \code{Clusterization}:
      c2 contains all the \code{Clusterization} having 2 clusters, c3
      contains all the \code{Clusterization} having 3 clusters and so
      on.}
    \item{\code{other}}{[list]: list of additional information (see section Value in
      \code{\link[longitudinalData]{generateArtificialLongData}} for an example).}
  }
}

\section{Construction}{
  Class \code{ClusterizLongData} objects can be constructed via function \code{\link{cld}} (build from scratch) and
  via \code{\link[=as.clusterizLongData]{as.cld}} (turning a \code{data.frame} or a \code{\link[longitudinalData:LongData-class]{LongData}}
  into a \code{ClusterizLongData}). Note that some artificial data can be generated using the combination of \code{as.cld} and
  \code{\link[longitudinalData]{generateArtificialLongData}}.
}

\section{Get [}{
  \describe{
    \item{Object["id"]}{[vecteur(character)]: Gets each individual
      Identifier the value of the slot \code{id})}
    \item{Object["time"]}{[vecteur(numeric)]: Gets the times (the value of the slot \code{time})}
    \item{Object["varName"]}{[character]: Gets the name of the variable (the value of the slot \code{varName})}
    \item{Object["trajMinSize"]}{[numeric]: Gets the limit for
      not excluding a trajectory containing missing values (the value of the slot \code{trajMinSize})}
    \item{Object["traj"]}{[matrix(numeric)]: Gets all the trajectories'
      values (the value of the slot \code{traj})}
    \item{Object["clusters"]}{[list(list(Clusterization))]: Gets the list
      of all the sublists of \code{Clusterization} contained in the object.}
    \item{Object["clusters",3]}{[list(Clusterization)]: Gets the sublists \code{"c3"},
      the sublist that contain all the \code{Clusterization} with 3 clusters.}
    \item{Object["clusters",c(3,2)]}{[Clusterization]: Get the second
      \code{Clusterization} of the sublist \code{"c3"}.}
    \item{Object["criterionValue"]}{[matrix(numeric)]: Gets the matrix of the
      criterion values for all the \code{Clusterization} contained in
      the object.}
    \item{Object["criterionValue",3]}{[vector(numeric)]: Gets the vector of the
      criterion values for all the \code{Clusterization} with 3 clusters (sublist \code{"c3"}).}
    \item{Object["criterionValue",c(3,2)]}{[numeric]: Gets criterion values for the second
      \code{Clusterization} of the sublist \code{"c3"}.}
    \item{Object["criterionName"],Object["criterionName",3] or Object["criterionName",c(3,2)]}{[matrix(numeric)]:
      Gets the criterion names for the \code{Clusterization} contained in
      the object (all, the third line or the second of the third line, as for "criterionValue").}
    \item{Object["myCriterion"]}{[matrix(numeric)]: if "myCriterion" is a criterion name, gets all this
      criterion value for all the \code{Clusterization}.}
    \item{Object["calinski",c(4,5)]}{[numeric] Gets the fifth Calinski
      criterion of the \code{"c4"} list.}
    \item{Object["other"]}{[list] Gets the list contain in the field "other".}
  }
}


\section{Set [<-}{
  \describe{
    \item{Object["id"]<-value}{[vecteur(character)]: Sets each individual
      Identifier to \code{value}}
    \item{Object["time"]<-value}{[vecteur(numeric)]: Sets the times to
      \code{value}.}
    \item{Object["varName"]<-value}{[character]: Sets the name of the
      variable to \code{value})}
    \item{Object["trajMinSize"]<-value}{[numeric]: Sets the limit for
      not excluding a trajectory with missing value to \code{value}.}
    \item{Object["traj"]}{[matrix(numeric)]: Sets all the trajectories'
      to \code{values}.}
    \item{Object["clusters","add"]<-value}{[Clusterization]: Adds the
      \code{Clusterization} to the corresponding sublist
      (the sublist that contain Clusterization with that same number of
      clusters that the one currently added), then sorts the sublist by
      decreasing quality criterion value (highest criterion firsts).}
    \item{Object["clusters","clear"]<-value}{[numeric]: Empties the sublist
      of \code{Clusterization} with \code{value} clusters.}
    \item{Object["clusters","clear"]<-"all"}{[numeric]: Empties all the
      sublist of \code{clusters}.}
    \item{Object["clusters"]<-value}{[Clusterization]: Short cut for \code{Object["clusters","add"]<-value}.}
  }
}



%\section{Methods}{
%  \describe{
%    \item{\code{\link{kml}}}{is the algorithm that constructs a new
%      \code{Clusterization} and add it to the one already calculated.}
%    \item{\code{\link{choice}}}{enables the user to display all the \code{Clusterization}
%      found, to choose some and export them.}
%    \item{\code{\link{plotCriterion}}}{display the quality criterion. Either the best one for each cluster number
%      or all.}
%    \item{["id"]}{\code{signature(.Object = "ClusterizLongData")}: ... }
%    \item{addPartition}{\code{signature(.Object = "ClusterizLongData")}: ... }
%    \item{getCalinskiCriterion}{\code{signature(object = "ClusterizLongData")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "missing", clusterNumber = "missing")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "numeric", clusterNumber = "missing")}: ... }
%    \item{getClusterizList}{\code{signature(object = "ClusterizLongData", nbClusters = "numeric", clusterNumber = "numeric")}: ... }
%    \item{plot}{\code{signature(x = "ClusterizLongData", y = "ANY")}: ... }
%    \item{print}{\code{signature(x = "ClusterizLongData")}: ... }
%    \item{show}{\code{signature(object = "ClusterizLongData")}: ... }
%  }
%}


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



\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{Clusterization}}, \code{\link[longitudinalData:LongData-class]{LongData}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{kml}}, \code{\link{choice}}, \code{\link{as.clusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotCriterion]{plotCriterion}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}






\examples{
################
### Creation of some trajectories
mat <- matrix(c(1,2,3,1,NA,6,1,8,NA),3)

################
### Creation of LongitudinalData
clustLd <- new("ClusterizLongData",id=c("1","2","3"),time=c(2,4,8),varName="Age",traj=mat,trajMinSize=2)

################
### get and set
clustLd["id"]
clustLd["time"]<- c(1,3,9)
clustLd["varName"]
clustLd["traj"]
clustLd["traj"][3,]<-c(2,7,9)

################
### Creation of a clusterization
part <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1)])
clus <- clusterization(xPartition=part,yLongData=clustLd)

################
### Adding a clusterization to a clusterizLongData
(clustLd["clusters","add"] <- clus)

################
### Removing all the clusterization from a clusterizLongData
clustLd["clusters","clear"] <- "all"
(clustLd)
}
\keyword{classes}
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
