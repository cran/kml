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
\description{\code{ClusterizLongData} is an objet containing trajectories and associated clusterizations.
}

\section{Objects from the Class}{
  \code{\link[=kml]{kml}} is an algorithm that builds an set of \code{\linkS4class{Clusterization}} from longitudinal data. \code{ClusterizLongData}
  is the object contaning the original longitudinal data and all the \code{Clusterization} that \code{kml} finds.\cr

  When created, a class \code{ClusterizLongData} object simply contains initial data (the trajectories). After the execution of \code{kml}, it contains
  the original data and the \code{\linkS4class{Clusterization}} which has just been calculated by \code{kml}.

  Please note that if \code{kml} is executed several times, every new \code{Clusterization}
  are added to the original ones, no pre-existing \code{Clusterization} is erased.
}

\section{Slots}{
  \describe{
    \item{\code{id}}{\code{[character]}: single identifier
      for each of the trajectories, which is to say  each individual.}
    \item{\code{time}}{\code{[numeric]}: time during which measures were made.}
    \item{\code{traj}}{\code{[array(numeric)]}: contains longitudinal data. Each line corresponds to the trajectory of an individual. The columns refer to the time during which measures were made.}
    \item{\code{varName}}{\code{[character]}:
      Name of the variable measured.}
    \item{\code{trajSizeMin}}{\code{[numeric]}:
      Trajectories whose values are partially missing can either be
      excluded or included in the computation. \code{trajSizeMin} sets the
      minimum number of values that a trajectory must contain to not be
      excluded. For example, if the trajectories have 7 mesurements
      (\code{time=7}) and \code{trajSizeMin} is set to 3, the trajectory
      \code{(5,3,NA,4,NA,NA,NA)} will be include in the calculation while
      \code{(2,NA,NA,NA,4,NA,NA)} will be exclude. Please note that trajectories
      that are totally missing (i.e. 0 present values) are always excluded.}
    \item{\code{clusters}}{\code{[list(list(Clusterization))]:}
      \code{clusters} contains the list of \code{\linkS4class{Clusterization}} found by
      \code{kml}. More specifically, \code{clusterizList} contains
      twenty five items : c1, c2, c3,
      c4 up to c25. Each item contains a list of \code{Clusterization}: c2 contains the \code{Clusterization} having 2 clusters, c3
      contains the \code{Clusterization} having 3 clusters and so
      on. Note that c1
      is not used for regular \code{Clusterization} but for trajectories
      that are artificialy generated, it contain the "true" clusterization, .}
  }
}

\section{Construction}{
  Class \code{ClusterizLongData} objects can be constructed
  via function \code{\link{cld}} (build from scratch), via
  \code{\link[=generateArtificialLongData]{gald}} (generation of artificial
  longitudinal data) or via \code{\link[=as.clusterizLongData]{as.cld}}
  (turning a \code{data.frame} into a \code{ClusterizLongData}).
}

\section{Get [}{
  \describe{
    \item{Object["id"]}{[vecteur(character)]: Gets each individual
      indentifiant (the value of the slot \code{id})}
    \item{Object["time"]}{[vecteur(numeric)]: Gets the times (the value of the slot \code{time})}
    \item{Object["varName"]}{[character]: Gets the name of the variable (the value of the slot \code{varName})}
    \item{Object["trajSizeMin"]}{[numeric]: Gets the limit for
      not excluding a trajectory countaining missing values (the value of the slot \code{trajSizeMin})}
    \item{Object["traj"]}{[matrix(numeric)]: Gets all the trajectories'
      values (the value of the slot \code{traj})}
    \item{Object["traj",3]}{[matrix(numeric)]: Gets the trajectorie of
      the individual 3.}
    \item{Object["clusters"]}{[list(list(Clusterization))]: Gets the list
      off all the sublist of \code{Clusterization} contain in the object.}
    \item{Object["clusters",3]}{[list(Clusterization)]: Gets the sublist \code{"c3"},
      the sublist that contain all the \code{Clusterization} with 3 clusters.}
    \item{Object["clusters",c(3,2)]}{[Clusterization]: Get the second
      \code{Clusterization} off the sublist \code{"c3"}.}
    \item{Object["calinski"]}{[calinski, matrix(numeric)]: Gets the matrix of the
      Calinski criterion for all the \code{Clusterization} contain in
      the object. The class of \code{Object["calinski"]} is
      \code{c("calinski","matrix")}. The \code{calinski} class is used only for
      graphical output.}
    \item{Object["calinski",4]}{[vecteur(numeric)]: Gets the Calinski
      criterion for the \code{"c4"} sublist.}
    \item{Object["calinski",c(4,5)]}{[numeric] Gets the fifth Calinski
      criterion of the \code{"c4"} list.}
  }
}


\section{Set [<-}{
  \describe{
    \item{Object["id"]<-value}{[vecteur(character)]: Sets each individual
      indentifiant to \code{value}}
    \item{Object["time"]<-value}{[vecteur(numeric)]: Sets the times to
      \code{value}.}
    \item{Object["varName"]<-value}{[character]: Sets the name of the
      variable to \code{value})}
    \item{Object["trajSizeMin"]<-value}{[numeric]: Sets the limit for
      not excluding a trajectory with missing value to \code{value}.}
    \item{Object["traj"]}{[matrix(numeric)]: Sets all the trajectories'
      to \code{values}.}
    \item{Object["traj",3]}{[matrix(numeric)]: Sets the trajectorie of
      the individual 3 to \code{value}.}
    \item{Object["clusters","add"]<-value}{[Clusterization]: Adds the
      \code{Clusterization} to the corresponding sublist
      (the sublist that contain Clusterization with that same number of
      clusters that the one curently added), then sorts the sublist by
      decreasing Calinski criterion value (highest Calinski firsts).}
    \item{Object["clusters","clear"]<-value}{[numeric]: Empty the sublist
      "c-value", that is the sublist of \code{Clusterization} with
      \code{value} clusters.}
    \item{Object["clusters","clear"]<-"all"}{[numeric]: Empty all the
      sublist of \code{clusters}.}
    \item{Object["clusters"]<-value}{[Clusterization]: Short cut for \code{Object["clusters","add"]<-value}.}
  }
}



\section{Methods}{
  \describe{
    \item{kml}{\code{kml} is the algorithm that construct a new
      \code{Clusterization} and add it to the one already calculated.}
    \item{choice}{\code{choice} enables you to display all the \code{Clusterization}
      found, to choose some and export them.}
%    \item{["id"]}{\code{signature(.Object = "ClusterizLongData")}: ... }
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
  Classes : \code{\linkS4class{Clusterization}}, \code{\linkS4class{ArtificialLongData}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{kml}}, \code{\link{choice}}, \code{\link{as.clusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[=plotTraj]{plot(ClusterizLongData)}},
  \code{\link[=plotCalinski]{plot(Calinski)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}


\examples{
showClass("ClusterizLongData")
mat <- matrix(c(1,2,3,1,4,6,1,8,10),3)
ld <- new("ClusterizLongData",id=c("1","2","3"),time=c(2,4,8),varName="Age",traj=mat,trajSizeMin=2)
ld["id"]
ld["time"]<- c(1,3,9)
ld["varName"]
ld["traj"]
ld["traj",3]<-c(2,7,9)
(ld)
}
\keyword{classes}
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
