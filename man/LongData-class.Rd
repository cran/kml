\name{LongData-class}
\docType{class}
\alias{LongData-class}
\alias{choicePlot,LongData-method}
\alias{getId,LongData-method}
\alias{getTime,LongData-method}
\alias{getTraj,LongData-method}
\alias{getTrajSizeMin,LongData-method}
\alias{getVarName,LongData-method}
\alias{importClusterization,LongData-method}
\alias{imputeLongData,LongData,ANY,missing-method}
\alias{imputeLongData,LongData,ANY,Partition-method}
\alias{plot,LongData,ANY-method}
\alias{plot,LongData,Partition-method}
\alias{print,LongData-method}
\alias{selectTrajNoNA,LongData-method}
\alias{setId<-,LongData-method}
\alias{setTime<-,LongData-method}
\alias{setTraj<-,LongData-method}
\alias{setTrajSizeMin<-,LongData-method}
\alias{setVarName<-,LongData-method}
\alias{show,LongData-method}

\title{~ Class: LongData ~}
\description{For internal uses only. See \code{\linkS4class{ClusterizLongData}} for details.}

\section{Slots}{
  \describe{
    \item{\code{id}:}{See \code{\linkS4class{ClusterizLongData}} for details.}
    \item{\code{time}:}{See \code{\linkS4class{ClusterizLongData}} for details.}
    \item{\code{varName}:}{See \code{\linkS4class{ClusterizLongData}} for details.}
    \item{\code{traj}:}{See \code{\linkS4class{ClusterizLongData}} for details.}
    \item{\code{trajSizeMin}:}{See \code{\linkS4class{ClusterizLongData}} for details.}
  }
}
\section{Methods}{
  \describe{
    \item{choicePlot}{\code{signature(x = "LongData")}: ... }
    \item{getId}{\code{signature(object = "LongData")}: ... }
    \item{getTime}{\code{signature(object = "LongData")}: ... }
    \item{getTraj}{\code{signature(object = "LongData")}: ... }
    \item{getTrajSizeMin}{\code{signature(object = "LongData")}: ... }
    \item{getVarName}{\code{signature(object = "LongData")}: ... }
    \item{importClusterization}{\code{signature(xLongData = "LongData")}: ... }
    \item{imputeLongData}{\code{signature(.Object = "LongData", method = "ANY", yPartition = "missing")}: ... }
    \item{imputeLongData}{\code{signature(.Object = "LongData", method = "ANY", yPartition = "Partition")}: ... }
    \item{plot}{\code{signature(x = "LongData", y = "ANY")}: ... }
    \item{plot}{\code{signature(x = "LongData", y = "Partition")}: ... }
    \item{print}{\code{signature(x = "LongData")}: ... }
    \item{selectTrajNoNA}{\code{signature(object = "LongData")}: ... }
    \item{setId<-}{\code{signature(object = "LongData")}: ... }
    \item{setTime<-}{\code{signature(object = "LongData")}: ... }
    \item{setTraj<-}{\code{signature(object = "LongData")}: ... }
    \item{setTrajSizeMin<-}{\code{signature(object = "LongData")}: ... }
    \item{setVarName<-}{\code{signature(object = "LongData")}: ... }
    \item{show}{\code{signature(object = "LongData")}: ... }
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

\seealso{
  \code{\linkS4class{ClusterizLongData},\link{kml-package}}
}
\examples{
}
\keyword{classes}
