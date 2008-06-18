\name{Partition-class}
\docType{class}
\alias{Partition-class}
\alias{expandPartition,Partition-method}
\alias{getClusters,Partition-method}
\alias{getId,Partition-method}
\alias{getNbClusters,Partition-method}
%\alias{imputeLongData,LongData,ANY,Partition-method}
%\alias{plot,LongData,Partition-method}
\alias{print,Partition-method}
\alias{setClusters<-,Partition-method}
\alias{setId<-,Partition-method}
\alias{setNbClusters<-,Partition-method}
\alias{show,Partition-method}

\title{~ Class: Partition ~}
\description{For internal uses only. See
  \code{\linkS4class{Clusterization}} for details.}
}

\section{Slots}{
  \describe{
    \item{\code{id}:}{See \code{\linkS4class{Clusterization}} for details.}
    \item{\code{nbClusters}:}{See \code{\linkS4class{Clusterization}} for details.}
    \item{\code{clusters}:}{See \code{\linkS4class{Clusterization}} for details.}
  }
}





\section{Methods}{
  \describe{
    \item{expandPartition}{\code{signature(object = "Partition")}: ... }
    \item{getClusters}{\code{signature(object = "Partition")}: ... }
    \item{getId}{\code{signature(object = "Partition")}: ... }
    \item{getNbClusters}{\code{signature(object = "Partition")}: ... }
    \item{imputeLongData}{\code{signature(.Object = "LongData", method = "ANY", yPartition = "Partition")}: ... }
    \item{plot}{\code{signature(x = "LongData", y = "Partition")}: ... }
    \item{print}{\code{signature(x = "Partition")}: ... }
    \item{setClusters<-}{\code{signature(object = "Partition")}: ... }
    \item{setId<-}{\code{signature(object = "Partition")}: ... }
    \item{setNbClusters<-}{\code{signature(object = "Partition")}: ... }
    \item{show}{\code{signature(object = "Partition")}: ... }
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
  \code{\linkS4class{Clusterization}}
}
\keyword{classes}
