\name{updateClusterization}
\Rdversion{1.1}
\alias{updateClusterization}
\title{
~ Function: updateClusterization ~
}
\description{
This fonction converts an object \code{Clusterization} V0.9 to an object
\code{Clusterization} V1.0.
}
\usage{
updateClusterization(object)
}

\arguments{
  \item{object}{[Clusterization]: object to convert}
}
\details{
  Objects used by \code{KmL} change with version V1.0. The new object structure is
  not compatible with the old one. This function lets the user convert
  its old object \code{\linkS4class{Clusterization}} to the new structure.

  For that, just open R ; load the old object ; load the new KmL library
  ; use \code{updateClusterization}.

}

\value{
  \code{\linkS4class{Clusterization}} version V1.0
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
####################
### Not executable
### if oldObject is your old data :
###
#
# newObject <- updateClusterization(oldObject)
#
}
\keyword{methods}
