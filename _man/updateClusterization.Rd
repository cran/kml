\name{updateClusterization}
\Rdversion{1.1}
\alias{updateClusterization}
\title{
~ Function: updateClusterization ~
}
\description{
This fonction convert an object \code{Clusterization} V0.9 to an object
\code{Clusterization} V1.0.
}
\usage{
updateClusterization(object)
}

\arguments{
  \item{object}{[Clusterization]: object to convert}
}
\details{
  Objects use by \code{KmL} change with version V1.0. The new object structure is
  not compatible with the old one. This function let the user convert
  its old object \code{\linkS4class{Clusterization}} to the new struture.

  For that, just open R ; load the old object ; load the new KmL library
  ; use \code{updateClusterization}.

}

\value{
  \code{\linkS4class{Clusterization}} version V1.0
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
