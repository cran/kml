\name{updateClusterizLongData}
\Rdversion{1.1}
\alias{updateClusterizLongData}
\title{
~ Function: updateClusterizLongData ~
}
\description{
This fonction convert an object \code{ClusterizLongData} V0.9 to an object
\code{ClusterizLongData} V1.0.
}
\usage{
updateClusterizLongData(object)
}

\arguments{
  \item{object}{[ClusterizLongData]: object to convert}
}
\details{
  Objects use by \code{KmL} change with version V1.0. The new object structure is
  not compatible with the old one. This function let the user convert
  its old object \code{\linkS4class{ClusterizLongData}} to the new struture.

  For that, just open R ; load the old object ; load the new KmL library
  ; use \code{updateClusterizLongData}.

}

\value{
  \code{\linkS4class{ClusterizLongData}} version V1.0
}

\examples{
####################
### Not executable
### if oldObject is your old data :
###
#
# newObject <- updateClusterizLongData(oldObject)
#
}
\keyword{methods}
