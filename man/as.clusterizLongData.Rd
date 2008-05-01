\name{as.clusterizLongData}
\alias{as.cld}
\alias{as.clusterizLongData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ Function: as.cld ~}

\description{
  \code{as.cld} (or \code{as.clusterizLongData}) turns data into an object of class \code{\linkS4class{ClusterizLongData}}.
}
\usage{
as.cld(data, ...)
as.clusterizLongData(data, ...)
}
\arguments{
  \item{data}{\code{data} is either a \code{matrix}, a \code{data.frame} or an object
    of class \code{\linkS4class{ArtificialLongData}}. See each method
    for detail.}
  \item{\dots}{See each method for detail.}

}
\details{
  For \code{matrix} see \code{\link{as.cld.matrix}}.\cr
  For \code{data.frame} see \code{\link{as.cld.data.frame}}.\cr
  For \code{ArtificialLongData}, see \code{\link{as.cld.artificialLongData}}.\cr
}
\value{
  An object of class \code{\link{ClusterizLongData}}.
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


\seealso{\code{\link{as.cld.matrix}}, \code{\link{as.cld.data.frame}},\code{\link{as.cld.artificialLongData}},\code{\link{kml-package}}}
\examples{
### as.cld for data.frame :
dn <- data.frame(i=11:13,size12=c(15,13,14),size14=c(16,15,17),size20=c(18,16,16))
as.cld(dn)

### as.cld for matrix
ma1 <- matrix(rnorm(50),ncol=5)
as.cld(ma1)

### as.cld for ArtificialLongData
ga1 <- generateArtificialLongData()
as.cld(ga1)

}

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series

