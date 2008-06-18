\name{as.clusterizLongData}
\alias{as.cld.artificialLongData}
\alias{as.clusterizLongData.artificialLongData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ function: as.cld.artificialLongData ~}
\description{
  \code{as.cld.ArtificialLongData} turn a object of class \code{\linkS4class{ArtificialLongData}} into an object of class \code{\linkS4class{ClusterizLongData}}.
}
\usage{
as.cld(data,...)
as.cld.artificialLongData(data,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{[ArtificialLongData]: contains an object of class \code{ArtificialLongData}.}
  \item{\dots}{For compatibility only}
}
\details{
  Object of class \code{\linkS4class{ArtificialLongData}}
  contain all the necessary fields for the creation of
  \code{\linkS4class{ClusterizLongData}} object.
  Conversion from the former to the latter is therefore automatic.
}
\value{
  An object of class \code{\linkS4class{ClusterizLongData}}.
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


\seealso{\code{\link{as.cld}}, \code{\link{as.cld.matrix}},
  \code{\link{as.cld.data.frame}}, \code{\link{generateArtificialLongData}}, \code{\link{kml-package}}}

\examples{
### Simple use
ga1 <- generateArtificialLongData()
as.cld(ga1)

### See example(generateArtificialLongData) for more examples
}

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series

