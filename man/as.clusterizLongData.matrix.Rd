\name{as.clusterizLongData}
\alias{as.cld.matrix}
\alias{as.clusterizLongData.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ function: as.cld.matrix ~}
\description{
  \code{as.cld.matrix} turn a matrix into an object of class \code{\linkS4class{ClusterizLongData}}.
}
\usage{
as.cld(data, id=1:nrow(data), time=1:ncol(data), varName="V",trajSizeMin=2,...)
as.cld.matrix(data, id=1:nrow(data), time=1:ncol(data), varName="V", trajSizeMin=2,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{[matrix(numeric)]}: contains longitudinal data. Each
    line refers to the trajectory of an individual.
    Columns correspond to the time at which measures were made.}
  \item{\code{id}}{\code{[character]} : single identifier
    for each trajectory (ie each individual). By default,\code{id} is simply the line number.}
  \item{\code{time}}{\code{[numeric]}: time at which measures
    were made. By default,\code{time} is simply the number of
    the column of the matrix.}
  \item{\code{varName}}{\code{[character]}:
    Name of the variable being measured ("V" by default).}
  \item{\code{trajSizeMin}}{\code{[numeric]}:
    The trajectories with missing values can either be excluded or included. \code{trajSizeMin} sets the minimum number of values that a trajectory must contain not to be excluded. Please note that the trajectories which are totally missing (0 present values) must always be excluded.}
  \item{\dots}{For compatibility only}
}

\details{
  \code{as.cld} apply on a matrix turn the matrix into an object of class
  \code{\linkS4class{ClusterizLongData}}. Each line of the matrix refers to a trajectory an individual), each column to a time measure. By default, identifiers \code{id} correspond to line numbers, time to column numbers.

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


\seealso{\code{\link{as.cld}}, \code{\link{as.cld.data.frame}},
  \code{\link{as.cld.artificialLongData}}, \code{\link{kml-package}}}


\examples{
### Simple use
ma1 <- matrix(rnorm(50),ncol=5)
as.cld(ma1)

### Changing parameters
as.cld(ma1,time=(1:5)*2,id=paste("U",101:110,sep=""))
}

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series


