\name{as.clusterizLongData}
\alias{as.cld.data.frame}
\alias{as.clusterizLongData.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: as.cld.data.frame ~}

\description{
  \code{as.cld.data.frame} turn a matrix into an object of class \code{\linkS4class{ClusterizLongData}}.
}
\usage{
as.cld(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),trajSizeMin=2,
   varName=sub("[[:digit:]]*$","",names(data)[timeCol[1]]),...)
as.cld.data.frame(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),trajSizeMin=2,
   varName=sub("[[:digit:]]*$","",names(data)[timeCol[1]]),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{[data.frame]}: contains longitudinal data. Each
    ligne refers to the trajectory of an individual. Each column refers to
    the time at which measures were made. Optionnaly, a column may
    refer to identifiers.}
  \item{\code{id}}{\code{[character]}: single identifier
    for each trajectory (ie each individual). By default,\code{id} is the first column of the \code{data.frame}.}
  \item{\code{timeCol}}{\code{[numeric]}: column number in which
    longitudinal data can be found. By default,\code{timeCol} is all the
    columns except for the first.}
  \item{\code{timeReal}}{\code{[numeric]}: time at which measures were
  made. By default,\code{timeReal} is \code{0:(length(timeCol)-1)}.}
  \item{\code{varName}}{\code{[character]}:
    Name of the variable being measured. By default, the name of the second column after having suppressed the numbers is chosen.}
  \item{\code{trajSizeMin}}{\code{[numeric]}:
    The trajectories that include missing values can either be excluded or included. \code{trajSizeMin} sets the minimum number of values that a trajectory must contain not to be excluded. Please note that trajectories that are completely missing (0 present values) must always be excluded.}
  \item{\dots}{For compatibility only}
}
\details{
  \code{as.cld} apply on a \code{data.frame} turn the \code{data.frame}
  into an object of class
  \code{\linkS4class{ClusterizLongData}}. Each line of the data frame
  refers to a trajectory (an individual), the columns specified in \code{timeCol} are the time.
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
  \code{\link{as.cld.artificialLongData}}, \code{\link{kml-package}}}


\examples{
### Simple use
dn <- data.frame(i=11:13,size12=c(15,13,14),size14=c(16,15,17),size20=c(18,16,16))
as.cld(dn)

### Changing parameters
as.cld(dn,i=101:103,timeCol=c(2,3,2,4,2),timeReal=c(1,2,4,8,16))
}

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series

