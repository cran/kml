\name{as.clusterizLongData}
\alias{as.cld}
\alias{as.clusterizLongData}
\alias{as.clusterizLongData,data.frame-method}
%- Also NEEDS an '\alias' for EACH other topic documented here.
\title{~ Function: as.clusterizLongData (or as.cld) ~}

\description{
  \code{as.clusterizLongData} (or \code{as.cld}) turns a data.frame into
  an object of class \code{\linkS4class{ClusterizLongData}}.
}
\usage{
as.cld(data, ...)

#as.cld(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),trajMinSize=1,
#   varName=sub("[[:digit:]]*$","",names(data)[timeCol[1]]),...)

as.clusterizLongData(data, ...)

#as.clusterizLongData(data,id=data[,1],timeCol=2:length(data),timeReal=0:(length(timeCol)-1),trajMinSize=1,
#   varName=sub("[[:digit:]]*$","",names(data)[timeCol[1]]),...)
}

\arguments{
  \item{data}{\code{[data.frame]}: contains the trajectories (longitudinal data). Each
    line refers to the trajectory of an individual. Each column refers to
    the time at which measures were made. Optionally, the first column may
    refer to identifiers.}
  \item{\dots}{
    \itemize{
      \item{id}{\code{[character]}: single identifier
	for each trajectory (i.e. each individual). By default,\code{id} is the first column of the \code{data.frame}.}
      \item{timeCol}{\code{[numeric]}: column number in which
	longitudinal data can be found. By default,\code{timeCol} is all the
	columns except for the first.}
      \item{timeReal}{\code{[numeric]}: time at which measures were
	made in "real" life. By default,\code{timeReal} is \code{0:(length(timeCol)-1)}.}
      \item{varName}{\code{[character]}:
	Name of the variable being measured. By default, the name of the
	second column after suppressing the numbers is chosen (if the name
	of the second column is \code{SizeAt4}, then \code{varName} will be \code{SizeAt}).}
      \item{trajMinSize}{\code{[numeric]}:
	The trajectories that include missing values can either be excluded
	or included. \code{trajMinSize} sets the minimum number of values that
	a trajectory must contain not to be excluded.  For example, if the trajectories have 7 measurements
	(\code{time=7}) and \code{trajSizeMin} is set to 3, the trajectory
	\code{(5,3,NA,4,NA,NA,NA)} will be included in the calculation while
	\code{(2,NA,NA,NA,4,NA,NA)} will be excluded. Please note that trajectories that are completely missing (0 present values) must always be excluded.}
    }
  }
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







\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterizLongData}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link[longitudinalData]{generateArtificialLongData}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotCriterion]{plot(Calinski)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}



\examples{
### Simple use
dn <- data.frame(i=11:13,size12=c(15,13,14),size14=c(16,15,17),size18=c(18,16,16))
as.clusterizLongData(dn)

### Changing parameters
as.clusterizLongData(dn,i=c("H101","H108","B103"),timeCol=c(2,2,3,3,3,3,4),timeReal=12:18)
}

\keyword{datagen}  % Functions for generating data sets
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{ts}       % Time Series

