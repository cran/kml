\name{clusterizLongData}
\alias{cld}
\alias{clusterizLongData}
\alias{clusterizLongData,ANY,ANY,ANY,ANY,ANY-method}
\alias{clusterizLongData,missing,missing,missing,missing,missing-method}
\title{ ~ Function: clusterizLongData (or cld) ~}
\description{
  \code{clusterizLongData} (or \code{cld} in short) is the constructor
  for \code{\linkS4class{ClusterizLongData}} object.
}
\usage{
clusterizLongData(traj, id, time, varName = "V", trajMinSize = 1)
cld(traj, id, time, varName = "V", trajMinSize = 1)
}

\arguments{
    \item{traj}{\code{[array(numeric)]}: contains longitudinal
    data. Each line is the trajectory of an individual. The columns refer to the time during which measures were made.}
    \item{id}{\code{[character]} : single identifier
      for each individual (i.e. each trajectories). Note that the identifiers are of type
      \code{character} (that allow to deal identifiers like \code{XUK32-612}
      identifiers that our favorite epidemiologists are so good at providing) . If \code{numeric} are provided, they are converted into \code{characters}.}
    \item{time}{\code{[numeric]}: time during which measures were made.}
    \item{varName}{\code{[character]}:
      name of the variable measured.}
    \item{trajMinSize}{\code{[numeric]}:
      Trajectories whose values are partially missing can either be excluded by treatment, or included. \code{trajSizeMin} sets the
      minimum number of values that a trajectory must contain not to be excluded. For example, if the trajectories have 7 measurements
      (\code{time=7}) and \code{trajSizeMin} is set to 3, the trajectory
      \code{(5,3,NA,4,NA,NA,NA)} will be included in the calculation while
      \code{(2,NA,NA,NA,4,NA,NA)} will be excluded. Please note that trajectories
      that are totally missing (i.e. 0 present values) are always excluded.}
}

\details{
  \code{clusterizLongData} create a
  \code{\linkS4class{ClusterizLongData}} object and set its slot to the
  corresponding value. Note that the field \code{clusters} cannot be
  initialized through this function since this slot is not supposed to be
  manipulated by the user (only by \code{kml}).
}


\value{
  Object of class \code{\linkS4class{ClusterizLongData}}.
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
  Methods : \code{\link{choice}}, \code{\link{as.clusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}


\examples{
################
### Creation of some trajectories
mat <- matrix(c(1,2,3,1,NA,6,1,8,NA),3)

################
### Creation of LongitudinalData
(ld1 <- cld(traj=mat,id=1:3,time=c(1,2,3),varName="V",trajMinSize=2))
(ld2 <- clusterizLongData(traj=mat,id=c("A-101","A-102","A-108"),time=c(2,4,8),varName="Age",trajMinSize=2))
}
\keyword{classes}
