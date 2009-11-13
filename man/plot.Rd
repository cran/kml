\name{plot,ClusterizLongData}
\alias{plot}
\alias{plot-method}
\alias{plot,ClusterizLongData}
\alias{plot,ClusterizLongData,ANY-method}
\alias{plot,ClusterizLongData,missing-method}
\alias{plot,ClusterizLongData,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plot for ClusterizLongData ~ }
\description{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterizLongData}} relativly
  to a \code{\linkS4class{Clusterization}}.
}
\usage{
\S4method{plot}{ClusterizLongData,ANY}(x,y,col="clusters",col.mean="clusters",main="",type="l",type.mean="",size=1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData]: Object containing the trajectories to plot.}
  \item{y}{[numeric] or [vector(numeric)]: Give the \code{Clusterization}
    to represent. If \code{y} is missing, the \code{Clusterization} with
    the highest quality criterion is selected. If \code{y} is a number,
    the first \code{Clusterization} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Clusterization} of the sublist \code{c-y[1]} is selected.}
  \item{col}{[character], [numeric] or vector[numeric]: Specification of the plotting
    color of the individual trajectories. In addition to the standard
    possible values, col="clusters" can be use to color the individual
    trajectories according to their clusters.}
  \item{col.mean}{[character], [numeric] or vector[numeric]:
    Specification of the plotting color of the mean trajectories. In
    addition to the standard possible values, col="clusters" can be used
    to color each mean trajectories according to its clusters.}
  \item{main}{[character]: give the title of the graph.}
  \item{type}{[character]: what type of plot should be drawn for the individual trajectories.}
  \item{type.mean}{[character]: what type of plot should be drawn for
    the mean trajectories ?}
  \item{size}{[numeric]: If some point are added on the mean trajectories, \code{size} set the
    size of the point.}
  \item{\dots}{Graphical parameters to be passed to methods, see
    \code{\link[longitudinalData:plot]{plot(LongData)}} and
    \code{\link{par}} for details.}
}

\details{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterizLongData}} relativly
  to a \code{\linkS4class{Clusterization}}.
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
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[=plotCriterion]{plotCriterion)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}

\examples{
clusLd <- as.cld(gald())
kml(clusLd,,1)
par(ask=TRUE)

### Default ploting
plot(clusLd)

### Only the trajectories in black
plot(clusLd,type="n",col.mean="1",type.mean="l")

### Only the mean trajectories, with letters (for publication ?)
plot(clusLd,type="n",col.mean="1",type.mean="b",cex=2)

### All at once.
plot(clusLd,col="clusters",col.mean="clusters",type.mean="l",legend=FALSE)

par(ask=FALSE)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


