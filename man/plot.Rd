\name{plot}
\alias{plot}
\alias{plot-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Functions: plot for KmL ~ }
\description{
  Four fonctions \code{plot} are define in the package KmL. See detail.
}
\usage{
plot(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{See details}
  \item{y}{See details}
  \item{\dots}{See details}
}
\details{
  Four methods \code{plot} are define in the package KmL, depending on the
  data they plot :
  \describe{
    \item{\code{\link[=plotTraj]{plot(ClusterizLongData)}} or \code{\link[=plotTraj]{plotTraj(ClusterizLongData)}}}{plot the trajectories relatively to a specific \code{Clusterization}}
    \item{\code{\link[=plotCalinski]{plot(Calinski)}} or \code{\link[=plotCalinski]{plotCalinski(Calinski)}}}{plot a matrix of Calinki's criterion.}
    \item{\code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}}{plot the trajectories clusters y clusters, relatively to a specific \code{Clusterization}}
    \item{\code{\link[=plotAll]{plotAll(ClusterizLongData)}}}{merge the three previous graph in one windows.}
  }
}

%\value{No values.}

\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}

\section{Author(s)}{
  Christophe M. Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author: <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}



\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}}, \code{\linkS4class{ArtificialLongData}} \cr
  Plot : \code{\link[=plotTraj]{plot(ClusterizLongData)}},
  \code{\link[=plotCalinski]{plot(Calinski)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}
\examples{
ld <- gald()
kml(ld,,5)
par(ask=TRUE)

### Default ploting
plot(ld)

### Plot all the trajectories
plotTraj(ld)

### Plot by subgroups
plotSubGroups(ld)

### Calinski and all at once
plotAll(ld,point="symbols",size=1)

par(ask=FALSE)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
