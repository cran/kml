\name{plotCalinski}
\alias{plot,Calinski}
\alias{plot,Calinski-method}
\alias{plotCalinski}
\alias{plotCalinski-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plot for Calinski ~ }
\description{
  \code{plot} the Calinski creterion.
}
\usage{
\S4method{plot}{Calinski}(x, y, ...)
plotCalinski(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[Calinski] (or [matrix]) Object containing the trajectories to plot.}
  \item{y}{For compatibility only}
  \item{\dots}{For compatibility only}
}

%\value{No values.}

\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}

\section{Author(s)}{
  Christophe M. Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\seealso{
  Overview: \code{\link{kml-package}} \cr
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{ArtificialLongData}} \cr
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[=plotTraj]{plot(ClusterizLongData)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}

\examples{
ld <- gald()
kml(ld,2:4,2)
plot(ld["calinski"])
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


