\name{plot,ClusterizLongData}
\alias{plot,ClusterizLongData}
\alias{plot,ClusterizLongData-method}
\alias{plotTraj}
\alias{plotTraj,ClusterizLongData}
\alias{plotTraj,ClusterizLongData-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ function: plot for ClusterizLongData ~ }
\description{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterizLongData}} relativly
  to a \code{\linkS4class{Clusterization}}.
}
\usage{
\S4method{plot}{ClusterizLongData}(x, y, colorTraj = "color", colorMean = "both", main =
                 "", point = "no", size = 1, ...)
plotTraj(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData]: Object containing the trajectories to plot.}
  \item{y}{[numeric] or [vector(numeric)]: Give the \code{Clusterization}
    to represent. If \code{y} is missing, the \code{Clusterization} with
    the highest Calinski creterion is selected. If \code{y} is a number,
    the first \code{Clusterization} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Clusterization} of the sublist \code{c-y[1]} is selected.}
  \item{colorTraj}{[character]: Precise the color of the
    trajectories. \code{'black'} draw all the trajectories in
    black. 'color' draw each subgroups with a specific color. 'no' does
    not plot the trajectories.}
  \item{colorMean}{[character]: Precise the color of the
    mean trajectories of each subgroups. \code{'black'} draw all the
    mean trajectories in black. 'color' draw each mean trajectories with
    a specific color. 'both' draw each mean trajectories with
    a specific color and add a small border in black. 'no' does
    not plot the mean trajectories.}
  \item{main}{[charater]: give the title of the graph.}
  \item{point}{[character]: Precise the kind of the point to plot on the
    mean trajectories. 'point' add a black dot ; 'symbols' add a symbol
    ; 'letters' add letters ; 'no' do not add points.}
  \item{size}{[numeric]: If some point are added on the mean trajectories, \code{size} fixe the
    size of the point.}
  \item{\dots}{Graphical
    parameters to be passed to methods (see \code{plot}). For
    \code{ClusterizLongData} object specificly :
  }
}

\details{
  \code{plot} the trajectories of an object \code{\linkS4class{ClusterizLongData}} relativly
  to a \code{\linkS4class{Clusterization}}.
  }


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
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{ArtificialLongData}} \cr
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[=plotCalinski]{plot(Calinski)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}

\examples{
ld <- gald()
kml(ld,,5)
par(ask=TRUE)

### Default ploting
plot(ld)

### Only the trajectories in black
plot(ld,colorMean="no",colorTraj="black")

### Only the mean trajectories, with letters (for publication ?)
plot(ld,colorTraj="no",colorMean="black",point="letters",size=2)

### All at once.
plot(ld,colorMean="both",point="letters",size=2)

par(ask=FALSE)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


