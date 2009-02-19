\name{plotAll,ClusterizLongData}
\alias{plotAll}
\alias{plotAll,ClusterizLongData}
\alias{plotAll,ClusterizLongData-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plotAll for ClusterizLongData ~ }
\description{
  \code{plot} the trajectories, the Calinski's criterions and the
  subgroups of an object \code{\linkS4class{ClusterizLongData}} on a
  single graph.
}
\usage{
plotAll(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData] Object containing the trajectories to plot.}
  \item{y}{[numeric] or [vector(numeric)] Give the \code{Clusterization}
    to represent. If \code{y} is missing, the \code{Clusterization} with
    the highest Calinski creterion is selected. If \code{y} is a number,
    the first \code{Clusterization} of the sublist \code{c y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Clusterization} of the sublist \code{c y[1]} is selected.}
  \item{\dots}{Graphical
    parameters to be passed to methods (see \code{plot}). For
    \code{ClusterizLongData} object specificly :
    \describe{
      \item{printCall}{[logical]: if TRUE, the Calinski criterion will
	be printed.}
      \item{printTraj}{[logical]: if TRUE, a "main" graph, showing all the
	trajectories at once, will be printed.}
      \item{printSub}{[logical]: if TRUE, several sub-graphs, each containing
	a specific cluster of trajetories, will be printed.}
      \item{colorTraj}{[character]: Precise the color of the
	trajectories on the main graph. \code{'black'} draw all the trajectories in
	black. 'color' draw each subgroups with a specific color. 'no' does
	not plot the trajectories.}
      \item{colorTrajSub}{[character] Precise the color of the
	trajectories on the sub-graphs. Same option that \code{colorTraj}.}
      \item{colorMean}{[character]: Precise the color of the
	mean trajectories of each subgroups on the main graph. \code{'black'} draw all the
	mean trajectories in black. 'color' draw each mean trajectories with
	a specific color. 'both' draw each mean trajectories with
	a specific color and add a small border in black. 'no' does
	not plot the mean trajectories.}
      \item{colorMeanSub}{[character]: Precise the color of the
	mean trajectories of each subgroups on the sub-graphs. Same option that \code{colorMean}.}
      \item{main}{[charater]: give the title on the main graph.}
      \item{mainSub}{[charater] give the title of the sub-graphs.}
      \item{point}{[character]: Precise the kind of the point to plot on the
	mean trajectories on the main graph. 'point' adds a black dot ; 'symbols' adds a symbol
	; 'letters' adds letters ; 'no' does not add points.}
      \item{pointSub}{[character]: Precise the kind of the point to plot on the
	mean trajectories on the sub-graphs. Same option that \code{point}.}
      \item{size}{[numeric]: If some point are added on the mean trajectories, \code{size} fixe the
	size of the point (on the main graph).}
      \item{sizeSub}{[numeric]: If some point are added on the mean trajectories, \code{size} fixe the
	size of the point on the sub-graphs.}
      \item{ylimSub}{[numeric]: give the y limit of the sub-graphs}
    }
  }
}

\details{
  For every \code{clusterizLongData} object, there is three possible
  graphical representation: calinski criterion, all the trajectories at once
  and trajectories clusters by clusters. This function combine these three
  graphical output (or only two, or one, accorind to the three arguments \code{printCal}, \code{printTraj} and
  \code{printSub}). Thus every option (like colorMean,...) are
  duplicate : one is for the main graph (colorMean), the second is for
  the sub-graphs (colorMeanSub).
}

\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}
\section{Author(s)}{
  Christophe Genolini\cr
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
  \code{\link[=plotCalinski]{plot(Calinski)}}, \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}
}

\examples{
ld <- gald()
kml(ld,2:5,5)
kml(ld,16,5)
par(ask=TRUE)

### Default ploting
plotAll(ld)

### Only the calinski criterion (same effect than plotCalinski(ld))
plotAll(ld,printCal=TRUE,printTraj=FALSE,printSub=FALSE)

### Groups and sub-groups
plotAll(ld,printCal=FALSE,printTraj=TRUE,printSub=TRUE)
plotAll(ld,printCal=FALSE,printTraj=TRUE,printSub=TRUE,colorTraj="black",colorMean="no",colorMeanSub="black")

### All at once
plotAll(ld,printCal=TRUE,printTraj=TRUE,printSub=TRUE)

### To see the clusterization with only 2 clusters
plotAll(ld,2,printCal=FALSE,printTraj=TRUE,printSub=TRUE)

### To see the third clusterization with 16 clusters
plotAll(ld,c(16,3),printCal=FALSE,printTraj=TRUE,printSub=TRUE)

par(ask=FALSE)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


