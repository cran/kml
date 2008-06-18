\name{choice}
\alias{choice}
\alias{choice-methods}
\alias{choice,ClusterizLongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ function: choice ~}
\description{
\code{choice} lets the user choose the \code{Clusterization} he wants
to export.
}
\usage{
choice(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{[ClusterizLongData]: Contains all the clusterization found by \code{KmL}.}
}


\details{
\code{choice} run in three steps:
\enumerate{
\item Choice of the \code{Clusterization}
\item Exporting the \code{Clusterization}
\item Selection of a graphic representation.
}
}


\section{1. Choice of the \code{Clusterization}}{
\code{choice} opens two graphic windows. In the left one, the Calinski
criteria of all \code{Clusterization} found by \code{kml} are represented.
They are filed according to the number of clusters, from the biggest to
the smallest. This windows let the user to select a
\code{Clusterization}.
The selected \code{Clusterization} is pointed with a dark spot.


In the right window, the \code{Clusterization} selected by the user is represented.

When \code{choice} is called, the \code{Clusterization} having the larger Calinski criterion is selected.
It is possible to visualize the other \code{Clusterization} by using the
arrows on the keyboard.

When the choice of a \code{Clusterization} has been made and needs to be
exported, the use can go on to the next step by pressing "Return".
}


\section{2. Exporting the \code{Clusterization}}{
When a \code{Clusterization} is being chosen, there are three possibilities :
\enumerate{
\item "Return" enables the visualization
\code{Clusterization} on screen (but nothing else).
\item Entering the name of a file \code{nomDeFichier} enables the user to export the \code{Clusterization}.
Clusters are exported in the file
\code{nomDeFichier-cluster.csv}. Criteria are exported in \code{nomDeFichier-criteres.csv}. Distances and posterior probabilities are in \code{nomDeFichier-distance.csv} (not implemented yet).
\item Entering a name preceded by the symbol \code{->} (like
\code{->variablesNames}) enables the user to save the
\code{Clusterization}. Clusters are stored in the variable
\code{nomVariable_cluster.csv} as a \code{data.frame}. Criteria are
saved in \code{nomDeVariable_criters.csv} as lists. Distances and
posterior probabilities are saved \code{nomDeVariable_distance.csv}
(not implemented yet).
}
}

\section{3. Selection of a graphic representation}{
Finally, it is possible to export a graphic representation of the
\code{Clusterization}. With the keyboard, the user can modify the aspect of the graph.
\itemize{
\item Keys \code{e}, \code{r} and \code{t} deal with individual trajectories.
\item Keys \code{d}, \code{f} and \code{g} deal with the average trajectories of clusters.
\item Keys \code{c}, \code{v} and \code{b} deal with subgroups.
\item Keys \code{h}, \code{j}, \code{k}, \code{l} and \code{o} deal with symbols on the graph.
}

More precisely:

\itemize{
\item{e}{Suppresses the display of individual trajectories.}
\item{r}{Displays individual trajectories in black and white.}
\item{t}{Displays individual trajectories in color.}
\item{d}{Suppresses the display of average trajectories of clusters.}
\item{f}{Displays the average trajectories of clusters in black and white.}
\item{g}{Displays the average trajectories of clusters in color.}
\item{c}{Suppresses the display of subgroups.}
\item{v}{Displays the subgroups in black and white.}
\item{b}{Displays subgroups in color.}
\item{h}{Suppresses the display of points of average trajectories}
\item{j}{Displays the points of average trajectories as letters}
\item{k}{Displays the points of average trajectories as symbols}
\item{o}{Enlarges fonts}
\item{l}{Makes fonts smaller}
}

When the final graph is choisen by pressing "Return". It can then be exported as usual with a right click on the figure.
}

\value{Non applicable}

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

\seealso{\link{kml-package}}

\examples{
cld1 <- as.cld(gald())
kml(cld1,2:3,3)
#choice(cld1)
}

\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
