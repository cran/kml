\name{kml}
\alias{kml}
\alias{kml,ClusterizLongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ Algorithm: KmL, K-means for Longitidinal data ~}

\description{
  \code{KmL} is a non parametric algorithm for clustering longitudinal
  data. Here is the description of the algorithm. For an overview of the package,
  see \link{kml-package}.
}
\usage{
kml(.Object, nbClusters = 2:6, nbRedrawing = 20, maxIt = 200, print = "calinski", distance = "euclidean")
}
\arguments{
  \item{.Object}{[ClusterizLongData]: contains trajectories to clusterize as well as pre-existing clusterizations.}
  \item{nbClusters}{[numeric] Vector containing the number of clusters
  with which KmL must work. By default,
  \code{nbClusters} is \code{2:6} which indicates that \code{KmL} must
  search partitions with respectively 2, then 3, ... up to 6
  clusters. Maximum number of cluster is 10.}
  \item{nbRedrawing}{[numeric] Sets the number of retire to execute for
  each number of clusters.}
  \item{maxIt}{[numeric]: sets a limit to the number of iteration before convergeance.}
  \item{print}{[character]: can take on two values : \code{"all"}
  forces the display of the algorithme under progression. Any other value suppresses this display (faster).}
  \item{distance}{[character] method used to measures the distance
  between trajectories (only "euclidean" is avalable for now).}
}

\details{
  \code{kml} works on object of class \code{ClusterizLongData}.
  For each number included in \code{nbClusters}, \code{kml} looks for a
  clusterization then stores it in the field \code{clusterizList} according to its number of clusters. The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a clusterization has been found, it is added to the slot
  \code{clusterizList}. \code{clusterizList} stores all the partitions
  found according to their number of clusters. The
  \code{Clusterization} with the same number of clusters are sort from the biggest Calinski criterion to
  the smallest. So the best are stored first.

  \code{Clusterization} are saved throughout. If the user wish to
  interrupt the execution of \code{kml}, the result will not be lost.
}
\value{
   A class \code{\linkS4class{ClusterizLongData}} object, after having added supplementary \code{Clusterization}.
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

\seealso{\link{kml-package}}

\examples{
### Generation of some data
cld1 <- as.cld(generateArtificialLongData())

### We suspect 2, 3 4 or 5 clusters, we want 3 redrawing.
#     And we want to "see" what happen (so print="all")
#kml(cld1,2:6,3,print="all")

### 3 seems to be the best. But to be sure, we try more redrawing 3 or 6 only.
#     We don't want to see again, we want to get the result as fast as possible.
#kml(cld1,c(3,6),10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


\keyword{dplot}    % Computations Related to Plotting
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
