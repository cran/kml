\name{kml}
\alias{kml}
\alias{kml-method}
\alias{kml,ClusterizLongData-method}

\title{~ Algorithm kml: K-means for Longitidinal data ~}

\description{
  \code{kml} is a new implematation of k-means for longitudinal data (or trajectories). This algorithm is able to deal with missing value and
  provides an easy way to re roll the algorithm several times, varying the starting conditions and/or the number of clusters looked for.

  Here is the description of the algorithm. For an overview of the package, see \link{kml-package}.
}

\usage{
kml(Object, nbClusters = 2:6, nbRedrawing = 20, saveFreq = 100,
    maxIt = 200, trajMinSize = 2, print.cal = FALSE,
    print.traj = FALSE, imputationMethod = "copyMean",
    distance, power = 2, centerMethod = meanNA, startingCond = "allMethods",
    distanceStartingCond = "euclidean", ...)
}

\arguments{
  \item{Object}{[ClusterizLongData]: contains trajectories to clusterize as well as previous \code{\linkS4class{Clusterization}}.}
  \item{nbClusters}{[vector(numeric)]: Vector containing the number of clusters
    with which \code{kml} must work. By default,
    \code{nbClusters} is \code{2:6} which indicates that \code{kml} must
    search partitions with respectively 2, then 3, ... up to 6
    clusters. Maximum number of cluster is 52.}
  \item{nbRedrawing}{[numeric]: Sets the number of time that k-means
  must be re-run (with
    different starting conditions) for each number of clusters.}
  \item{saveFreq}{[numeric]: Long computations can take several
    days. So it is possible to save the object \code{ClusterizLongData}
    once in a wilde. \code{saveFreq} define the frequency of the saving
    process. The \code{ClusterizLongData} is saved every \code{saveFreq}
    clusterization calculations. The object is save in the file
    \code{objectName.Rdata} in the curent folder.}
  \item{maxIt}{[numeric]: Sets a limit to the number of iteration if
    convergeance is not reach.}
  \item{trajMinSize}{[numeric]: The trajectories that include missing
    values can either be excluded or included. \code{trajMinSize} sets the
    minimum number of values that a trajectory must contain not to be
    excluded. For example, if the trajectories have 7 mesurements (time=7)
    and \code{trajMinSize} is set to 3, the trajectory (5,3,NA,4,NA,NA,NA) will
    be include in the calculation while (2,NA,NA,NA,4,NA,NA) will be
    exclude. Please note that trajectories that are completely missing (0
    present values) must always be excluded.}
  \item{print.cal}{[logical]: If TRUE, the quality criterion will be
    print on screen during computation (if the number of redrawing is
    big, this can slow the overall calculation process).}
  \item{print.traj}{[logical]: If TRUE, each step of k-means is print on
    screen during the calculation. This can slow the overall calculation
    process by a factor 25, see "optimisation" below.}
  \item{imputationMethod}{[character]: the calculation of quality
    criterion can not be done if some value are
    missing. \code{imputationMethod} define the method use to impute the
    missing value. It should be one of
    "LOCF","LOCB","linearInterpolation","linearInterpolation2","linearInterpolation3"
    or "copyMean". See \code{\link[longitudinalData]{imputation}} for detail.}
  \item{distance}{[numeric <- function(trajectory,trajectory)] function that compute the
    distance between two trajectories. If no function is specified, the Euclidian
    distance with Gower adjustment is used (Gower adjustment takes in accomp
    missing value.) Using a classical distance speed up the overall calculation
    process by a factor 25, see "optimisation" below.}
  \item{power}{[numeric]: power define the parameter of the minkovski
    distance, if used.}
  \item{centerMethod}{[numeric <- function(vector(numeric))]: k-means algorithm compute centers of
    each clusters. It is possible to personnalised the definition of
    "center" by defining a function "centerMethod". This function should
    take a vector of numeric as argument and return a single numeric -the
    center of the vector-}.
  \item{startingCond}{[character]: specify the starting
  condition. Should be one of  "maxDist", "randomAll", "randomK" or
  "allMethods". See detail.}
  \item{distanceStartingCond}{[character]: some starting condition needs
    to compute the distance matrix of the
    trajectories. \code{distanceStartingCond} define the distance that will be
    use to calculate this matrix. It should be one of "euclidean",
    "maximum", "manhattan", "canberra", "binary" or "minkowski".}
  \item{\dots}{For graphical parameters.}
}

\details{
  \code{kml} works on object of class \code{ClusterizLongData}.
  For each number included in \code{nbClusters}, \code{kml} compute a
  \code{\link{Clusterization}} then stores it in the field
  \code{clusters} of the object \code{ClusterizLongData} according to its number of clusters.
  The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a \code{Clusterization} has been found, it is added to the slot
  \code{clusters}. \code{clusters} is a list of 52 sublist called c1,
  c2, c3 until c52. The sublist cX store the all \code{Clusterization} with
  X clusters. Inside a sublist, the
  \code{Clusterization} are sort from the biggest quality criterion to
  the smallest (the best are stored first).

  Note that \code{Clusterization} are saved throughout the algorithm. If the user
  interrupt the execution of \code{kml}, the result is not lost. If the
  user run kml on an object then run kml again on the same object, the
  \code{Clusterization} that are computed the second time are added to
  the one allready present in the object (unless you "clear" some
  list, see \code{Object["clusters","clear"]<-value} in
  \code{\link{ClusterizLongData}}).

  The possible starting conditions are "randomAll", "randomK" and
  "maxDist", as define in \code{\link{partitionInitialise}}. In
  addition, the method "allMethods" is a shortcut that run a "maxDist", a "randomAll"
  and "randomK" for all the other re rolling.
}

\section{Optimisation}{
  Behind kml, there is two different procedure :
  \enumerate{
    \item Fast: when the parameter \code{distance} is set to a classical
  distance (one of "euclidean", "maximum", "manhattan", "canberra",
  "binary" or "minkowski") and \code{print.traj} is set to \code{FALSE}
  (the default), \code{kml} call a C
    compiled (optimized) procedure.
    \item Slow: when the user define its own distance or if he wants
    to see the construction of the clusters by setting \code{print.traj=TRUE}, \code{kml} uses a R non compiled
    programmes.
  }
  The C prodecure is 25 times faster than the R one.

  So we advice to use the R procedure 1/ for trying some new method
  (like using a new distance) or 2/ to "see" the very first cluster
  construction, in order to check that every thing goes right, then to
  sweetch to the C procedure (like we do in \code{Example} section).

  If for a specific use, you need a different distance, feel free to
  contact the author.
}

\value{
  A \code{\linkS4class{ClusterizLongData}} object, after having added
  some \code{\link{Clusterization}} to it.
}

\references{Article submited\cr
  Web site: \code{\link[http://christophe.genolini.free.fr/kml]{http://christophe.genolini.free.fr/kml}}
}
\section{Author(s)}{
  Christophe Genolini\cr
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
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{choice}}
}

\examples{
### Generation of some data
cld1 <- as.cld(generateArtificialLongData())

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
#     We want to "see" what happen (so printCal and printTraj are TRUE)
kml(cld1,2:6,3,printCal=TRUE,printTraj=TRUE)

### 4 seems to be the best. But to be sure, we try more redrawing 4 or 6 only.
#     We don't want to see again, we want to get the result as fast as possible.
kml(cld1,c(4,6),10)
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
