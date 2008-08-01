\name{kml}
\alias{kml}
\alias{kml-method}
\alias{kml,ClusterizLongData-method}

\title{~ Algorithm kml: K-means for Longitidinal data ~}

\description{
  \code{KmL} is a new implematation of k-means for longitudinal data (or trajectories). This algorithm is able to deal with missing value and
  provides an easy way to re roll the algorithm several times, varying the starting conditions and/or the number of clusters looked for.

  Here is the description of the algorithm. For an overview of the package, see \link{kml-package}.
}

\usage{
kml(Object, nbClusters = 2:6, nbRedrawing = 20, saveFreq = 100,
    maxIt = 200, printCal = FALSE, printTraj = FALSE,
    distance=function(x,y){return(dist(t(cbind(x,y))))})
}
\arguments{
  \item{Object}{[ClusterizLongData]: contains trajectories to clusterize as well as pre-existing clusterizations.}
  \item{nbClusters}{[vector(numeric)]: Vector containing the number of clusters
    with which \code{kml} must work. By default,
    \code{nbClusters} is \code{2:6} which indicates that \code{kml} must
    search partitions with respectively 2, then 3, ... up to 6
    clusters. Maximum number of cluster is 25.}
  \item{nbRedrawing}{[numeric]: Sets the number of k-means (with
    different starting conditions) that must be
    run for each number of clusters.}
  \item{saveFreq}{[numeric]: Long computations can take several
    days. So it is possible to save the object \code{ClusterizLongData}
    once in a wilde. \code{saveFreq} define the frequency of the saving
    process. The \code{ClusterizLongData} is saved every \code{saveFreq}
    clusterization calculations. The object is save in the file
    \code{objectName.Rdata} in the curent folder.}
  \item{maxIt}{[numeric]: Sets a limit to the number of iteration if convergeance is not reach.}
  \item{printCal}{[logical]: If TRUE, the calinski criterion will be
    print on screen during computation (if the number of redrawing is
    big, this can slow the overall calculation process).}
  \item{printTraj}{[logical]: If TRUE, each step of k-means is print on
    screen during the calculation. This slow the overall calculation
    process by a factor 25, see "optimisation" below.}
  \item{distance}{[function(numeric,numeric)] function that compute the
    distance between two trajectories. The default function is the Euclidian
    distance with Gower adjustment (Gower adjustment takes in accomp
    missing value.) Changing the distance can slow the overall calculation
    process by a factor 25, see "optimisation" below.}
}

\details{
  \code{kml} works on object of class \code{ClusterizLongData}.
  For each number included in \code{nbClusters}, \code{kml} compute a
  \code{\link{Clusterization}} then stores it in the field
  \code{clusters} of the object \code{ClusterizLongData} according to its number of clusters.
  The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a \code{Clusterization} has been found, it is added to the slot
  \code{clusters}. \code{clusters} is a list of 25 sublist called c1,
  c2, c3 until c25. The sublist cX store the all \code{Clusterization} with
  X clusters. Inside a sublist, the
  \code{Clusterization} are sort from the biggest Calinski criterion to
  the smallest. So the best are stored first.

  Note that \code{Clusterization} are saved throughout the algorithm. If the user
  interrupt the execution of \code{kml}, the result is not lost. If the
  user run kml on an object, then run kml again on the same object, the
  \code{Clusterization} that are computed the second time are added to
  the one allready present in the object (unless you use "clear" some
  list, see "\code{Object["clusters","clear"]<-value}" in \code{\link{ClusterizLongData}}).
}

\section{Optimisation}{
  Behind kml, even if the final user does not see it, there is two different procedure :
  \enumerate{
     \item Fast: when the user does not change the default \code{distance} (Euclidean with Gower
     adjustement) and the default \code{printTraj} (FALSE), kml call a C
     compiled and optimized procedure.
     \item Slow: when the user define its own distance, or if he wants
     to see the construction of the clusters, kml uses a R non compiled
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
   some \code{Clusterization} to it.
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
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}}, \code{\linkS4class{ArtificialLongData}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{generateArtificialLongData}}, \code{\link{choice}}
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
