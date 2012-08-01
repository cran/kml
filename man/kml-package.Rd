\name{kml-package}
\alias{kml-package}
\docType{package}
\title{~ Overview: K-means for Longitudinal data ~}

\description{
  \code{KmL} is a implematation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package. For the description of the
  algorithm, see \code{\link{kml}}.
}
\details{
  \tabular{ll}{
    Package: \tab kml\cr
    Type: \tab Package\cr
    Version: \tab 2.1\cr
    Date: \tab 2012-08-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,clv,longitudinalData(>= 2.1)\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To cluster data, \code{KmL} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" partition

    \item Exporting results
  }
}


\section{1. Data preparation}{
  \code{kml} works on object of class \code{\linkS4class{ClusterLongData}}.
  Data preparation therefore simply consists in transforming data into an object \code{ClusterLongData}.
  This can be done via function
  \code{\link[=clusterLongData]{clusterLongData}}
  (\code{\link[=clusterLongData]{cld}} in short).
  It converts a \code{data.frame} or a \code{matrix} in \code{\linkS4class{ClusterLongData}}.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with
  \code{\link{generateArtificialLongData}}
  (\code{\link{gald}} in
  short).
}

\section{2. Building "optimal" partition}{
  Once an object of class \code{\linkS4class{ClusterLongData}} has been created, the algorithm
  \code{\link[=kml]{kml}} can be run.

  Starting with a \code{ClusterLongData}, \code{kml} built a \code{\linkS4class{Partition}}.
  A object of class \code{Partition} is a partition of trajectories
  into subgroups that also contains some information like the
  percentage of trajectories contained in each group or some quality critetion.\cr

  \code{kml} is a "hill-climbing" algorithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Partition}, it is better to run the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Partition} maximizing the determinant of the matrix between.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate
  clues that will enable us to choose.

  In the end, \code{kml} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{\link[=kml]{kml}} has constructed some
  \code{Partition}, the user can examine them one by one and choose
  to export some. This can be done via function
  \code{\link{choice}}. \code{choice} opens a graphic windows showing
  various information including the trajectories clutered by a specific
  \code{Partition}.

  When some \code{Partition} has been selected (the user can select
  more than 1), it is possible to
  save them. The clusters are therefore exported towards the file
  \code{name-cluster.csv}. Criteria are exported towards
  \code{name-criteres.csv}. The graphs are exported according to their
  extension.
}


\seealso{
  Classes : \code{\linkS4class{ClusterLongData}}, \code{\linkS4class{Partition}} \cr
  Methods : \code{\link{clusterLongData}}, \code{\link{kml}}, \code{\link{choice}}\cr
  Plot : \code{\link[=plot,ClusterLongData]{plot(ClusterLongData)}}
}

\examples{
### 1. Data Preparation
data(pregnandiol)
names(pregnandiol)
cldPregnan <- cld(pregnandiol,timeInData=1:30*2+1)

### 2. Building "optimal" clusteration (with only 3 redrawings)
kml(cldPregnan,nbRedrawing=3,toPlot="both")

### 3. Exporting results
try(choice(cldPregnan))
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
\keyword{models}   % Statistical Models



