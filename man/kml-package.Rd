\name{kml-package}
\alias{kml-package}
\docType{package}
\title{~ Overview: K-means for Longitudinal data ~}

\description{
  \code{KmL} is a new implematation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package. For the description of the
  algorithm, see \code{\link{kml}}.
}
\details{
  \tabular{ll}{
    Package: \tab kml\cr
    Type: \tab Package\cr
    Version: \tab 1.1.3\cr
    Date: \tab 2011-07-07\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,clv,longitudinalData\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To clusterize data, \code{KmL} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" clusterization.

    \item Exporting results
  }
}


\section{1. Data preparation}{
  \code{kml} works on object of class \code{\linkS4class{ClusterizLongData}}.
  Data preparation therefore simply consists in transforming data into an object \code{ClusterizLongData}.
  This can be done via function
  \code{\link[=clusterizLongData]{clusterizLongData}}
  (\code{\link[=clusterizLongData]{cld}} in short) or
  \code{\link[=as.clusterizLongData]{as.clusterizLongData}}
  (\code{\link[=as.clusterizLongData]{as.cld}} in short).
  The formers lets the user build some data from scratch, the latters
  converts a \code{data.frame} in \code{\linkS4class{ClusterizLongData}}.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with
  \code{\link[longitudinalData:generateArtificialLongData]{generateArtificialLongData}}
  (\code{\link[longitudinalData:generateArtificialLongData]{gald}} in
  short) from the package \code{longitudinalData}.
}

\section{2. Building "optimal" clusterization}{
  Once an object of class \code{\linkS4class{ClusterizLongData}} has been created, the algorithm
  \code{\link[=kml]{kml}} can be run.

  Starting with a \code{ClusterizLongData}, \code{kml} built a \code{\linkS4class{Clusterization}}.
  A object of class \code{Clusterization} is a partition of trajectories
  into subgroups that also contains some information like the
  percentage of trajectories contained in each group or some quality critetion (like the Calinski &
  Harabasz).\cr

  \code{kml} is a "hill-climbing" algorithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Clusterization}, it is better to execute the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Clusterization} maximising the determinant of the matrix between.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate
  clues that will enable us to choose.

  In the end, \code{kml} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{\link[=kml]{kml}} has constructed some
  \code{Clusterization}, the user can examine them one by one and choose
  to export some. This can be done via function
  \code{\link{choice}}. \code{choice} opens a graphic windows showing
  various information including the trajectories cluterized by a specific
  \code{Clusterization}.

  When some \code{Clusterization} has been selected (the user can select
  more than 1), it is possible to
  save them. The clusters are therefore exported towards the file
  \code{nom-cluster.csv}. Criteria are exported towards
  \code{nom-criteres.csv}. The graphs are exported according to their
  extension.
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
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{kml}}, \code{\link{choice}}, \code{\link{as.clusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotCriterion]{plotCriterion)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
}

\examples{
### 1. Data Preparation
myCld <- as.clusterizLongData(generateArtificialLongData())

### 2. Building "optimal" clusterization (with only 3 redrawings)
kml(myCld,nbRedrawing=3,print.cal=TRUE,print.traj=TRUE)

### 3. Exporting results
try(choice(myCld))
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



