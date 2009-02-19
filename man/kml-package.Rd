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
    Version: \tab 0.9.1\cr
    Date: \tab 2009-01-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,clv\cr
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
  \code{KmL} works on object of class \code{\linkS4class{ClusterizLongData}} (abreviated \code{cld}).
  Data preparation therfore simply consists in tranforming data into an object \code{ClusterizLongData}.
  This can be done via function
  \code{\link[=clusterizLongData]{clusterizLongData}}
  (\code{\link[=clusterizLongData]{cld}} in short) or
  \code{\link[=as.clusterizLongData]{as.clusterizLongData}}
  (\code{\link[=as.clusterizLongData]{as.cld}} in short).
  The formers let the user build some data from scratch, the latters
  convert a \code{data.frame} in \code{\linkS4class{ClusterizLongData}}.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with
  \code{\link[=generateArtificialLongData]{generateArtificialLongData}}
  (\code{\link[=generateArtificialLongData]{gald}} in short). The resulting data
  will be of class \code{\linkS4class{ArtificialLongData}}
  which is a subclass of \code{\linkS4class{ClusterizLongData}}.
}

\section{2. Building "optimal" clusterization}{
  Once an object of class \code{\linkS4class{ClusterizLongData}} has been created, the algorithm
  \code{\link[=kml]{kml}} can be run.

  Starting with a \code{ClusterizLongData}, \code{kml} built a \code{\linkS4class{Clusterization}}.
  A object of class \code{Clusterization} is a partition of trajectories
  into subgroups that also contains some information like the
  percentage of trajectories contained in each group or the Calinski &
  Harabasz criterion.\cr

  \code{kml} is a "hill-climbing" algorhithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Clusterization}, it is better to execute the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Clusterization} maximising the determinant of the matrix between.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate
  clues that will enable us to choose. \code{kml} uses the Calinski &
  Harabasz criterion.

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
  extention.
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
  Classes : \code{\linkS4class{ClusterizLongData}}, \code{\linkS4class{Clusterization}}, \code{\linkS4class{ArtificialLongData}} \cr
  Methods : \code{\link{clusterizLongData}}, \code{\link{kml}}, \code{\link{generateArtificialLongData}}, \code{\link{choice}}, \code{\link{as.clusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot: overview}}, \code{\link[=plotTraj]{plot(ClusterizLongData)}},
  \code{\link[=plotCalinski]{plot(Calinski)}},
  \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}, \code{\link[=plotAll]{plotAll(ClusterizLongData)}}
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



\examples{
### 1. Data Preparation
myCld <- as.clusterizLongData(generateArtificialLongData())

### 2. Building "optimal" clusterization (with only 3 redrawings)
kml(myCld,nbRedrawing=3,printCal=TRUE,printTraj=TRUE)

### 3. Exporting results
try(choice(myCld))
}

