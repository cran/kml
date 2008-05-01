\name{kml-package}
\alias{kml-package}
\docType{package}
\title{~ Overview: K-means for Longitudinal data ~}
}
\description{
  KmL is a non parametric algorithm for clustering longitudinal
  data. Here is an overview of the package. For the description of the
  algorithm, see \code{\link{kml}}.
}
\details{
  \tabular{ll}{
    Package: \tab kml\cr
    Type: \tab Package\cr
    Version: \tab 0.9.0\cr
    Date: \tab 2008-05-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,codetools,clv\cr
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
  This is done via function
  \code{\link[=as.clusterizLongData]{as.cld}}. This function deals with
  \code{matrix} (see \code{\link{as.clusterizLongData.data.frame}}) and
  \code{data.frame} (see \code{\link{as.clusterizLongData.matrix}}).

  Instead of working on real data, one can also work on artificial
  data. Such data will be of the \code{\link{ArtificialLongData}} type. It is
  then possible to turn them into \code{ClusterizLongData} using
  function \code{as.cld} for \code{ArtificialLongData} (see \code{\link{as.cld.artificialLongData}}).
}

\section{2. Building "optimal" clusterization}{
  Once the object \code{ClusterizLongData} has been created, the algorithm
  \code{\link[=kml]{KmL}} can be executed.

  Starting with a \code{ClusterizLongData}, \code{kml} built a \code{\linkS4class{Clusterization}}.
  A object of class \code{Clusterization} is a partition of trajectories
  into subgroups. The object also contains a information as the percentage of trajectories contained in each group or the Calinski criterion.\cr

  \code{kml} is a "hill-climbing" algorhithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Clusterization}, it is better to execute the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Clusterization} maximising the determinant of the matrix between.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate clues that will enable us to choose. \code{kml} uses the \code{Calinski criterion}.

  In the end, \code{kml} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{kml} has constructed a certain number of
  \code{Clusterization}, the user can examine them one by one and choose
  the one that suits
  them best. This can be done via function \code{\link{choice}}. \code{choice} opens two graphic windows.
  The one on the left shows all of Calinski criterion calculated for each \code{Clusterization} ;
  the one on the right shows the slected \code{Clusterization}. The arrows on keyboard enable to move from one \code{Clusterization} to another.

  When a \code{Clusterization} has been chosen, it is possible to put it
  on the screen, to save it in memory or to export it towards a file. The clusters are therefore exported towards the file
  \code{nom-cluster.csv}. Criteria are exported towards \code{nom-criteres.csv}. The distances and posterior probability are in
  \code{nom-distance.csv} (in preparation, non implemented for the time being).

  Last but not least, it is possible to exporte a graphic representation
  of clusters. With the keyboard, it is possible to the aspect of the graphic (black and white or color, presence or absence of trajectories, sub groups, size of fonts...). The final graphic
can be exported in the usual manner : right click on the figure...
}

\references{Article submited\cr
web site: \code{http://christophe.genolini.free.fr/kml}
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

### 2. Building "optimal" clusterization (with only 5 redrawings)
#kml(myCld,,5,print="all")

### 3. Exporting results
#choice(myCld)
}
