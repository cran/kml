\name{exportClusterization}
\Rdversion{1.1}
\alias{exportClusterization}
\alias{exportClusterization,ClusterizLongData}
\alias{exportClusterization,ClusterizLongData-method}

\title{
~ Function: exportClusterization ~
}
\description{
This function save all the information contained in a
\code{\linkS4class{Clusterization}} object.
}
\usage{
exportClusterization(object, y, typeGraph = "bmp", col = "clusters", type = "l", col.mean = "clusters", type.mean = "b", main = "", cex = 1, pch.mean = "letters", pch.time = NA, ...)
}

\arguments{
  \item{object}{[ClusterizLongData]: object containing the information
  that have to be saved}
  \item{y}{[couple(numeric)]: indicate which \code{Clusterization}
    shall be exported. The first numeric y[1] is the cluster number, the
    second y[2] is the rank of clusterization in the list y[1].}
  \item{typeGraph}{[character]: indicates the type of the graph that will
    be exported. See \link[grDevices]{savePlot} for avalaible options}
  \item{col}{[character], [numeric] or vector[numeric]: Specification of the plotting
    color of the individual trajectories. In addition to the standard
    possible values, col="clusters" can be used to color the individual
    trajectories according to their clusters.}
  \item{col.mean}{[character], [numeric] or vector[numeric]:
    Specification of the plotting color of the mean trajectories. In
    addition to the standard possible values, col="clusters" can be used
    to color each mean trajectories according to its clusters.}
  \item{main}{[character]: give the title of the graph.}
  \item{type}{[character]: what type of plot should be drawn for the
    individual trajectories ?}
  \item{type.mean}{[character]: what type of plot should be drawn for
    the mean trajectories ?}
  \item{cex}{[numeric]: fixes the size of the point on the mean trajectories.}
  \item{pch.mean}{[character]: specifies the symbol to be used as plotting
  point on the mean trajectories. Option pch.mean="symbols" or
  pch.mean="letters" can be used.}
  \item{pch.time}{[vector(numeric)]: specifies the time at which a point
  should be plot (useful if there is a important number of time, see
  \link[longitudinalData]{plot} for detail).}
  \item{\dots}{Graphical parameters to be passed to methods, see
    \code{\link[longitudinalData:plot]{plot(LongData)}} and
    \code{\link{par}} for details.}
}
\details{
  \code{exportClusterization}is mainly used by the function
  \code{\link{choice}}. It export the clusters affectation (individual
  and its letters) in a file named "objectName-Clusters.csv", the information about the clusterization
  (algorithm used, clusters' number, convergence time, percentage in each
  cluster, criterion quality name and value, imputation method and
  starting condition) in another file names "objectName-Details.csv".
  It also saves two graphs, one with the trajectories, the other one with
  the subgroups.
}
\value{
Two files and two graphs.
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





\examples{
#############
### Creating a ClusterizLongData object, with 3 cluterizations (5 clusters each)
dn <- as.cld(gald())
kml(dn,5,3)

### Exporting the second clusterization in pdf format
try(exportClusterization(dn,c(5,2),typeGraph="pdf"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
