\name{plotAll,ClusterizLongData}
\alias{plotAll}
\alias{plotAll,ClusterizLongData}
\alias{plotAll,ClusterizLongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: plotAll for ClusterizLongData ~ }
\description{
  \code{plot} the trajectories, the quality's criteria and the
  subgroups of an object \code{\linkS4class{ClusterizLongData}} on a
  single graph.
}
\usage{
\S4method{plotAll}{ClusterizLongData}(x, y, print.cal = TRUE, print.traj = TRUE,
        print.sub = FALSE, allCrit = TRUE, nbCriterion = 100, col = 1,
        type = "l", col.mean = "clusters", type.mean = "b", main = "",
        size = 1, ylim = NA,
        col.sub = 1, type.sub = "l", col.mean.sub = "clusters",
        type.mean.sub = "b", main.sub = "", size.sub = 1, ylim.sub = NA,
        ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData] Object containing the trajectories to plot.}
  \item{y}{[numeric] or [vector(numeric)] Give the \code{Clusterization}
    to represent. If \code{y} is missing, the \code{Clusterization} with
    the highest quality criterion is selected. If \code{y} is a number,
    the first \code{Clusterization} of the sublist \code{c-y} is
    selected. If \code{y} is a couple of numeric, the \code{y[2]}th
    \code{Clusterization} of the sublist \code{c-y[1]} is selected.}
  \item{print.cal}{[logical]: if TRUE, the quality criterion are printed.}
  \item{print.traj}{[logical]: if TRUE, a "main" graph, showing all the
    trajectories at once, is printed.}
  \item{print.sub}{[logical]: if TRUE, several sub-graphs, each containing
    a specific cluster of trajetories, are printed.}
  \item{allCrit}{[logical] shall all the quality criterion be display, or
    only the best one for each clusters' number ?}
  \item{nbCriterion}{[numeric]: gives an upper limit to the number of quality
    criterion to print.}
  \item{col}{[character], [numeric] or vector[numeric]: Specification of the ploting
    color of the individual trajectories. In addition to the standard
    possibles values, col="clusters" can be use to color the individual
    trajectories according to their clusters.}
  \item{col.mean}{[character], [numeric] or vector[numeric]:
    Specification of the plotting color of the mean trajectories. In
    addition to the standard possible values, col="clusters" can be used
    to color each mean trajectories according to its clusters.}
  \item{type}{[character]: what type of plot should be drawn for the individual trajectories.}
  \item{type.mean}{[character]: what type of plot should be drawn for
    the mean trajectories ?}
  \item{main}{[charater]: give the title of the graph.}
  \item{size}{[numeric]: If some point are added on the mean trajectories, \code{size} fixe the
    size of the point.}
  \item{ylim}{[couple(numeric)]: fixe the ylim parameter.}

  \item{col.sub}{[character], [numeric] or vector[numeric]: Specification of the ploting
    color of the individual trajectories on the sub-groups graph. In addition to the standard
    possible values, col="clusters" can be used to color the individual
    trajectories according to their clusters.}
  \item{type.sub}{[character]: what type of plot should be drawn for the
    individual trajectories on the sub-groups graph.}
  \item{col.mean.sub}{[character], [numeric] or vector[numeric]:
    Specification of the ploting color of the mean trajectories on the sub-groups graph. In
    addition to the standard possible values, col="clusters" can be used
    to color each mean trajectories according to its clusters.}
  \item{type.mean.sub}{[character]: what type of plot should be drawn for
    the mean trajectories on the sub-groups graph ?}
  \item{main.sub}{[charater]: gives the title of the sub-groups graph.}
  \item{size.sub}{[numeric]: If some point are added on the mean trajectories, \code{size} sets the
    size of the point on the sub-groups graph.}
  \item{ylim.sub}{[couple(numeric)]: sets the ylim parameter for the sub-groups graph.}

  \item{\dots}{Graphical parameters to be passed to methods, see
    \code{\link[longitudinalData:plot]{plot(LongData)}} and
    \code{\link{par}} for details.}
}


\details{
  For every \code{clusterizLongData} object, there is three possible
  graphical representation: quality criterion, all the trajectories at once
  and trajectories clusters by clusters. This function combines these three
  graphic outputs (or only two, or one, according to the three arguments \code{print.cal}, \code{print.traj} and
  \code{print.sub}). Thus every option (like \code{col.mean},...) are
  duplicated : one is for the main graph (\code{col.mean}), the second is for
  the sub-graphs (\code{col.mean.sub}).
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
  Classes : \code{\linkS4class{ClusterizLongData}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotCriterion]{plotCriterion}}, \code{\link[=plotSubGroups]{plotSubGroups(ClusterizLongData)}}
}

\examples{
##################
### Data construction
dn <- as.cld(gald())
kml(dn,2:5,5)
kml(dn,16,5)
par(ask=TRUE)

### Default ploting
plotAll(dn)
plotAll(dn,legend=FALSE)

### Only the calinski criterion (same effect than plotCalinski(ld))
plotAll(dn,print.cal=TRUE,print.traj=FALSE,print.sub=FALSE)

### Groups and sub-groups
plotAll(dn,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE,legend=FALSE)
plotAll(dn,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE,col="black",type.mean="n",legend=FALSE)

### All at once
plotAll(dn,print.cal=TRUE,print.traj=TRUE,print.sub=TRUE)

### To see the clusterization with only 2 clusters
plotAll(dn,2,print.cal=FALSE,print.traj=TRUE,print.sub=TRUE)

### To see the third clusterization with 16 clusters
plotAll(dn,c(16,3),print.cal=FALSE,type.mean="b",type="n")

par(ask=FALSE)
}

\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series


