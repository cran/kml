\name{choice}
\alias{choice}
\alias{choice-methods}
\alias{choice,ClusterizLongData-method}
%- Also NEEDS an '\alias' for EACH other topic documented here.
\title{ ~ Function: choice ~}
\description{
  \code{choice} lets the user choose some \code{Clusterization} he wants to export.
}
\usage{
choice(Object, typeGraph = "bmp",...)
}

\arguments{
  \item{Object}{\code{[ClusterizLongData]}: Object containing the trajectories and all the clusterizations found by \code{kml}
    from whom the user want to export some \code{\linkS4class{Clusterization}}.}
  \item{typeGraph}{\code{[character]} for every selected \code{clusterization}, \code{choice} export some graphs. \code{type} set
    the format that will be used. Possible formats are the one available
    for \code{\link{savePlot}}}
  \item{\dots}{  }
}


\details{
  \code{choice} is a function that lets the user see the \code{\link{Clusterization}} found by \code{\link{kml}}.
  At first, \code{choice} opens a graphics window. On the left side are the Calinski criteria of all the \code{Clusterization} contained in Object.
  One \code{Clusterization} is 'active', it is the one marked by a black dot.
  On the right side, the trajectories of Object are drawn, according to the active \code{Clusterization}.

  From there, \code{choice} offers numerous options :
  \describe{
    \item{Arrow}{Change the active \code{Clusterization}.}
    \item{Space}{Select/unselect a \code{Clusterization} (the selected
  \code{Clusterization} are surrounded by a circle).}
    \item{Return}{Export all the selected \code{Clusterization}, then
      quit the function \code{choice}.}
    \item{'c'}{Switch the graphical representation of the Calinski
      criterions on/off.}
    \item{'e'}{Switch the graphical representation of the trajectories
      on/off (see \code{\link{plot}} for details). }
    \item{'d'}{Switch the graphical representation of the trajectories' subgroups
      on/off (see \code{\link[longitudinalData]{plotSubGroups}} for details). }
    \item{'r'}{Change the trajectories color.}
    \item{'f'}{Change the trajectories' subgroups color.}
    \item{'t'}{Change the mean trajectories color.}
    \item{'g'}{Change the mean trajectories' subgroups color.}
    \item{'y'}{Change the mean trajectories symbols.}
    \item{'h'}{Change the mean trajectories' subgroups symbols.}
    \item{'u'}{Increase the size of the mean trajectories symbols.}
    \item{'j'}{Increase the size of the mean trajectories subgroups symbols.}
    \item{'i'}{Decrease the size of the mean trajectories symbols.}
    \item{'k'}{Decrease the size of the mean trajectories subgroups symbols.}
  }
  Note that the letter on the upper line (e,r,t,y,u,i) concerns the
  main graph, the letter on the lower line (d,f,g,h,j,k) concerns
  the subgroups.  ; the letter on the first column (e,d,c) switches the
  graphic representation on/off, the second and third columns
  (r,f,t,g) change
  the color and the other columns (y,h,u,j,i,k) deal with the symbols.

  When 'return' is pressed, the selected \code{Clusterization} are
  exported. Exporting is done in a specific folder named
  \code{objectName-CnumberOfCluster-OrderInTheSublist}. Four files are created
  :
  \describe{
    \item{name-Clusters.csv}{Table with two columns. The first is the
      identifier of each trajectory ; the second holds the cluster's
      affectation of the trajectory.}
    \item{name-Detail.csv}{Table containing information about the
      clusterization (the Calinski criterion and the percentage of individual
      in each cluster)}
    \item{name-Traj.ext}{Graph (of type 'ext') representing the
      trajectories. All the parameters set during the visualization (color
      of the trajectories, symbols used, mean color) are used
      for the export.}
    \item{name-SubGroups.ext}{Graph (of type 'ext') representing the
      trajectories subgroups. All the parameters set during the visualization (color
      of the trajectories, symbols used, mean color) are used
      for the export.}
  }
}

\value{For each selected \code{Clusterization}, a folder containing four files.}



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




\references{Article submited \cr
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
  Methods : \code{\link{kml}}\cr
  Plot : \code{\link[kml:plot]{plot(ClusterizLongData)}},
  \code{\link[=plotCriterion]{plotCriterion}}
}


\examples{
### Creation of articficial data
cld1 <- as.cld(gald())

### Clusterisation
#kml(cld1,nbRedrawing=3,printCal=TRUE,printTraj=TRUE)

### Selection of the clusterization we want
#     (note that "try" is for compatibility with CRAN only,
#     you probably can use "choice(cld1)")
try(choice(cld1))
}

\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
