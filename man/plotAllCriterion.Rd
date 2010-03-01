\name{plotAllCriterion}
\Rdversion{1.1}
\alias{plotAllCriterion}
\alias{plotAllCriterion-method}
\alias{plotAllCriterion,ClusterizLongData}
\alias{plotAllCriterion,ClusterizLongData-method}
\title{
~ Function: plotAllCriterion ~
}
\description{
  This function graphically displays three quality criterions for each
  possible cluster number.ll the
  \code{\linkS4class{Clusterization}} of a \code{\linkS4class{ClusterizLongData}} object.
}
\usage{
plotAllCriterion(x,method="linearInterpolation")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData]: object whose quality criterion should be
    displayed.}
  \item{method}{[character]: imputation methode that should be used in
    case of missing values. See
    \code{\link[longitudinalData]{imputation}} for detail.}
}

\details{
  This function display graphically three quality criterion (probably to
  decide the best clusters' number). The three criterions are Calinski &
  Harabatz, Ray & Turi and Davies & Bouldin.
  It displays only the best result for each clusters number : this helps to
  find the local maximum, which is classically used to chose the
  "correct" clusters' number.
}

\value{
  No value are return. A graph is printed.
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
#################
### Data generation
dn <- as.cld(gald())

### Trying several clusters number and several starting condition
kml(dn)

### Display the quality criterion, both way :
plotAllCriterion(dn)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
