\name{plotCriterion}
\Rdversion{1.1}
\alias{plotCriterion}
\alias{plotCriterion-method}
\alias{plotCriterion,ClusterizLongData}
\alias{plotCriterion,ClusterizLongData-method}
\title{
~ Function: plotCriterion ~
}
\description{
  This function graphicaly display the quality criterion of all the
  \code{\linkS4class{Clusterization}} of a \code{\linkS4class{ClusterizLongData}} object.
}
\usage{
plotCriterion(x, nbCriterion = 100, allCrit = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterizLongData]: object whose quality criterion should be displayed.}
  \item{nbCriterion}{[numeric]: if there is a big number of
    \code{Clusterization}, the graphical display of all of them can be
    slow. \code{nbCriterion} let the user limit the number of criterion
    that will be take in account.}
  \item{allCrit}{[logical]: if TRUE, up to \code{nbCriterion} for each
    possible clusters number are considered. If \code{FALSE}, only
    the best criterion for each possible clusters number are displayed.}
}

\details{
  This function display graphicaly the quality criterion (probably to
  decide the best clusters' number). It can either display all the
  criterion ; this is usefull to see the consistancy of the result : is
  the best clusterization obtain several time or only one ? It can also
  display only the best result for each clusters number : this help to
  find the local maximum, which is classicaly used to chose the
  "correct" clusters' number.
}

\value{
  No value are return. A graph is printed.
}
\examples{
#################
### Data generation
dn <- as.cld(gald())

### Trying several clusters number and several starting condition
kml(dn)

### Display the quality criterion, both way :
par(mfrow=c(1,2))
plotCriterion(dn)
plotCriterion(dn,allCrit=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
