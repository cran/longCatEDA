\name{longCatEDA-package}
\alias{longCatEDA-package}
\alias{longCatEDA}
\docType{package}
\title{
Plot Categorical Longitudinal and Time-Series Data
}
\description{
Package to implement horizontal line plots for categorical (ordinal or nominal) longitudinal or time-series data, where categorical states are differentiated using color or shade.
}
\details{
\tabular{ll}{
Package: \tab longCatEDA\cr
Type: \tab Package\cr
Version: \tab 0.16\cr
Date: \tab 2012-11-05\cr
License: \tab GNU GPL\cr
Depends: \tab methods\cr
}
Example usage is \code{\link{longCatPlot}}(\code{\link{longCat}}(y)) where \code{y} is a matrix or data frame in wide format with participants in rows and repeated observations in columns. The function \code{\link{longCat}} returns an object of class \code{longCat} which can be plotted using \code{\link{longCatPlot}}. Options for sorting and/or stratifying by groups are implemented using \code{\link{sorter}} which returns a sorted and/or stratified object of class \code{longCat}. 
}
\author{
Stephen Tueller

Maintainer: Stephen Tueller <stueller@rti.org>
}
\references{Tueller, S. J. (2011). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\keyword{ package }
\examples{
longCatPlot( longCat( example3 ) )
}
