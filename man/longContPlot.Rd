\name{longContPlot}
\alias{longContPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Continuous Longitudinal Data
}
\description{
Function to plot continuous longitudinal or time-series data.
}
\usage{
longContPlot(y, times = NULL, ylim = NULL, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame in wide (as opposed to long) format with cases in rows and repeated observations in columns.}
  \item{times}{time points used for the x-axis in plotting. Either a vector of the same length as the number of columns in \code{y} (i.e., all cases have the same times of observation), or a matrix of the same dimension as \code{y} (i.e., individually varying times of observation). Default is \code{NULL} and is assigned the value 1:\code{ncol(y)}.}  
  \item{ylim}{
see \code{\link{par}}. Default is \code{NULL} and calculated from \code{y}.
}
  \item{xlim}{
see \code{\link{par}}. Default is \code{NULL} and calculated from \code{y}.
}
  \item{\dots}{
Arguments to be passed to \code{\link{plot}}. See \code{\link{par}}.
}
}

\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}

\seealso{
\code{\link{longCatPlot}}.
}
\examples{
# longitudinal plot
times <- c(1,100,200,300,400,500)
longContPlot(example2cont, times, ylim=c(-2,6), main='', ylab='', xlab='Day')
}
