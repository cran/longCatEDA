\name{longCat}
\alias{longCat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creation of Objects of Class longCat}
\description{Function to create objects of class \code{longCat}.}
\usage{
longCat(y, times = NULL, Labels = NULL, tLabels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame in wide (as opposed to long) format with cases in rows and repeated observations in columns. At most, \code{y} can have 9 unique non-missing values.}
  \item{times}{time points used for the x-axis in plotting. Either a vector of the same length as the number of columns in \code{y} (i.e., all cases have the same times of observation), or a matrix of the same dimension as \code{y} (i.e., individually varying times of observation). Default is \code{NULL} and is assigned the value 1:\code{ncol(y)}.}  
  \item{Labels}{a vector of numeric or character labels for the response options in \code{y}. Must be the same length as the number of unique non-missing values in \code{y}. Default is \code{NULL} and is assigned the values \code{1:max(unique(y))}.}
  \item{tLabels}{numeric or character labels for the time points in \code{times}. Default is \code{NULL} and is assigned the values \code{1:ncol(y)}.}
}
\value{
\code{longCat} returns an object of class \code{longCat} which is a list containing at least the following components:
  \item{data }{\code{y}}
  \item{data.sorted }{\code{y} sorted (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{dim }{the dimension of \code{data}.}
  \item{times }{the \code{times} object as described above.}
  \item{times.sorted }{if \code{times} is a matrix of the same dimension as \code{y}, \code{times.sorted} contains a matrix of individually varying times of observation with the same sorting as \code{data.sorted.}}
  \item{labels }{the \code{labels} vector as described above}
  \item{tLabels }{the \code{tLabels} vector as described above}
  \item{factors }{a vector containing the unique values in \code{data}. Not that if the unique values in \code{y} were not sequential integers starting at 1, both \code{factors} and \code{data} are recoded such that they contain sequential integers starting at 1.}
  \item{IndTime }{a logical indicator of whether \code{times} is a matrix of the same dimension as \code{data}. If \code{TRUE}, \code{\link{longCatPlot}} treats these times as individually varying times of observation.}
  \item{nfactors}{the number of unique values in \code{data}, and is the same as the length of the \code{factors} vector. \code{nfactors} is determined by \code{longCat}, but cannot exceed 9. If users have data with more than 9 categories, continuous plotting methods are recommended (e.g., try \code{\link{longContPlot}}.)}
  \item{sorted }{a logical indicator of whether \code{data} has been sorted by the \code{\link{sorter}} function. If \code{TRUE}, \code{data.sorted} (and \code{times.sorted} if \code{IndTime} is \code{TRUE}) will not be \code{NULL}. }
  \item{ascending }{logical indicator. If \code{sorted} is \code{TRUE}, this will indicate whether sorting was done ascending. (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{group }{a vector of the same length as the number of rows in \code{data} (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object along with a grouping variable).}
  \item{groupLabels }{a optional vector of character or numeric labels for the \code{group} variable (see \code{\link{sorter}}).}

%% ...
}
\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}

\seealso{
\code{\link{longCatPlot}} to plot \code{longCat} objects created by the \code{\link{longCat}} function.
}
\examples{
# create the longcat object for Figure 2 in Tueller (2011)
times <- c(1,100,200,300,400,500)
f2lc <- longCat(example2cat, times)
# see the object
summary(f2lc)
# plot the continuous and categogorical versions of this data
par(mfrow=c(1,2), bg='lightblue')
longContPlot(example2cat, times, ylim=c(1,5), main='A', ylab='', xlab='Day')
longCatPlot(f2lc, lwd=4, main='B', colScheme='heat')
par(mfrow=c(1,1), bg='white')
}

