\name{dimCheck}
\alias{dimCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Check the Dimensions of Inputs to longCat
}
\description{
Function for checking the dimensions of \code{y} and \code{times} and returning a logical indicator \code{indTime} of whether individually varying times of observation were detected. Used internally by \code{\link{longCat}}, though it may be called by the user.
}
\usage{
dimCheck(y, times)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame in wide (as opposed to long) format with cases in rows and repeated observations in columns. At most, \code{y} can have 9 unique non-missing values.}
  \item{times}{time points used for the x-axis in plotting. Either a vector of the same length as the number of columns in \code{y} (i.e., all cases have the same times of observation), or a matrix of the same dimension as \code{y} (i.e., individually varying times of observation). Default is \code{NULL} and is assigned the value 1:\code{ncol(y)}.}
}

\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}

\seealso{
\code{\link{longCat}}.
}
