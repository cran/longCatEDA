\name{longCat}
\alias{longCat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creation of Objects of Class longCat}
\description{Function to create objects of class \code{longCat}.}
\usage{
longCat(y, times = NULL, Labels = NULL, tLabels = NULL, id = NULL, endt = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame in wide (as opposed to long) format with cases in rows and repeated observations in columns. At most, \code{y} can have 9 unique non-missing values.}
  \item{times}{time points used for the x-axis in plotting. Either a vector of the same length as the number of columns in \code{y} (i.e., all cases have the same times of observation), or a matrix of the same dimension as \code{y} (i.e., individually varying times of observation). Note that \code{times} may have one more column than \code{y}, see \code{endt} below. Default is \code{NULL} and is assigned the value 1:\code{ncol(y)}.}  
  \item{Labels}{a vector of numeric or character labels for the response options in \code{y}. Must be the same length as the number of unique non-missing values in \code{y}. Default is \code{NULL} and is assigned the values \code{1:max(unique(y))}.}
  \item{tLabels}{numeric or character labels for the time points in \code{times}. Default is \code{NULL} and is assigned the values \code{1:ncol(y)}.}
  \item{id}{An optional variable identifying or naming the rows of \code{y}. Returned as the first column of the matrix \code{order.data} (see \code{order.data} in the value section below).}
  \item{endt}{An optional variable defining how far to extend the last time point for visualization purposes. The default is \code{NULL}, in which case the average time interval in \code{times} is used. It can take on a single value which is applied to all cases (e.g., \code{endt=1}), be set to a vector values of the same length as the number of rows in \code{y}, or be a matrix of values with the same number of rows as \code{y} and one column. If the \code{times} argument is a matrix with one more column than \code{y}, \code{endt} will be set to the last column of \code{times}.}
}
\value{
\code{longCat} returns an object of class \code{longCat} which is a list containing at least the following components:
  \item{data }{\code{y}}
  \item{data.sorted }{\code{y} sorted (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{dim }{the dimension of \code{data}.}
  \item{times }{the \code{times} object as described above.}
  \item{endt}{the \code{endt} object as described above.}
  \item{times.sorted }{if \code{times} is a matrix of the same dimension as \code{y}, \code{times.sorted} contains a matrix of individually varying times of observation with the same sorting as \code{data.sorted.}}
  \item{endt.sorted}{\code{endt} sorted after an \code{lc} object is passed to \code{\link{sorter}}}
  \item{labels }{the \code{labels} vector as described above}
  \item{tLabels }{the \code{tLabels} vector as described above}
  \item{factors }{a vector containing the unique values in \code{data}. Not that if the unique values in \code{y} were not sequential integers starting at 1, both \code{factors} and \code{data} are recoded such that they contain sequential integers starting at 1.}
  \item{IndTime }{a logical indicator of whether \code{times} is a matrix of the same dimension as \code{data}. If \code{TRUE}, \code{\link{longCatPlot}} treats these times as individually varying times of observation.}
  \item{nfactors}{the number of unique values in \code{data}, and is the same as the length of the \code{factors} vector. \code{nfactors} is determined by \code{longCat}, but cannot exceed 9. If users have data with more than 9 categories, continuous plotting methods are recommended (e.g., try \code{\link{longContPlot}}.)}
  \item{sorted }{a logical indicator of whether \code{data} has been sorted by the \code{\link{sorter}} function. If \code{TRUE}, \code{data.sorted} (and \code{times.sorted} if \code{IndTime} is \code{TRUE}) will not be \code{NULL}. }
  \item{ascending }{logical indicator. If \code{sorted} is \code{TRUE}, this will indicate whether sorting was done ascending. (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{group }{a vector of the same length as the number of rows in \code{data} (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object along with a grouping variable).}
  \item{groupLabels }{a optional vector of character or numeric labels for the \code{group} variable (see \code{\link{sorter}}).}
  \item{order.data}{A matrix with identification (see input \code{id} above) and sorting information. Rows of the matrix correspond to rows of \code{y} and the columns are \code{id}, and order variable, and a variable representing the unique data patterns in \code{y}. The former is returned only if \code{id} is provided to \code{longCat}. The latter two are only returned by {\link{sorter}}. If only unique data patterns are desired, use \code{\link{makePatterns}}; see \code{example(makePatterns)}.}
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

# object summary
summary(f2lc)

# compare growth curves to longCat
par(mfrow=c(1,2), bg='cornsilk3')
longContPlot(example2cat, times, ylim=c(1,5), 
  main='Growth Curves', ylab='', xlab='Days')
longCatPlot(f2lc, lwd=4, main='Horizontal Line Plot', colScheme='heat')
par(mfrow=c(1,1), bg='transparent')

# illustrate individually varying times of observation and the use of endt
y <- matrix(sample(1:5, 500, replace=TRUE), 100, 5)
t <- matrix(runif(600, 1, 3), 100, 6)
times <- cbind(t[,1], t[,1]+t[,2],
               t[,2]+t[,3],
               t[,3]+t[,4],
               t[,4]+t[,5])
endt <- t[,6]
lc <- longCat(y, times=times, endt=endt)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0, groupBuffer=0, 
	main='Individually Varying Times of Observation')
legend(7.5, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

\dontrun{
# illustrate handling factor input 
y <- matrix(sample(c('1', '2', '3', '4', '5'), 500, replace=TRUE), 100, 5)
lc <- longCat(y)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0)
legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

# illustrate plotting with more than 9 categories 
# (a warning is issued) and the use of endt
y <- matrix(sample(1:18, 500, replace=TRUE), 100, 5)
lc <- longCat(y, endt=.5)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0)
legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}

}

