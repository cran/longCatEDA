\name{sorter}
\alias{sorter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
General Sorting Function
}
\description{
A function to sort an \code{\link{longCat}} object created by \code{\link{longCat}}. \code{sorter} must be used directly when stratified plots of subgroups is desired, or when sorting other than the default sorting is desired. Otherwise, \code{sorter} is used internally with the defaults by \code{\link{longCatPlot}} if \code{lc$sorted=FALSE}. If an object has already been sorted (\code{lc$sort=TRUE}), \code{sorter} will not resort it, but will print a code example of how to use multiple sortings.
}
\usage{
sorter(lc, ascending = TRUE, whichColumns = NULL, num = TRUE,
                 mindur = NULL, igrpt = FALSE, customSort = NULL,
                 initFirst = FALSE, group = NULL, groupLabels = NULL,
                 ggap = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lc}{
an object of class \code{\link{longCat}} created by \code{\link{longCat}}.
}
  \item{ascending}{
logical - should sorting be done ascending. Default is \code{TRUE}.
}
  \item{whichColumns}{
a numeric list indicating which columns in \code{lc$data} should be used for sorting (.e.g., \code{c(1, 5, 7)}). Useful if, for example, an intervention occurs after data collection has started, and the user is not interested in sorting on pre-intervention observations.
}
  \item{num}{
see \code{\link{makePatterns}} for details.
}
  \item{mindur}{
see \code{\link{makePatterns}}.
}
  \item{igrpt}{should \code{sorter} (ig)nore (r)e(p)ea(t)ed values for each row in \code{lc$data} for sorting purposes? See \code{\link{norpt}}.}
  \item{customSort}{
a vector of the same length as the number of rows in \code{lc$data} providing a user defined variable on which to sort the data prior to secondarily applying the default sort. If \code{group} is not \code{NULL}, group will be sorted on prior to the \code{customSort} variable. Alternatively, \code{lc$data} can be sorted without calling \code{sorter} using \code{lc$data.sorted <- lc$data[o, ]} where \code{o} is the \code{\link{order}} (e.g., use \code{o <- \link{order}(customSort)}). The user must also set \code{lc$sorted <- TRUE} to prevent on-the-fly default sorting from being carried out by \code{\link{longCatPlot}}. Users unfamiliar with sorting in \code{R} should take care not to confuse \code{\link{order}} with \code{\link{sort}}. Default is \code{NULL}. If any values on \code{customSort} are missing, the function will return an error message.
}
  \item{initFirst}{if \code{customSort} is not \code{NULL}, setting \code{initFirst=TRUE} will sort on initial values prior to the custom sorting variable.}
  \item{group}{
a vector of the same length as the number of rows in \code{lc$data} indicating group membership. Default is \code{NULL}. If \code{group} is \code{NA}, corresponding rows in \code{lc$data} will be deleted prior to completing the sorting, and a warning indicating this has been done is printed to the console. If a large number of cases have missing data on the grouping variable, consider recoding the missings into their own group, e.g., \code{group[is.na(group)] <- -999} and add a missing label to \code{groupLabels}, e.g. \code{groupLabels=c('Missing', 'Group1', 'Group2', 'Etc.')}.
}
  \item{groupLabels}{
a vector of numeric or character labels of the same length as the number of unique values in \code{group}. Default is \code{NULL}. If \code{group} is not \code{NULL} and \code{groupLabels} is not provided, then the numeric values in \code{group} are used as the labels.
}
  \item{ggap}{
a number zero or greater. The number blank rows to be plotted between groups when \code{group} is specified. As the number of rows in \code{lc$\%data} becomes large, \code{ggap} will also need to become larger than the default of 10 to ensure sufficient visual separation of groups in plots produced by \code{\link{longCatPlot}}.
}
}


\value{
Returns an object of class \code{longCat} where \code{lc$sorted=TRUE}. See \code{\link{longCat}} for values.
}
\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{
Stephen Tueller
}

\seealso{
\code{\link{longCat}} and \code{\link{longCatPlot}}.
}
\examples{
# create a plot like that in Figure 3 from Tueller, Van Dorn, & Bobashev (2013)
par(mfrow=c(1,2), bg='cornsilk3')
times <- c(1,100,200,300,400,500)
f3lc <- longCat(example3, times); f3lc$sorted <- TRUE; f3lc$data.sorted <- f3lc$data
longCatPlot(f3lc, main='Unsorted', colScheme='heat', lwd=2)
f3lc <- longCat(example3, times)
longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2)

# sort with a grouping variable and plot
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
times <- c(1,100,200,300,400,500)
lc <- longCat(example3, times)
group <- sample(1:3, nrow(example3), replace=TRUE)
grouplc <- sorter(lc, group=group)
cols <- longCatPlot(grouplc, groupBuffer=.15, main='Grouped Data', colScheme='heat', 
                    lwd=2, legendBuffer=0)
legend(600, 130, legend=1:5, col=cols, lty=1, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}
