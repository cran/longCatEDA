\name{longCatPlot}
\alias{longCatPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of lc objects
}
\description{
Function to plot \code{longCat} objects created by \code{\link{longCat}}.  
}
\usage{
longCatPlot(lc, xlab = "Days", ylab = NULL, cols = NULL, colScheme = "heat", 
			reverse = FALSE, lwd = 0.5, lcex = 1, llwd = 3, 
			legendBuffer = 0.12, groupBuffer = 0, groupRotation = 90, 
			gcex = 1, seg.len = 1, xlas = 0, xcex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lc}{
an object of class \code{\link{longCat}} created by \code{\link{longCat}}. See \code{\link{par}}.
}
  \item{xlab}{
a label for the x-axis. Default is "Days". See \code{\link{par}}.
}
  \item{ylab}{
a label for the y-axis. Default is \code{NULL} which is changed to "Each Line Represents a Participant" with the sample size appended (\code{lc$dim[1]}, see \code{\link{longCat}}). See \code{\link{par}}.
}
  \item{cols}{
a numeric or character list of colors. See \code{\link{par}}. Default is \code{NULL}. To use internal color schemes, use \code{colScheme}.
}
  \item{colScheme}{
select a color scheme. See \code{\link{colChoose}} for available options.
}
  \item{reverse}{
color schemes are applied from the lowest to highest level of categorical data in \code{lc$data} or \code{lc$data.sorted}. Set \code{reverse=TRUE} to reverse this. Default is \code{FALSE}.
}
  \item{lwd}{
set the width of horizontal lines. Default is .5. \code{lwd} should be reduced proportionally to the number of rows in \code{lc$data} to avoid overlap in plotting. 
}
  \item{lcex}{
character expansion factor for the legend text. Default is 1. See \code{\link{par}}.
}
  \item{llwd}{
set the width of lines in the legend. default is 3. See \code{lwd}.
}
  \item{legendBuffer}{
set proportion of the plot to retain for legends, must be in [0,1]. Note that the legend is very sensitive to the scaling of the graphics device. Users are advised to maximize their device and rerun, or call \code{dev.new()} and resize prior to running \code{longCatPlot}. Default is .12 (i.e., 12\% of the vertical plot area is retained for the legend). Set to 0 if no legend is desired. See the examples for moving the legend outside of the plotting margins.
}
  \item{groupBuffer}{
similar to legendBuffer, but for group labels on the left side of the plot. Default is 0 (i.e., 0\% of the horizontal plot area is retained for group labels). Can take on any value in [0,1]. 
}
  \item{groupRotation}{
if \code{lc$groupLabels} are long (see \code{\link{longCat}}), rotation of the labels can be used reduce the needed size of \code{groupBuffer}.
}
  \item{gcex}{character expansion factor for group labels. Default is 1. See\code{\link{par}}.
}
  \item{seg.len}{Length of lines in the upper legend. Default is 1. See \code{\link{legend}}.}
  \item{xlas}{see \code{las} in \code{\link{par}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{xcex}{see \code{axis.cex} in \code{\link{par}} and  \code{\link{axis}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{\dots}{
Arguments to be passed to \code{\link{plot}} (see \code{\link{par}}).
}
}
\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V. (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}

\seealso{
\code{\link{longCat}}.
}
\examples{
# Illustrate longCatPlot with the legend outside the plot
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(
  longCat(example3),
  legendBuffer=0,
  main='Horizontal Line Plot')
legend(7.1, 100, legend=1:5, col=cols, lty=1, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

\dontrun{
# for data sets with many rows, writing directly to a file 
# is much faster and unaffected by device resizing, see ?pdf
pdf('C:/mydir/mysubdir/myfile.pdf')
par(bg='cornsilk3')
longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2)
par(mfrow=c(1,1), bg='transparent')
dev.off()
# see ?jpeg for picture file options
}
}
