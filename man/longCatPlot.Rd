\name{longCatPlot}
\alias{longCatPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of lc objects
}
\description{
Function to plot \code{longCat} objects created by \code{\link{longCat}}. Note that depending on your 
}
\usage{
longCatPlot(lc, xlab = "Day", ylab = "Each Line Represents a Participant", 
                cols = NULL, colScheme = "heat", reverse = FALSE, 
                lwd = 0.5, lcex = 1, llwd = 3, legendBuffer = 0.12, 
                groupBuffer = 0.1, groupRotation = 90, gcex = 1, 
                bg = "cornsilk3", seg.len = 1,
                xlas = 0, xcex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lc}{
an object of class \code{\link{longCat}} created by \code{\link{longCat}}. See \code{\link{par}}.
}
  \item{xlab}{
a label for the x-axis. Default is "Day". See \code{\link{par}}.
}
  \item{ylab}{
a label for the y-axis. Default is "Each Line Represents a Participant". See \code{\link{par}}.
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
set proportion of the plot to retain for legends, must be in [0,1]. Note that the legend is very sensitive to the scaling of the graphics device. Users are advised to maximize their device and rerun, or call \code{dev.new()} and resize prior to running \code{longCatPlot}. Default is .12 (i.e., 12\% of the vertical plot area is retained for the legend).
}
  \item{groupBuffer}{
similar to legendBuffer, but for group labels on the left side of the plot. Default is .1 (i.e., 10\% of the horizontal plot area is retained for group labels). Can take on any value in [0,1]. 
}
  \item{groupRotation}{
if \code{lc$groupLabels} are long (see \code{\link{longCat}}), rotation of the labels can be used reduce the needed size of \code{groupBuffer}.
}
  \item{gcex}{character expansion factor for group labels. Default is 1. See\code{\link{par}}.
}
  \item{bg}{a background color for plots. Default is \code{'cornsilk3'}. See \code{bg} in \code{\link{par}}.}
  \item{seg.len}{Length of lines in the upper legend. Default is 1. See \code{\link{legend}}.}
  \item{xlas}{see \code{las} in \code{\link{par}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{xcex}{see \code{axis.cex} in \code{\link{par}} and  \code{\link{axis}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{\dots}{
Arguments to be passes to \code{\link{plot}} (see \code{\link{par}})
}
}
\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}

\seealso{
\code{\link{longCat}}.
}
\examples{
# create a plot like that in Figure 3 from Tueller, Van Dorn, & Bobashev (2013)
\dontshow{par(mfrow=c(1,2), bg='lightblue')}
times <- c(1,100,200,300,400,500)
f3lc <- longCat(example3, times); f3lc$sorted <- TRUE; f3lc$data.sorted <- f3lc$data
longCatPlot(f3lc, main='Unsorted', colScheme='heat', lwd=2)
f3lc <- longCat(example3, times)
longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2)
\dontshow{par(mfrow=c(1,1), bg='white')}
\dontrun{
# for data sets with many rows, writing directly to a file 
# is much faster and unaffected by device resizing, see ?pdf
pdf('C:/mydir/mysubdir/myfile.pdf')
longCatPolt(f3lc, main='Sorted', colScheme='heat', lwd=2))
dev.off()
# see ?jpeg for picture file options
}
}
