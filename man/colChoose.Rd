\name{colChoose}
\alias{colChoose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal Function for Selecting Color Schemes Used by longCatPlot 
}
\description{
Internal function used by \code{\link{longCatPlot}}.
}
\usage{
colChoose(colScheme, nfactors, reverse = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colScheme}{
can be one of
       \itemize{
		\item 0 = 1:8 + 'darkgreen' (9==1, so you can't use 1:9)
		\item 1 = roygbiv
		\item 2 = orange to blue
		\item 3 = green to red
		\item 'gray' = a spectrum of greys
		\item 'oldheat' = a manual heat map spectrum
		\item 'coldheat' = a manual heat map starting with cold colors
		\item 'rainbow' see \code{\link{rainbow}}
		\item 'heat' see \code{\link{heat.colors}}
		\item 'terrain' see \code{\link{terrain.colors}}
		\item 'topo' see \code{\link{topo.colors}}
		\item 'cm' see \code{\link{cm.colors}}
       }
No default is given in the function definition, but the default in \code{\link{longCatPlot}} passed to \code{colChoose} is \code{'heat'}.
}
  \item{nfactors}{
see \code{nfactors} in values returned by \code{\link{longCat}}.
}
  \item{reverse}{
logical - should color scheme be applied in reverse order to the levels of the categorical variable? Default is \code{FALSE}. See \code{reverse} input to \code{\link{longCatPlot}}.
}
}

\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\author{Stephen Tueller}


\seealso{
\code{\link{longCatPlot}}.
}
\examples{
# color examples
par(mfrow=c(3,4), bg='wheat')
times <- c(1,100,200,300,400,500)
f3lc <- longCat( example3, times, Labels=rep('',5) )
longCatPlot(f3lc, main='colScheme=0', colScheme=0, lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=1', colScheme=1, lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=2', colScheme=2, lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=3', colScheme=3, lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=gray', colScheme='gray', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=oldheat', colScheme='oldheat', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=coldheat', colScheme='coldheat', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=rainbow', colScheme='rainbow', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=heat', colScheme='heat', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=terrain', colScheme='terrain', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=topo', colScheme='topo', lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=cm', colScheme='cm', lwd=.1, ylab='', legendBuffer = .25)
par(mfrow=c(1,1), bg='white')
}
