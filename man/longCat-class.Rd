\name{longCat-class}
\Rdversion{0.16}
\docType{class}
\alias{longCat-class}

\title{Class \code{"longCat"}}
\description{
An object of class \code{\link{longCat}.} 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("longCat", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{data.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{dim}:}{Object of class \code{"integer"} ~~ }
    \item{\code{times}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{times.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{labels}:}{Object of class \code{"character"} ~~ }
    \item{\code{factors}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{IndTime}:}{Object of class \code{"logical"} ~~ }
    \item{\code{nfactors}:}{Object of class \code{"integer"} ~~ }
    \item{\code{sorted}:}{Object of class \code{"logical"} ~~ }
    \item{\code{ascending}:}{Object of class \code{"logical"} ~~ }
    \item{\code{group}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{groupLabels}:}{Object of class \code{"character"} ~~ }
    \item{\code{order.data}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "longCat")}: ... }
	 }
}
\references{
Tueller, S. J., Van Dorn, R. A., and Bobashev, G. V.  (2013). Visualization of Categorical Longitudinal and Times Series Data. Manuscript Under Review.}
\examples{
showClass("longCat")
}
\keyword{classes}
