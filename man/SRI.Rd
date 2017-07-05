\name{SRI}
\alias{SRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple Ratio Index}
\description{
Calculates the simple ratio index
}
\usage{
SRI(x, Ya, Yb, Yab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Number of times individuals a and b were observed together
}
  \item{Ya}{
	Number of times individual a was observed without b
}
  \item{Yb}{
	Number of times individual b was observed without a
}
  \item{Yab}{
	Number of times individuals a and b were observed at the same time but not associating
}
}
\details{
	The simple ratio index calculates the probability that two individuals are observed together given that one has been seen. This index is widely used in animal social network analysis.
}
\value{
	Returns two elements: the estimated association strength and the standard error of the estimate.
}
\references{
	Hoppitt, W. & Farine, D.R. (in prep) Association indices for quantifying social relationships: how to deal with missing observations of individuals or groups.
}
\author{
	William Hoppitt <W.J.E.Hoppitt@leeds.ac.uk>
	Damien Farine <dfarine@orn.mpg.de>
}
\examples{
	
	# Simulated values (all = 10)
	x <- ya <- yb <- yab <- 10
	
	# Calculate simple ratio index
	SRI(x,ya,yb,yab)
	
}
