\name{MWI}
\alias{MWI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	M-Weight index
}
\description{
	Calculates the m-weight index, a generalisation of the half-weight index correcting for individual identification error.
}
\usage{
MWI(x, Ya, Yb, Yab, m)
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
  \item{m}{
	The correction term m (see details)
}
}
\details{
	The m-weight index calculates the probability that two individuals are observed together given that one has been seen, but under the assumption that the probability of missing individuals a or b when they are together is equal to m times the probability of missing them when they are apart. This index can be used if prior information is available on the observation probability of individuals when they are together versus when they are apart (defined by the ratio m).
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
	
	# Set m (here make the MWI equal to the SRI)
	m <- 1.0
	
	# Calculate m-weight index
	MWI(x,ya,yb,yab,m)
	
}
