\name{HWI}
\alias{HWI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Half-Weight Index
}
\description{
	Calculates the half-weight index
}
\usage{
HWI(x, Ya, Yb, Yab)
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
}\details{
	The half-weight index calculates the probability that two individuals are observed together given that one has been seen, but under the assumption that the probability of missing individuals a or b when they are together is equal to M times the probability of missing them when they are apart. This index is the most commonly used in animal social network analysis, but note that Hoppitt & Farine (in prep) and other authors argue that the weighting of the probabilities is arbitrary.
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
	
	# Calculate half-weight index
	HWI(x,ya,yb,yab)
	
}
