\name{vSRI}
\alias{vSRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	The very Simple Ratio Index
}
\description{
	Calculates the very Simple Ratio Index
}
\usage{
vSRI(x, Yab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Number of times individuals a and b were observed together
}
  \item{Yab}{
	Number of times individuals a and b were observed at the same time but not associating
}
}
\details{
	The very simple ratio index calculates the probability that two individuals are observed together given that both have been seen. This index is useful if there is large individual identification error and is an unbiased estimate (however it requires the majority of groups in the study to be sampled in a given sampling period).
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
	x <- yab <- 10
	
	# Calculate very simple ratio index
	vSRI(x,yab)
	
}
