\name{GLECI}
\alias{GLECI}
\title{
	The Group Location Error Corrected Index 
}
\description{
	Calculates the Group Location Error Corrected Index
}
\usage{
GLECI(x, Ya, Yb, Yab, Ynull, w)
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
  \item{Ynull}{
	Number of times neither a or b were observed
}
  \item{w}{
	The correction term w (see details)
}
}
\details{
	The GLECI calculates the probability that two individuals are observed together given that one has been seen, correcting for group location error (missing entire groups during a sampling period). This index can be used if prior information is available on the observation probability of finding groups, where the correction factor w is based on calibration data suggesting that failing to observe a group containing both a and b when they are together is w times more (w > 0) or less (w < 0) likely than failing to observe both the group containing a and the group containing b when a and b are apart.
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
	
	# Simulated values
	x <- ya <- yb <- yab <- 10
	ynull <- 0
	
	# Set w (here make the GLECI equal to the SRI)
	w <- 1.0

	# Calculate the group location error corrected index
	GLECI(x,ya,yb,yab,ynull,w)
	
}
