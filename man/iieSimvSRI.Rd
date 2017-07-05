\name{iieSimvSRI}
\alias{iieSimvSRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Simulate vSRI with individual identification error
}
\description{
	Generate an estimated very simple ratio index under a given rate of missing observations of one individual given that it is present
}
\usage{
iieSimvSRI(aAB, e, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aAB}{
	The real association rate between individuals A and B
}
  \item{e}{
	The probability of failing to observe an individual given it is present in a group
}
  \item{n}{
	The number of sampling periods (number of observations of the dyad)
}
}
\details{
	A simple function that simulates data for a given rate of identification error and real association strength. The function returns the simulated very simple ratio index and whether the value lies within the 95 percent confidence intervals of the very simple ratio index given the number of samples and under the assumption of no error.
}
\value{
	Returns two values: the simulated very simple ratio index and whether or not it falls within the 95 percent confidence intervals (1 = yes, 0 = no)
}
\references{
	Hoppitt, W. & Farine, D.R. (in prep) Association indices for quantifying social relationships: how to deal with missing observations of individuals or groups.
}
\author{
	William Hoppitt <W.J.E.Hoppitt@leeds.ac.uk>
	Damien Farine <dfarine@orn.mpg.de>
}
\examples{

	# Set a real association index
	aAB <- 0.5

	# Create a range of errors
	e <- seq(0,0.8,0.01)
	
	# Replicate N times
	replicates <- 100  # small number used to save computation time
	
	# Create a blank storage matrices
	assocStrength <- matrix(NA,nrow=replicates,ncol=length(e))
	inCIs <- matrix(NA,nrow=replicates,ncol=length(e))
	
	# Loop through repeating N times for each error value
	for (i in 1:length(e)) { 
		for (j in 1:replicates) {
			out <- iieSimvSRI(aAB,e[i],20)
			assocStrength[j,i] <- out[1]
			inCIs[j,i] <- out[2]
		}
	}
	
	# Plot the results
	par(mfrow=c(1,2))
	plot(e,colMeans(assocStrength, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Simulated HWI")
	CIs <- apply(assocStrength,2,quantile,c(0.025,0.975),na.rm=TRUE)
	arrows(e,CIs[1,],e,CIs[2,],len=0.1,code=3,angle=90)
	abline(h=0.5,col="red")
	
	plot(e,colMeans(inCIs, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Percent of times in CIs")
	abline(h=0.95, col="red")

}
