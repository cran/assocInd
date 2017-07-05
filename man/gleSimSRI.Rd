\name{gleSimSRI}
\alias{gleSimSRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Simulate SRI with group location error
}
\description{
	Generate an estimated simple ratio index under a given rate of missing observations of groups that are present
}
\usage{
gleSimSRI(aAB, w, pMissA, pMissB, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aAB}{
	The real association rate between individuals A and B
}
  \item{w}{
	The group location error term
}
  \item{pMissA}{
	Probability of missing group A
}
  \item{pMissB}{
	Probability of missing group A
}
  \item{n}{
	The number of sampling periods (number of observations of the dyad)
}
}
\details{
	A simple function that simulates data for a given probability of missing groups and real association strength. The w term represents the likelihood of failing to observe a group containing a and b compared to failing to observe both groups containing a and b if the two individuals are apart. The function returns the simulated simple ratio index and whether the value lies within the 95 percent confidence intervals of the simple ratio index given the number of samples and under the assumption of no error.
}
\value{
	Returns two values: the simulated simple ratio index and whether or not it falls within the 95 percent confidence intervals (1 = yes, 0 = no)
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
	
	# Set w range
	w <- seq(-1,1,0.1)

	# Set observation errors
	pMissA <- 0.7
	pMissB <- 0.7
	
	# Replicate N times
	replicates <- 100  # small number used to save computation time
	
	# Create a blank storage matrices
	assocStrength <- matrix(NA,nrow=replicates,ncol=length(w))
	inCIs <- matrix(NA,nrow=replicates,ncol=length(w))
	
	# Loop through repeating N times for each error value
	for (i in 1:length(w)) { 
		for (j in 1:replicates) {
			out <- gleSimSRI(aAB,w[i],pMissA,pMissB,20)
			assocStrength[j,i] <- out[1]
			inCIs[j,i] <- out[2]
		}
	}
	
	# Plot the results
	par(mfrow=c(1,2))
	plot(w,colMeans(assocStrength, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Simulated SRI")
	CIs <- apply(assocStrength,2,quantile,c(0.025,0.975),na.rm=TRUE)
	arrows(w,CIs[1,],w,CIs[2,],len=0.1,code=3,angle=90)
	abline(h=0.5,col="red")
	
	plot(w,colMeans(inCIs, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Percent of times in CIs")
	abline(h=0.95, col="red")

}
