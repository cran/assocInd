\name{ceSimErrorsEqualHWI}
\alias{ceSimErrorsEqualHWI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Simulate SRI under combined group and individual location errors
}
\description{
	Generate an estimated simple ratio index under conditions where both group and individual location errors are present, but where individual location errors are equal for all individuals
}
\usage{
ceSimErrorsEqualHWI(aAB, w, psi, E, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aAB}{
	The real association rate between individuals A and B
}
  \item{w}{
	The group location error term
}
  \item{psi}{
	The relative importance of the group location error to the individual location error
}
  \item{E}{
	The probability of missing one individual given that it is not with the other
}
  \item{n}{
	The number of sampling periods
}
}
\details{
	A simple function that simulates data for a given probability of missing groups and real association strength. The w term represents the likelihood of failing to observe a group containing a and b compared to failing to observe both groups containing a and b if the two individuals are apart. E (Epsilon) is probability of missing one individual given that it is not with the other. psi represents the relative importance of the group location error to the individual identification error. The function returns the simulated half weight index and whether the value lies within the 95 percent confidence intervals of the half weight index given the number of samples and under the assumption of no error.
}
\value{
	Returns two values: the simulated half weight index and whether or not it falls within the 95 percent confidence intervals (1 = yes, 0 = no)
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

	# Set errors parameters
	psi <- 0.5
	E <- 0.5
	
	# Replicate N times
	replicates <- 100  # small number used to save computation time
	
	# Create a blank storage matrices
	assocStrength <- matrix(NA,nrow=replicates,ncol=length(w))
	inCIs <- matrix(NA,nrow=replicates,ncol=length(w))
	
	# Loop through repeating N times for each error value
	for (i in 1:length(w)) { 
		for (j in 1:replicates) {
			out <- ceSimErrorsEqualHWI(aAB,w[i],psi,E,20)
			assocStrength[j,i] <- out[1]
			inCIs[j,i] <- out[2]
		}
	}
	
	# Plot the results
	par(mfrow=c(1,2))
	plot(w,colMeans(assocStrength, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Simulated HWI")
	CIs <- apply(assocStrength,2,quantile,c(0.025,0.975),na.rm=TRUE)
	arrows(w,CIs[1,],w,CIs[2,],len=0.1,code=3,angle=90)
	abline(h=0.5,col="red")
	
	plot(w,colMeans(inCIs, na.rm=TRUE), pch=20, ylim=c(0,1), ylab="Percent of times in CIs")
	abline(h=0.95, col="red")

}
