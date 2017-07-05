GLECI <-
function(x,Ya,Yb,Yab,Ynull,w){
	T<-x+Ya+Yb+Yab+Ynull
	Ti<-x+Ya+Yb+Yab

	#Get A, B and C for the quadratic formula
	A<--w*T
	B<-x*w+Ynull*w-Ti
	C<-x

	#If w=0 return SRI else return the solution to quadratic		
	if(w==0){estimate<-x/(Ya+Yb+Yab+x)}else{estimate=(-B-sqrt(B^2-4*A*C))/(2*A)}

	#Calculate effective sample size and use to get standard error
	Ne<-Ti+Ynull*(w*estimate*(1-estimate))/((w*estimate+1)^2)
	se<-sqrt(estimate*(1-estimate)/Ne)

	return(c(estimate,se))
}
