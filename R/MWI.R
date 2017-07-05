MWI <-
function(x,Ya,Yb,Yab,m){
	estimate<-x/(m*(Ya+Yb)+Yab+x)
	se<-sqrt(estimate*(1-estimate)/(Yab+m*(Ya+Yb)+x))
	return(c(estimate,se))
}
