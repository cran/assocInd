HWI <-
function(x,Ya,Yb,Yab){
	estimate<-x/(0.5*(Ya+Yb)+Yab+x)
	se<-sqrt(estimate*(1-estimate)/(Yab+0.5*(Ya+Yb)+x))
	return(c(estimate,se))
}
