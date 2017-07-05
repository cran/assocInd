SRI <-
function(x,Ya,Yb,Yab){
	estimate<-x/(Ya+Yb+Yab+x)
	se<-sqrt(estimate*(1-estimate)/(Yab+Ya+Yb+x))
	return(c(estimate,se))
}
