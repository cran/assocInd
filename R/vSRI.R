vSRI <-
function(x,Yab){
	estimate<-x/(x+Yab)
	se<-sqrt(estimate*(1-estimate)/(x+Yab))
	return(c(estimate,se))
}
