iieSimSRI <-
function(aAB,e,n){
	Yab<-Ya<-Yb<-Ynull<-x<-0
	together<-rbinom(n,1,aAB)
	for (i in 1:length(together)){
		if(together[i]==1){
			if(runif(1,0,1)<e){
				if(runif(1,0,1)<e){Ynull<-Ynull+1}else{Yb<-Yb+1}
			}else{
				if(runif(1,0,1)<e){Ya<-Ya+1}else{x<-x+1}
			}
		}else{
			if(runif(1,0,1)<e){
				if(runif(1,0,1)<e){Ynull<-Ynull+1}else{Yb<-Yb+1}
			}else{
				if(runif(1,0,1)<e){Ya<-Ya+1}else{Yab<-Yab+1}
			}
		}
	}
	
	estimate<-x/(Ya+Yb+Yab+x)
	se<-sqrt(estimate*(1-estimate)/(Yab+Ya+Yb+x))
	
	return(c(estimate,1*(estimate+1.96*se)>aAB&(estimate-1.96*se)<aAB))
}
