ceSimErrorsEqualSRI <-
function(aAB,w, psi,E,n){
	Yab<-Ya<-Yb<-Ynull<-x<-0
	EGab<-((1-psi)/psi)*(1+w)*(E^2)/2
	together<-rbinom(n,1,aAB)
	for (i in 1:length(together)){
		if(together[i]==1){
			randEvent<-rmultinom(1,1,c(((1+w)*E^2), EGab, EGab,1-2*EGab-((1+w)*E^2)))
			Ynull<-Ynull+randEvent[1]
			Ya<-Ya+randEvent[2]
			Yb<-Yb+randEvent[3]
			x<-x+randEvent[4]			
		}else{
			if(runif(1,0,1)<E){
				if(runif(1,0,1)<E){Ynull<-Ynull+1}else{Yb<-Yb+1}
			}else{
				if(runif(1,0,1)<E){Ya<-Ya+1}else{Yab<-Yab+1}
			}
		}
	}
	
	SRI1<-SRI(x,Ya,Yb,Yab)	
	estimate<-SRI1[1]
	se<-SRI1[2]
	return(c(estimate,1*(estimate+1.96*se)>aAB&(estimate-1.96*se)<aAB))
}
