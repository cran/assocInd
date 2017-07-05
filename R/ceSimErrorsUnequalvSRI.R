ceSimErrorsUnequalvSRI <-
function(aAB,w, psi,Emean, Esd,n){
	Yab<-Ya<-Yb<-Ynull<-x<-0
	
	# generate random errors
	Ea<-rnorm(1,Emean,Esd)
	Eb<-rnorm(1,Emean,Esd)
	while(Ea<0|Ea>1){
		Ea<-rnorm(1,Emean,Esd)
	}
	while(Eb<0|Eb>1){
		Eb<-rnorm(1,Emean,Esd)
	}

	EGa<-((1-psi)/psi)*(1+w)*(Ea^2*Eb)/(Ea+Eb)
	EGb<-((1-psi)/psi)*(1+w)*(Ea*Eb^2)/(Ea+Eb)

	if((1+w)*Ea*Eb>0){p1<-(1+w)*Ea*Eb}else{p1<-0}
	if(Ea>0){p2<-EGa}else{p2<-0}
	if(Eb>0){p3<-EGb}else{p3<-0}
	if((1-EGa-EGb-(1+w)*Ea*Eb)>0){p4<-1-EGa-EGb-(1+w)*Ea*Eb}else{p4<-0}

	together<-rbinom(n,1,aAB)
	
	for (i in 1:length(together)){
		if(together[i]==1){

			randEvent<-rmultinom(1,1,c(p1,p2,p3,p4))
			Ynull<-Ynull+randEvent[1]
			Ya<-Ya+randEvent[2]
			Yb<-Yb+randEvent[3]
			x<-x+randEvent[4]			
		}else{
			if(runif(1,0,1)<Ea){
				if(runif(1,0,1)<Eb){Ynull<-Ynull+1}else{Yb<-Yb+1}
			}else{
				if(runif(1,0,1)<Eb){Ya<-Ya+1}else{Yab<-Yab+1}
			}
		}
	}

	vSRI1<-vSRI(x,Yab)	
	estimate<-vSRI1[1]
	se<-vSRI1[2]
	return(c(estimate,1*(estimate+1.96*se)>aAB&(estimate-1.96*se)<aAB))

}
