gleSimHWI <-
function(aAB,w,pMissA,pMissB,n){
	Yab<-Ya<-Yb<-Ynull<-x<-0
	together<-rbinom(n,1,aAB)
	for (i in 1:length(together)){
		if(together[i]==1){
			if(runif(1,0,1)<(w+1)*pMissA*pMissB){Ynull<-Ynull+1}else{x<-x+1}
		}else{
			if(runif(1,0,1)<pMissA){
				if(runif(1,0,1)<pMissB){Ynull<-Ynull+1}else{Yb<-Yb+1}
			}else{
				if(runif(1,0,1)<pMissB){Ya<-Ya+1}else{Yab<-Yab+1}
			}
		}
	}
	
	HWI1<-HWI(x,Ya,Yb,Yab)	
	estimate<-HWI1[1]
	se<-HWI1[2]
	return(c(estimate,1*(estimate+1.96*se)>aAB&(estimate-1.96*se)<aAB))
	
}
