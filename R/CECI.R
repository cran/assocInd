CECI<-function (x, Ya, Yb, Yab, Ynull, w, psi, E, plot = F) 
{
    requireNamespace("rootSolve")
    fun <- function(Index, x, Ya, Yb, Yab, Ynull, w, psi, E) {
    	
    		if(Index==1){return (-Inf)}else{
    	    return(    (Ya + Yb) * (((1 - psi)/(psi)) * (1 + w) * E - 2 * (1 - 
            E))/(((1 - psi)/psi) * (1 + w) * E * Index + 2 * 
            (1 - E) * (1 - Index)) - Yab/(1 - Index) + x/Index + 
            Ynull * w/(w * Index + 1)
          )
    		}
    	}

    if (plot) {
        graphics::curve(fun(Index = x, x = x, Ya = Ya, Yb = Yb, Yab = Yab, 
            Ynull = Ynull, w = w, E = E, psi = psi), 0, 1)
        graphics::abline(h = 0, lty = 2)
    }
    if (x == 0) {
    		#The maximum likelihood is then at Index=0
        estimate <- 0
    }else {    	
		e <- try(estimate <- uniroot(fun, c(0, 1), x = x, Ya = Ya, Yb = Yb, Yab = Yab, Ynull = Ynull, w = w, E = E, psi = psi)$root)
		if (class(e) == "try-error") {
			estimate <- NA
		}
    }
    FishersInformation <- (x + Ya + Yb + Yab + Ynull) * (-E * 
        ((((1 - psi)/psi) * (1 + w) * E - 2 * (1 - E))^2)/(((1 - 
        psi)/psi) * (1 + w) * E * estimate + 2 * (1 - E) * (1 - 
        estimate)) + ((1 - E)^2)/(1 - estimate) + (psi - (1 + 
        w) * (E^2))/(psi * estimate) + (E^2) * (w^2)/(w * estimate + 
        1))
    se <- sqrt(1/FishersInformation)
    return(c(estimate, se))
}
