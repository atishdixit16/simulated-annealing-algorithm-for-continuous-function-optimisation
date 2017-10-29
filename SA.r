
func <- function(x) {
	return(-20*exp(-0.2*sqrt(0.5*(x[1]^2+x[2]^2))) - exp(0.5*(cos(2*pi*x[1])+cos(2*pi*x[2]))) + exp(1) + 20)
}


SimAnn <- function(x,func, Temp=1000, n_mmc=100, mmc_step=0.1,delta_Temp=0.95, Temp_steps=500, display=TRUE) {
	F <- func(x)
	minFunc <- 10e10
	minArg <- x
	for (T in 0:Temp_steps) {
		if (display==TRUE) {
			if (T==0) {
				a <- seq(-5,5,0.1)
				l <- length(a)
				mat <- matrix(0,l, l)
				for (i in 1:l)
					for (j in 1:l)
						mat[i,j] = func(c(a[i],a[j]))
				image(a,a,mat)
				points(x[1], x[2])
			}
		}
		Temp <- max ( 1 , Temp*delta_Temp)
		for (i in 1:n_mmc) {
			x_new <- (x - runif(length(x),-mmc_step,mmc_step))
			F_new <- func(x_new)
			deltaF <- F_new - F
			if ( (deltaF<0) || (runif(1) < exp(-deltaF/Temp)) ) {
				F <- F_new
				x <- x_new
			}
			if (F_new < minFunc) {
				minFunc <- F_new
				minArg <- x_new
			}
			if (display==TRUE)
				points(x[1], x[2])
		}
	}

	return ( list(min_func=minFunc,min_arg=minArg) )
}


answer <- SimAnn(c(-4,4),func, Temp=1, n_mmc=150, mmc_step=0.1,delta_Temp=0.95, Temp_steps=50, display=TRUE )
print(answer) 
