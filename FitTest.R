k <- c(83, 91, 122, 107, 74, 123)
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
N <- 600 #Dobasok szama

Illeszkedes <- function(k, p, N){

	khi2 <- 0
	n <- length(k)

	for (i in 1:n) {
        khi2 <- khi2 + ((( k[i] - N * p[i] ) ^2 ) / ( N * p[i] ))
    	}

    	return(khi2)
}

Illeszkedes(k, p, N)
