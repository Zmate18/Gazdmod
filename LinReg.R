x <- rnorm(100)
y <- rnorm(100)
n <- length(x)

LinRegres <- function(X, Y) {

	plot(X, Y)
	a <- calculateA(X, Y)
	b <- calculateB(X, Y)
	lines(X, a * X + b, type = "l", lty = 1, col = "red")
}

calculateA <- function(X, Y) {

	sumX <- Summary(X)
   	sumY <- Summary(Y)
    	sumXY <- Summary(X, Y)
    	sumXsquare <- SummarySquare(X)

    	first <- (sumXY * n) - (sumY * (-sumX))
    	second <- (sumXsquare * n) - (sumX * (-sumX))
    	return(first / second)
}

calculateB <- function(X, Y) {

    	sumX <- Summary(X)
    	sumY <- Summary(Y)
    	sumXY <- Summary(X,Y)
    	sumXsquare <- SummarySquare(X)

    	first <- (sumX * sumY) - (sumX * sumXY)
    	second <- (sumXsquare * n) - (sumX * (-sumX))
    	return(first / second)
}

Summary <- function(X, Y) {

	sum <- 0
	n <- length(X)


      for (i in 1:n) {
        sum <- sum + X[i]
      }
      return(sum)


	for (i in 1:n) {
        sum <- sum + X[i] * Y[i]
    	}
    	return(sum)
}

SummarySquare <- function(X) {

	sum <- 0
	n <- length(X)

	for (i in 1:n) {
        sum <- sum + X[i]^2
    	}
    	return(sum)
}

LinRegres(x, y)

