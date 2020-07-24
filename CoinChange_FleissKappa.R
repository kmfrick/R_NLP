devmatrix <- matrix(data= c(0,0,0,0,14,0,2,6,4,2,0,0,3,5,6,0,3,9,2,0,2,2,8,1,1,7,7,0,0,0,3,2,6,3,0,2,5,3,2,2,6,5,2,1,0,0,2,2,3,7), ncol=5, byrow=TRUE)

atm <- function(n) {
	if (n %% 5 != 0) {
		return(0);
	}
	bills <- c(500, 200, 100, 50, 20, 10, 5);
	result <- rep(0, length(bills))
	i <- 1;
	while (i <= length(bills)) {
		while (n >= bills[i]) {
			n = n -  bills[i];
			result[i] = result[i] + 1;
		}
		i = i + 1;
	}
	result = as.list(result)
	names(result) <- bills;
	return(result);
}

pe <- function(m) {
	oea = 0;
	for (i in 1:ncol(m)) {
		numerator = sum(m[, i])
		denominator = sum(m[i,]) * nrow(m)
		pj = as.double(numerator) / denominator
		# Number of annotators = sum of row
		# SHOULD be equal across rows
		oea = oea + pj * pj
	}
	return(oea)
}

po <-function(m) {
	ooa = 0;
	for (i in 1:nrow(m)) {
		numerator = sum(m[i,] * (m[i,] - 1))
		denominator = sum(m[i,]) * (sum(m[i,]) - 1)
		ooa = ooa + as.double(numerator) / denominator
	}
	ooa = ooa / (nrow(m))
	return(ooa)
}

