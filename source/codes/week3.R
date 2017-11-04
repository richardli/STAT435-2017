
set.seed(042017)
n0 <- 25 * 2
train <- matrix(NA, n0, 2)
class <- rep("", n0)
# red class
train[1:(n0/2), 1] <- rnorm(n0/2, 0, 1)
train[1:(n0/2), 2] <- rnorm(n0/2, 0, 1)
class[1:(n0/2)] <- "red"

# blue class
train[(n0/2+1):n0, 1] <- rnorm(n0/2, 1.5, 1)
train[(n0/2+1):n0, 2] <- rnorm(n0/2, 1.5, 1)
class[(n0/2+1):n0] <- "blue"

plot(train[, 1], train[, 2], col = class, 
	 xlab = "First dimension", ylab = "Second dimension", main = "Training set")

## ---- 1b
nlist <- c(25, 100, 1000, 10000) * 2
par(mfrow = c(2, 2))
for(k in 1:4){
	n <- nlist[k]
	test <- matrix(NA, n, 2)
	class.test <- rep("", n)
	# red class
	test[1:(n/2), 1] <- rnorm(n/2, 0, 1)
	test[1:(n/2), 2] <- rnorm(n/2, 0, 1)
	class.test[1:(n/2)] <- "red"

	# blue class
	test[(n/2+1):n, 1] <- rnorm(n/2, 1.5, 1)
	test[(n/2+1):n, 2] <- rnorm(n/2, 1.5, 1)
	class.test[(n/2+1):n] <- "blue"

	## ---- 1c
	library(class)
	K <- 20
	error <- matrix(NA, K, 2)
	for(k in 1:K){
		knn1 <- knn(train=train, test=train, cl=class, k=k)
		error[k, 1] <- sum(knn1 != class) / n0
		knn2 <- knn(train=train, test=test, cl=class, k=k)
		error[k, 2] <- sum(knn2 != class.test) / n
	}
	plot(1/seq(1, K), error[, 1], ylim = range(error), type = "l",
		 xlab = "1/K", ylab = "Error", main = paste(round(n/2), "test observations in each class\n minimum test error:", round(min(error[, 2]), 4)))
	lines(1/seq(1, K), error[, 2], col = "red")	
	legend("bottomleft", c("Training error", "Test error"), 
			col = c("black", "red"), lty = c(1, 1))	 
	abline(h=1 - pnorm(sqrt(1.5 ^ 2 + 1.5 ^ 2) / 2), col = 'orange', lty = 2, lwd = 2)
}
