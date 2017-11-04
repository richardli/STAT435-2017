## ---- pca-1
library(ISLR)
data(College)
dim(College)
College[, 1] <- as.numeric(College[, 1] == "Yes")
fit <- princomp(College)
plot(fit)

## ---- pca-2
dim(fit$loading)
dim(fit$scores)
College.center <- as.matrix(College)
College.center <- apply(College.center, 2, function(x){x - mean(x)})
transform <- College.center %*% fit$loading
mean(abs(fit$scores - transform))

## ---- pca-3
Z1 <- fit$scores[, 1]
approx1 <- Z1 %*% t(fit$loading[, 1])
mean(abs(approx1 - College.center))
plot(approx1, College.center)
abline(0, 1, col="red")

## ---- pca-4
Z2 <- fit$scores[, 1:2]
approx2 <- Z2 %*% t(fit$loading[, 1:2])
mean(abs(approx2 - College.center))
plot(approx2, College.center)
abline(0, 1, col="red")

## ---- pca-5
Z18 <- fit$scores[, 1:18]
approx18 <- Z18 %*% t(fit$loading[, 1:18])
mean(abs(approx18 - College.center))
plot(approx18, College.center)
abline(0, 1, col="red")


## ---- pcr
library(pls)
Y <- 2 + College.center %*% rnorm(18)
fit.pcr <- pcr(Y ~ College.center, validation ="CV")
validationplot(fit.pcr,val.type="MSEP")
pred.pcr <- predict(fit.pcr, College.center, ncomp = 2)

## ---- pcr-2
# reproduce this with PCA
Z2 <- College.center %*% fit$loading[, 1:2]
fit.byhand <- lm(Y ~ Z2)
pred.byhand <- cbind(1, Z2) %*% coef(fit.byhand)
plot(pred.pcr, pred.byhand)

