## ---- Vectors_and_Matrices 
x <- c(1, 2, 3, 4, 6)
x
x <- c(6:1)
x
length(x)
y <- matrix(x, nrow = 2, ncol = 3)
y
y <- matrix(x, nrow = 2, ncol = 3, byrow=TRUE)
y

## ---- Random_Numbers
x <- rnorm(1000, mean = 10, sd = 2)
head(x)
hist(x)

## ---- Random_Numbers_2
x <- rexp(1000, rate = 10)
hist(x)

## ---- Plot
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(1000, mean = 5, sd = 1)
plot(x, y)

## ---- Plot_2
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(1000, mean = 5, sd = 1)
z <- rexp(1000, rate = 0.2)
plot(x, y, ylim = range(c(x, y, z)), main = "My plot", ylab="Y and Z")
points(x, z, col = "red")
legend("topright", c("these dots are Y", "these dots are Z"),
	  pch = c(1, 1), col = c("black", "red"))

## ---- Regression_1
library(MASS)
data(Boston)
names(Boston)

## ---- Regression_2
lm.fit <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)


## ---- Regression_3
confint(lm.fit)

## ---- Regression_4
par(mfrow = c(1, 2))
plot(predict(lm.fit), residuals(lm.fit))
abline(h=0, col = "red")
plot(lm.fit, which = 2)

## ---- Regression_5
lm.fit.all <- lm(medv ~ ., data=Boston)

## ---- Fun
install.packages("fun")
library(fun)
gomoku(n = 19)
