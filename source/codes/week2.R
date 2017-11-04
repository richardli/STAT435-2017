## ---- data
library(ISLR)

data(Carseats)

?Carseats

## ---- regression
fit <- lm(Sales ~ Age + Price + CompPrice + Population + Income:Advertising, 
	   data = Carseats)
summary(fit)

## ---- regression2
fit_no_pop <- lm(Sales ~ Age + Price + CompPrice + Income:Advertising, 
	   data = Carseats)
anova(fit, fit_no_pop)

## ---- regression3
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

## ---- selection 
library(MASS)
step <- stepAIC(fit, direction="backward", trace=FALSE)
step$anova

## ---- interval
newdata = data.frame(Age = 50, Price = 100, CompPrice = 110, Income = 100,
          Advertising = 10, Population = 300)
predict(fit, newdata, interval = "confidence")
predict(fit, newdata, interval = "predict")

## ---- residual
set.seed(1)
x <- rnorm(1000, mean = 5, sd = 2)
y <- 2 + x^2 * 10 + rnorm(1000)
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ I(x ^ 2))
par(mfrow = c(1, 2))
plot(fit1, 1, main = "Y ~ X")
plot(fit2, 1, main = "Y ~ X^2")

## ---- double
confint(fit2)
x2 <- rep(x, 10)
y2 <- rep(y, 10)
fit3 <- lm(y2 ~ I(x2 ^ 2))
confint(fit3)

## ---- double2
set.seed(1)
n0 <- 0
n1 <- 0
for(sim in 1:1000){
	x <- rnorm(1000, mean = 5, sd = 2)
	y <- 2 + x^2 * 10 + rnorm(1000)
	m0 <- lm(y ~ I(x ^ 2))
	CI0 <- confint(m0)[1, ]

	x2 <- rep(x, 10)
	y2 <- rep(y, 10)
	m1 <- lm(y2 ~ I(x2 ^ 2))
	CI1 <- confint(m1)[1, ]

	if(CI0[1] < 2 && CI0[2] > 2) n0 <- n0 + 1
	if(CI1[1] < 2 && CI1[2] > 2) n1 <- n1 + 1
}
c(n0, n1) / 1000

# ---- weight
set.seed(1)
x <- rnorm(1000, mean = 3, sd = 0.5)
y <-  2 + x * 10 + rnorm(1000, sd = x)
fit1 <- lm(y ~ x)
plot(fit1, 1)

# ---- weight2
summary(fit1)$coefficients
# use inverse variance as the weight
fit2 <- lm(y ~ x, weights = 1/(x^2))
summary(fit2)$coefficients


