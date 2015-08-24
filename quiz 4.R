# Quiz 4

# Q1

library(MASS)

data("shuttle")

fit1 <- glm(shuttle$use ~ shuttle$wind, family = "binomial")

odds <- exp(summary(fit1)$coeff)

#Q2 

fit2 <- glm(shuttle$use ~ shuttle$wind + shuttle$magn, family = "binomial")

odds <- exp(summary(fit2)$coeff)

#Q4

fit3 <- glm(count ~ spray - 1, data=InsectSprays, family="poisson")
rate <- exp(summary(fit3)$coef)

rate[1]/rate[2]

#Q5

x <- -5 : 5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

knotPoint <- c(0)
spline <- sapply(knotPoint, function(knot) (x > knot) * (x - knot))
xMatrix <- cbind(1, x, spline)
fit <- lm(y ~ xMatrix - 1)
yhat <- predict(fit)
yhat
slope <- fit$coef[2] + fit$coef[3]
slope # 1.013
plot(x, y)
lines(x, yhat, col=2)