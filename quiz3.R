#Q1

fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)

summary(fit1)$coeff

#Q2

fit2 <- lm(mpg ~ factor(cyl), data = mtcars)

summary(fit1)$coef[3]; summary(fit2)$coef[3];

#Q3 

fit3 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, data = mtcars)

anova(fit1,fit3)

#Q4

fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

summary(fit4)

#Q5

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit5 <- lm(y~x)

round(hatvalues(fit5),4)

#Q6

round(dfbetas(fit5), 4)


