#Q1, Q2

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

data <- data.frame(cbind(x,y))

fit <- lm(y~x,data = data)

summary(fit)

#Q3

fit2 <- lm(mpg~wt, data = mtcars)

predict(fit2, data.frame(wt=mean(mtcars$wt)), interval = ("confidence"))

#Q4

help("mtcars")

#Q5

predict(fit2, data.frame(wt=3), interval = ("prediction"))

#Q6

fit3 <- lm(mpg~I(wt/2), data = mtcars)

confint(fit3)

#Q7

summary(fit2)

#Q9

fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")

anova(fit)

anova(fit5)

278/1126