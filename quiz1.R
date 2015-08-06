# Quiz 1

# Q1

minsq <- function(mu){
        x <- c(0.18, -1.54, 0.42, 0.95)
        
        y <- (x - mu)^2
        
        w <- c(2, 1, 3, 1)
        
        sum(w*y) 
}

v <- c(0.3,0.0025,1.077,0.1471)

lapply(v, minsq)

# Q2

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

yc <- y - mean(y)
xc <- x - mean(x)

beta1 <- sum(y * x) / sum(x ^ 2)

beta1

#Q3

data(mtcars)

head(mtcars)

lm(mpg~wt, data=mtcars)

#Q6

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

(x - mean(x)) / sd(x)

#Q7

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)


beta1 = cor(y, x) *  sd(y) / sd(x)
beta0 = mean(y) - beta1 * mean(x)

beta1; beta0

#Q9

mindis <- function(mu){
        
        x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
        
        sum((x - mu)^2)
        
}

v <- c(0.573,0.8,0.36,0.44)

lapply(v, mindis)


