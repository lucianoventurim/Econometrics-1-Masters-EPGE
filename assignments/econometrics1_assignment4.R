### Econometrics 1 - Problem set 4
### Luciano Fabio B Venturim
set.seed(1)

### Question 2. Item 2
### Creating the data

x <- matrix(rnorm(500,2,4), ncol = 5) 
e <- matrix(rnorm(500,0,1), ncol = 5)
a <- matrix(rnorm(100,0,1))

beta0 <- 3

y <- beta0*x + e

for (i in 1:5){
  y[,i] = y[,i]+a
}

### First, we estimate the beta_fe estimator

x_mean <- rowMeans(x)
y_mean <- rowMeans(y)

y_tilde <- y
x_tilde <- x

for (i in 1:5){
  x_tilde[,i] = x[,i]-x_mean
  y_tilde[,i] = y[,i]-y_mean
}

beta_fe <- (sum(x_tilde*y_tilde))/(sum(x_tilde*x_tilde))

### Now, we estimate the LDSV estimator. As in Question 1, item 1, with only one
### regressor x_it, the LDSV estimator is just the linear projection estimator
### of y_tilde on x_tilde

Y_tilde <- t(y_tilde)
dim(Y_tilde) <- c(500,1)
X_tilde <- t(x_tilde)
dim(X_tilde) <- c(500,1)

proj_estim <- lm(Y_tilde ~ 0+X_tilde)
beta_lsdv <- proj_estim$coefficients

beta_fe == beta_lsdv

### Note that they are identical