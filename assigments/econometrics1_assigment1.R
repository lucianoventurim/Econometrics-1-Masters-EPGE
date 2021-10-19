### Econometrics 1 - Problem Set 1 - Pratical Questions
### Luciano Fabio Busatto Venturim

### First, we create our dataset, based on a dgp that follows y_i = x_i'\beta + e_i
### where E(e_i|x_i) = 0, i.e, a linear regression model. Here, x_i includes a
### constant.
set.seed(1)

###x1 <- rnorm(n = 1000, mean = 2, sd = 3)
###x2 <- rnorm(n = 1000, mean = 3, sd = 4)
###const <- rep(1,1000)
###e <- rnorm(n = 1000, mean = 0, sd = 10)

###y <- 2*const+5*x1+2*x2+e

###dgp <- data.frame(y,const,x1,x2,e)

###saveRDS(dgp, file = "student_datasets/ps1_lucianoventurim.Rds")


### Question 6. Now, we load our data and perform our analysis. 
data <- readRDS(file = "data/econometrics1_assigment1_data.Rds")

n <- length(data[,'x1'])

### We have that E(x_i(y_i - x_i'\beta)) = 0. Since the model is just-identified,
### we write the sample moments as the sum of x_i(y_i-x_i'\beta) from i=1 to n equals
### to zero. In matrix notation and solving for \beta, we have our MM estimator
### beta_mm = (x'x)^{-1}x'y, as follows:

x <- as.matrix(data[,c('const','x1','x2')])
y <- as.matrix(data[,'y'])

beta_mm <- solve(t(x)%*%x,t(x)%*%y)

### Question 7. Consider further that the errors are conditionally symmetric,
### i.e., that E(e_i^3|x_i) = 0 (which we know it is, since x_i and e_i are
### independente and e_i is normally distributed). Then, we have 6 conditions and
### 3 unknowns, so the model is over-identified. We then proceed to estimate the
### GMM estimator. First, we define our function gn(\beta).

gn <- function(b){
  #Notice that we do not divide by n in this function
  e_b <- y-x%*%b
  first_condition <- t(x)%*%e_b
  second_condition <- t(x)%*%(e_b^3)
  both_conditions <- c(first_condition,second_condition)
  return(both_conditions)
}

qn_ident <- function(b){
  q <- gn(b)
  return(t(q)%*%diag(length(q))%*%q)
}

gmm1 <- optim(c(0,0,0), fn = qn_ident, method = "BFGS")
#summary(gmm1)

beta_gmm1 <- gmm1$par


### Question 8. Now, we estimate the variance matrix of beta_gmm. Since 
### g_i(\beta) = [x_i(y_i-x_i'\beta),x_i(y_i-x_i'\beta)^3]', we have that G1, the
### Jacobian of g_n = 1/n*sum {g_i}, is 1/n*sum {[-x_ix_i',-3x_i(y_i-x_i'\beta)x_i']}.

residuals_gmm1 <- y-x%*%beta_gmm1

G1 <- rbind(-t(x)%*%x, -3*t(x)%*%((as.vector(residuals_gmm1)^2)*x))/n 

S1_1 <- t(x)%*%((as.vector(residuals_gmm1)^2)*x)/n 
S1_2 <- t(x)%*%((as.vector(residuals_gmm1)^4)*x)/n
S1_3 <- t(x)%*%((as.vector(residuals_gmm1)^6)*x)/n

S1 <- cbind(rbind(S1_1,S1_2),rbind(S1_2,S1_3))

### Thus, the variance estimate is given by

V_gmm1 <- solve(t(G1)%*%diag(6)%*%G1)%*%t(G1)%*%diag(6)%*%S1%*%diag(6)%*%G1%*%solve(t(G1)%*%diag(6)%*%G1)/n


### Question 9. Since we already have S1 estimated, we need only to find the 
### inverse and the two-steps GMM beta_gmm2.

qn_s<- function(b){
  q <- gn(b)
  return(t(q)%*%solve(S1)%*%q)
}

gmm2 <- optim(c(0,0,0), fn = qn_s, method = "BFGS")
#summary(gmm2)

beta_gmm2 <- gmm2$par

### Question 10. The estimator of the variance of the OGMM estimator is V_gmm2 =
### (G2'S2^{-1}G2)^{-1}/n, where the G2 and S2 are the estimators evaluated at
### beta_gmm2

residuals_gmm2 <- y-x%*%beta_gmm2

G2 <- rbind(-t(x)%*%x, -3*t(x)%*%((as.vector(residuals_gmm2)^2)*x))/n 

S2_1 <- t(x)%*%((as.vector(residuals_gmm2)^2)*x)/n 
S2_2 <- t(x)%*%((as.vector(residuals_gmm2)^4)*x)/n
S2_3 <- t(x)%*%((as.vector(residuals_gmm2)^6)*x)/n

S2 <- cbind(rbind(S1_1,S1_2),rbind(S1_2,S1_3))

V_gmm2 <- solve(t(G2)%*%solve(S2)%*%G2)/n
