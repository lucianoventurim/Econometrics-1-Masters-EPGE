### Econometrics 1 - Problem set 3
### Luciano Fabio Busatto Venturim

### We will use here the code made available at /ps3_prep.R
library("AER")
data("MASchools")


df<-data.frame(
  MASchools$stratio, 
  MASchools$english, 
  MASchools$income, 
  MASchools$score4)
x<-as.matrix(df[1:3])
y<-as.matrix(df[4])
n<-length(y)

head(df)

x32 = x[,3]^2
Jn<-function(beta){
  u<-y-x%*%beta
  m1<-(1/n*(t(x)%*%u))
  m2<- (1/n*t(x32)%*%u) # let's use a square term in income
  M<-as.matrix(rbind(m1,m2))
  return(n*t(M)%*%M)
}

#  Solve numerically
sol<-optim(par = c(0,0,0),Jn, method = 'BFGS')
beta_gmm<-sol$par
beta_gmm


### Question 4. Calculate the gradient / Jacobian (d Jn / d beta) by hand

x1 = x[,1]
x2 = x[,2]
x3 = x[,3]
x32 = x3^2

gradient = function(beta) {
  u<-y-x%*%beta
  Jnb1 = (1/n)*(
    2 * sum(x1*u) * sum(-x1*x1) +
      2 * sum(x2*u) * sum(-x1*x2) +
      2 * sum(x3*u) * sum(-x1*x3) +
      2 * sum(x32*u) * sum(-x1*x32)
  )
  Jnb2 = (1/n)*(
    2 * sum(x1*u) * sum(-x2*x1) +
      2 * sum(x2*u) * sum(-x2*x2) +
      2 * sum(x3*u) * sum(-x2*x3) +
      2 * sum(x32*u) * sum(-x2*x32)
  )
  Jnb3 = (1/n)*(
    2 * sum(x1*u) * sum(-x3*x1) +
      2 * sum(x2*u) * sum(-x3*x2) +
      2 * sum(x3*u) * sum(-x3*x3) +
      2 * sum(x32*u) * sum(-x3*x32)
  )
  grad = c(Jnb1, Jnb2, Jnb3)
  return(grad)
}

### gradient(beta_gmm)
### Remember that beta_gmm was found by minmizing Jn, so the gradient here should be near zero

steps <- 10
record_table <- as.data.frame(matrix(numeric(70), nrow = 10))
colnames(record_table) <- c("function", "beta1", "beta2", "beta3", "gradient1",
                            "gradient2", "gradient3")

b0 <- c(0,0,0) #initial parameters
A0 <- diag(3) #initial updating matrix

for (i in 1:10){
  q0 <- gradient(b0) #gradient at beta0

  b1 <- as.vector(b0 - A0%*%q0) #beta update
  q1 <- gradient(b1)
  
  g0 <- q1 - q0 #difference between the gradients at beta0 and beta1
  p0 <- b1 - b0 #difference between beta0 and beta1
  h0 <- p0/(as.numeric(t(p0)%*%g0)) - (A0%*%g0)/(as.numeric(t(g0)%*%A0%*%g0))
  
  A1 <- A0 + (p0%*%t(p0))/(as.numeric(t(p0)%*%g0)) - (A0%*%g0%*%t(g0)%*%A0)/as.numeric((t(g0)%*%A0%*%g0)) + (as.numeric(t(g0)%*%A0%*%g0))*(h0%*%t(h0)) #updating matrix update
    
  record_table[i,"function"] <- Jn(b1)
  record_table[i,c("beta1","beta2","beta3")] <- b1
  record_table[i,c("gradient1","gradient2","gradient3")] <- q1
  
  b0 <- b1
  A0 <- A1
}

print(record_table)

### In Cameron and Trivedi's book, the update rules are slightly different, with
### difference in the signs. I tried with both the book rule and the rule in the
### problem set, but the parameters diverged. The only way i found for the 
### parameters to converge is plugging a minus sign in the third term of A1.

### Question 5. Now, we perform a bootstrap to find our empirical confidence
### interval.

Jn_boot<-function(beta,data){
  x<-as.matrix(data[1:3])
  y<-as.matrix(data[4])
  x32 <- x[,3]^2
  u<-y-x%*%beta
  m1<-(1/n*(t(x)%*%u))
  m2<- (1/n*t(x32)%*%u) # let's use a square term in income
  M<-as.matrix(rbind(m1,m2))
  return(n*t(M)%*%M)
}

B <- 1000
betas_boot <- matrix(0, nrow = B, ncol = 3)

set.seed(1)

for (i in 1:B){
  sample_i <- sample(1:n,size = n, replace = TRUE)
  bootsample <- df[sample_i,]
  solution <- optim(par = c(0,0,0),Jn_boot, data = bootsample,method = 'BFGS')
  beta_i <- solution$par
  betas_boot[i,] <- beta_i
}

### With our bootstrap estimates in hands, we can find out the percentile intervals
### for each parameter by finding the quantiles for 0.05/2 and 1-0.05/2, as follows:

pi_1 <- quantile(betas_boot[,1], probs = c(0.025,0.975))
pi_2 <- quantile(betas_boot[,2], probs = c(0.025,0.975))
pi_3 <- quantile(betas_boot[,3], probs = c(0.025,0.975))

