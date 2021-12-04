### Econometrics 1 - Problem Set 2
### Luciano Fabio Busatto Venturim
library(tidyverse)
library(sf)
library(geobr)

### Lets first load the data from the census_rj/census_rj_pessoas_ps2.Rds data set

data <- readRDS("data/census_rj/census_rj_pessoas_ps2.Rds")

### We first calculate the population density of each área de ponderação, since
### we will need it in the next questions.
### For that, we use the code provided in /prep_ps2_rafael_geobr.R

pop = data %>% 
  group_by(area.pon) %>%
  summarise(pop=sum(peso.amostra)) %>%
  ungroup 
sum(pop$pop) # how many people in the state of RJ in 2010? plausible?


#  Keep only people who are actively working in the week of Jul 31, 2010; younger than 75, earning more than zero R$
data = data %>% 
  dplyr::filter(work.active==1, age<=75, total.monthly>0) 
summary(data)
dim(data)

#  Generate dataset that contains land area in km2 for each area de ponderacao in RJ
geo <- geobr::read_weighting_area(year = "2010", code_weighting = "RJ") %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(
    area.pon.km2 = sf::st_area(.) / 1e6, 
    km2 = as.numeric(area.pon.km2),
    area.pon = as.numeric(code_weighting)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(area.pon, km2)
sum(geo$km2)  # Bernardo says this should be 43969, not exact but close enough

#  Merge data
data = left_join(data, pop, by="area.pon") # merge in population per area de ponderacao
data = left_join(data, geo, by="area.pon") # merge in km2 per area de ponderacao
data = data %>% dplyr::mutate(density = pop/km2)
summary(data)
#  538 areas de ponderacao in the census data but only 330 in the shapefiles
#  Thanks Rafael

data = data %>% dplyr::filter(!is.na(km2)) # keep only observations with non-NA information on km2 and density


#  Use person weights in peso amostra to correct oversampling/undersampling; take 10 percent random sample
n = dim(data)[1]
set.seed(536547) # fix seed value for random sampling, to make sure everyone gets the same results
xs = sample(1:n, floor(n/10), prob=data$peso.amostra, replace=TRUE)
data = data[xs,] # dataset ready to estimate

### We only want those points for which the monthly earnings are positive. We also
### apply the logarithm and divide into two groups by gender.

logearnings <- log(select(data, total.monthly)) 

logearnings_m <- log(select(filter(data, gender == 1), total.monthly))
logearnings_f <- log(select(filter(data, gender == 2), total.monthly))

h <- 0.6 #the bandwidht
n_m <- length(logearnings_m$total.monthly)
n_f <- length(logearnings_f$total.monthly)

### Question 6. Lets estimate a Kernel density estimate for log of earnings of
### each gender using a uniform kernel.

unif_kernel_m <- function(x0,h){
  value_x0 <- sum(I(abs(logearnings_m$total.monthly-x0)<h))/(2*n_m*h)
  return(value_x0)
}

unif_kernel_f <- function(x0,h){
  value_x0 <- sum(I(abs(logearnings_f$total.monthly-x0)<h))/(2*n_f*h)
  return(value_x0)
}

x <- seq(from = min(logearnings$total.monthly), to = max(logearnings$total.monthly), by = 0.1)
f_hat_f <- sapply(x, function(x){return(unif_kernel_f(x,h))})
f_hat_m <- sapply(x, function(x){return(unif_kernel_m(x,h))})

### Lets plot the histograms and the kernel density estimates.
hist(logearnings$total.monthly, freq=FALSE, main = "Kernel Density Estimates",
     xlab = "Log of earnings", ylim = c(0,max(max(f_hat_f),max(f_hat_m))))
lines(x, f_hat_m, col = "blue")
lines(x, f_hat_f, col = "red")
legend("topleft", legend = c('Male', 'Female'), col = c('blue','red'), cex = 0.8, pch = 15)

### The optimal bandwith is as follows:
std_m <- sd(logearnings_m$total.monthly)
std_f <- sd(logearnings_f$total.monthly)

h_opt_m <- 1.3510*(3/(8*sqrt(pi)*std_m^5))^(-0.2)*n_m^(-0.2)
h_opt_f <- 1.3510*(3/(8*sqrt(pi)*std_f^5))^(-0.2)*n_f^(-0.2)

### With these new bandwidths, we estimate again:

f_hat_opt_m <- sapply(x, function(x){return(unif_kernel_m(x,h_opt_m))})
f_hat_opt_f <- sapply(x, function(x){return(unif_kernel_f(x,h_opt_f))})

### Plotting again:
hist(logearnings$total.monthly, freq=FALSE, main = "Kernel Density Estimates and Optimal Bandwidth", 
     xlab = "Log of earnings", ylim = c(0,max(max(f_hat_opt_f),max(f_hat_opt_m))))
lines(x, f_hat_opt_m, col = "blue")
lines(x, f_hat_opt_f, col = "red")  
legend("topleft", legend = c('Male', 'Female'), col = c('blue','red'), cex = 0.8, pch = 15)


### Question 8. We want to estimate the relationship between the log of population density
### and log of monthly earnings.

logdensities <- log(select(data, density))

### Defining the Epanechnikov kernel and the kernel regression estimator

h1 <- 0.2
h2 <- 0.4
h3 <- 0.8 #three bandwidths


epan_kernel_reg <- function(x0,h){
  # It is more efficient to code the kernel explicitly than to use sapply
  z <- abs((logdensities$density-x0)/h)
  k <- 3/4*(1-z^2)*I(z < 1)
  ky <- k*logearnings$total.monthly
  return(sum(ky)/sum(k))
}

x <- seq(from = min(logdensities$density), to = max(logdensities$density), by = 0.1)

m1 <- sapply(x, function(x){return(epan_kernel_reg(x,h1))})
m2 <- sapply(x, function(x){return(epan_kernel_reg(x,h2))})
m3 <- sapply(x, function(x){return(epan_kernel_reg(x,h3))})

plot(x,m1, type = "l" ,col = "blue", main = "Kernel Regression" ,xlab = "log of density", ylab = "log of earnings")
lines(x,m2, col = "red")
lines(x,m3, col = "black")
legend("topleft", legend = c("h = 0.2", "h = 0.4", "h = 0.8"), col = c("red", "blue", "black"),
       cex = 0.8, pch = 15)

### Question 9. Now, we aim to find the optimal bandwith using cross-validation.
### To ease our job, we create a dataframe with logdensity and logearnings and a
### auxiliary function to estimate the m_tilda_i at x_i.

df <- cbind(logearnings,logdensities)
n <- length(logearnings$total.monthly)

epan_kr_i <- function(i,h){
  #the data
  x_i <- df[i,'density']
  x_not_i <- df[-i,'density']
  y <- df[-i,'total.monthly']
  
  #the epanechnikov kernel
  z <- abs((x_not_i - x_i)/h)
  k <- 3/4*(1-z^2)*I(z < 1)
  
  #the estimator
  ky <- k*y
  mi <- sum(ky)/sum(k)
  return(mi)
}


cv <- function(h, sample){
  # In order to save time, we only do the cross validation using a small random 
  # sample of our data
  m <- sapply(sample, function(i){return(epan_kr_i(i,h))})
  cv <- (1/length(sample))*sum((df$total.monthly[sample]-m)^2)
  return(cv)  
}

h_grid <- seq(0.1, 3, by=0.1)

#ptm <- proc.time()

sample_n <- sample(1:n, size = 100, replace = FALSE)
head(sample_n)

cv_values <- sapply(h_grid, function(x){return(cv(x,sample_n))})
cv_min <- min(cv_values)
pos <- which(cv_values == cv_min)
h_opt <- h_grid[pos] #h_opt is the optimal bandwidth found using cross validation
h_opt
#proc.time() - ptm

### Question 10. Now we re-estimate and plot the kernel estimator with the optimal
### bandwidth.

m_opt <- sapply(x, function(x){return(epan_kernel_reg(x,h_opt))})
plot(x,m_opt,type = 'l', main = "Kernel Regression with CV-Optimal Bandwith")
legend("topleft", legend = parse(text=sprintf('paste(h,\' = %s\')',h_opt)),cex = 0.8, pch = 15)

### For some reason, the CV function seems to be decreasing in h for most of the
### random samples used. Thus, the optimal bandwidth is the smaller one in the grid,
### although it does not seem correct.

