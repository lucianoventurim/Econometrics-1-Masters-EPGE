### Econometrics 1 - Problem set 6
### Luciano Fabio Busatto Venturim

library(tidyverse)
library(ivpack)

### Question 2. Loading the data.
dataq2 <- read.csv("data/econometrics1_assignment6_data1.csv")

### Question 2. Item 1. Fitting a linear regression of worked on morekids.

modelq2 <- lm(formula = worked ~ morekids, data = dataq2)

summary(modelq2)

### Question 2. Item 2.

ivq2 <- lm(formula = morekids ~ samesex, data = dataq2)

summary(ivq2)

### Question 2. Item 3. Estimating a IV model 

modelq2_iv <- ivreg(formula = worked ~ morekids | samesex, data = dataq2)

summary(modelq2_iv)


###_________________________________________________________________________###
#install.packages("stargazer")
library(stargazer)

### Question 3. Loading the data
dataq3 <- read.csv("data/econometrics1_assignment6_data2.csv")

### Question 3. Item 1. Creating the buckets. It is a way to group by each integer
dataq3$psu_int <- floor(dataq3$psu)

dataq3_collapsed <- dataq3 %>%
  group_by(psu_int) %>%
  summarise(entercollege_m = mean(entercollege),
            hsgpa_m = mean(hsgpa),
            privatehs_m = mean(privatehs),
            hidad_m = mean(hidad),
            himom_m = mean(himom))
ggplot(data = dataq3_collapsed) +
  geom_line(mapping = aes(x = psu_int, y = entercollege_m))
ggplot(data = dataq3_collapsed) +
  geom_line(mapping = aes(x = psu_int, y = hsgpa_m))
ggplot(data = dataq3_collapsed) +
  geom_line(mapping = aes(x = psu_int, y = privatehs_m))
ggplot(data = dataq3_collapsed) +
  geom_line(mapping = aes(x = psu_int, y = hidad_m))
ggplot(data = dataq3_collapsed) +
  geom_line(mapping = aes(x = psu_int, y = himom_m))
  
### Question 3. Item 2a. We will use only the data point of $dataq3_collapsed for
### which 465 <= psu <= 485.

dataq3_collapsed$psu_line <- dataq3_collapsed$psu_int-475

modelq3_i2_college <- lm(formula = entercollege_m ~ psu_line + I(psu_line >= 0) +
                   psu_line*I(psu_line >= 0), 
                   data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -10)&
                                             (dataq3_collapsed$psu_line <= 10),])

modelq3_i2_hsgpa <- lm(formula = hsgpa_m ~ psu_line + I(psu_line >= 0) +
                           psu_line*I(psu_line >= 0), 
                         data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -10)&
                                                   (dataq3_collapsed$psu_line <= 10),])

modelq3_i2_himom <- lm(formula = himom_m ~ psu_line + I(psu_line >= 0) +
                           psu_line*I(psu_line >= 0), 
                         data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -10)&
                                                   (dataq3_collapsed$psu_line <= 10),])

modelq3_i2_hidad <- lm(formula = hidad_m ~ psu_line + I(psu_line >= 0) +
                           psu_line*I(psu_line >= 0), 
                         data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -10)&
                                                   (dataq3_collapsed$psu_line <= 10),])

stargazer(modelq3_i2_college,modelq3_i2_hidad,
          modelq3_i2_himom,modelq3_i2_hsgpa)


### Question 3. Item 2b. Bandwidth equals to 20 points.

modelq3_i2_college2 <- lm(formula = entercollege_m ~ psu_line + I(psu_line >= 0) +
                           psu_line*I(psu_line >= 0), 
                         data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -20)&
                                                   (dataq3_collapsed$psu_line <= 20),])

modelq3_i2_hsgpa2 <- lm(formula = hsgpa_m ~ psu_line + I(psu_line >= 0) +
                         psu_line*I(psu_line >= 0), 
                       data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -20)&
                                                 (dataq3_collapsed$psu_line <= 20),])

modelq3_i2_himom2 <- lm(formula = himom_m ~ psu_line + I(psu_line >= 0) +
                         psu_line*I(psu_line >= 0), 
                       data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -20)&
                                                 (dataq3_collapsed$psu_line <= 20),])

modelq3_i2_hidad2 <- lm(formula = hidad_m ~ psu_line + I(psu_line >= 0) +
                         psu_line*I(psu_line >= 0), 
                       data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -20)&
                                                 (dataq3_collapsed$psu_line <= 20),])

stargazer(modelq3_i2_college2,modelq3_i2_hidad2,
          modelq3_i2_himom2,modelq3_i2_hsgpa2)


### Question 3. Item 2c. Collecting the coefficient on psu_line for different 
### bandwidths.

beta_3 <- numeric(50-5+1)

for (i in 5:50) {
  lm_i <- lm(formula = entercollege_m ~ psu_line + I(psu_line >= 0) +
               psu_line*I(psu_line >= 0), 
             data = dataq3_collapsed[(dataq3_collapsed$psu_line >= -i)&
                                       (dataq3_collapsed$psu_line <= i),])
  beta_3[i-4] = lm_i$coefficients[3]
}

plot(x = 5:50, y = beta_3, pch = 20, main = "ATE for different bandwidths",
     xlab = "bandwidth")
