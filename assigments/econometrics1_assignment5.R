### Econometrics 1 - Problem set 5
### Luciano Fabio Busatto Venturim

library(tidyverse)
# install.packages("sjlabelled")
library(sjlabelled)
# install.packages("ggpubr")
library(ggpubr)
#install.packages("plm")
library(plm)
library(lmtest)

### Question 1. Replicate D. Card and A. Kruger (1994) main findins following 
### Philipp Leppert's website https://rpubs.com/phle/r_tutorial_difference_in_differences

### First, we clean and organize the data.

# Temporary file and path
tfile_path <- tempfile()
tdir_path <- tempdir()

# Download zip file
download.file("http://davidcard.berkeley.edu/data_sets/njmin.zip", 
              destfile = tfile_path)

# Unzip
unzip(tfile_path, exdir = tdir_path)

# Read codebook
codebook <- read_lines(file = paste0(tdir_path, "/codebook"))

# Generate a vector with variable names
variable_names <- codebook %>%
  `[`(8:59) %>% # Variablennamen starten bei Element 8 (sheet)
  `[`(-c(5, 6, 13, 14, 32, 33)) %>% # Elemente ohne Variablennamen entfernen
  str_sub(1, 8) %>% # längster Variablenname enthält 8 Zeichen
  str_squish() %>% # Whitespaces entfernen
  str_to_lower() # nur Kleinbuchstaben verwenden

# Generate a vector with variable labels
variable_labels <- codebook %>%
  `[`(8:59) %>% # variable names start at element 8 (sheet)
  `[`(-c(5, 6, 13, 14, 32, 33)) %>% # remove elements w/o variable names
  sub(".*\\.[0-9]", "", .) %>%
  `[`(-c(5:10))  %>% # these elements are combined later on
  str_squish() # remove white spaces

# Region
variable_labels[41] <- "region of restaurant"

# Read raw data
data_raw <- read_table2(paste0(tdir_path, "/public.dat"),
                        col_names = FALSE)

###_______________________________________________________________________###

# Add variable names
data_mod <- data_raw %>%
  select(-X47) %>% # remove empty column
  `colnames<-`(., variable_names) %>% # Assign variable names
  mutate_all(as.numeric) %>% # treat all variables as numeric
  mutate(sheet = ifelse(sheet == 407 & chain == 4, 408, sheet)) # duplicated sheet id 407

# Process data (currently wide format)
data_mod <- data_mod %>%
  # chain value label
  mutate(chain = case_when(chain == 1 ~ "bk",
                           chain == 2 ~ "kfc",
                           chain == 3 ~ "roys",
                           chain == 4 ~ "wendys")) %>%
  # state value label
  mutate(state = case_when(state == 1 ~ "New Jersey",
                           state == 0 ~ "Pennsylvania")) %>%
  # Region dummy
  mutate(region = case_when(southj == 1 ~ "southj",
                            centralj == 1 ~ "centralj",
                            northj == 1 ~ "northj",
                            shore == 1 ~ "shorej",
                            pa1 == 1 ~ "phillypa",
                            pa2 == 1 ~ "eastonpa")) %>%
  # meals value label
  mutate(meals = case_when(meals == 0 ~ "none",
                           meals == 1 ~ "free meals",
                           meals == 2 ~ "reduced price meals",
                           meals == 3 ~ "both free and reduced price meals")) %>%
  # meals value label
  mutate(meals2 = case_when(meals2 == 0 ~ "none",
                            meals2 == 1 ~ "free meals",
                            meals2 == 2 ~ "reduced price meals",
                            meals2 == 3 ~ "both free and reduced price meals")) %>%
  # status2 value label
  mutate(status2 = case_when(status2 == 0 ~ "refused second interview",
                             status2 == 1 ~ "answered 2nd interview",
                             status2 == 2 ~ "closed for renovations",
                             status2 == 3 ~ "closed permanently",
                             status2 == 4 ~ "closed for highway construction",
                             status2 == 5 ~ "closed due to Mall fire")) %>%
  mutate(co_owned = if_else(co_owned == 1, "yes", "no")) %>%
  mutate(bonus = if_else(bonus == 1, "yes", "no")) %>%
  mutate(special2 = if_else(special2 == 1, "yes", "no")) %>%
  mutate(type2 = if_else(type2 == 1, "phone", "personal")) %>%
  select(-southj, -centralj, -northj, -shore, -pa1, -pa2) %>% # now included in region dummy
  mutate(date2 = lubridate::mdy(date2)) %>% # Convert date
  rename(open2 = open2r) %>% #Fit name to wave 1
  rename(firstinc2 = firstin2) %>% # Fit name to wave 1
  sjlabelled::set_label(variable_labels) # Add stored variable labels

###_______________________________________________________________________###

# Structural variables
structure <- data_mod %>%
  select(sheet, chain, co_owned, state, region)

# Wave 1 variables
wave1 <- data_mod %>%
  select(-ends_with("2"), - names(structure)) %>%
  mutate(observation = "February 1992") %>%
  bind_cols(structure) 

# Wave 2 variables
wave2 <- data_mod %>%
  select(ends_with("2")) %>%
  rename_all(~str_remove(., "2"))  %>%
  mutate(observation = "November 1992") %>%
  bind_cols(structure) 

# Final dataset
card_krueger_1994 <- bind_rows(wave1, wave2) %>%
  select(sort(names(.))) %>% # Sort columns alphabetically
  sjlabelled::copy_labels(data_mod) # Restore variable labels

###_______________________________________________________________________###

card_krueger_1994_mod <- card_krueger_1994 %>%
  mutate(emptot = empft + nmgrs + 0.5 * emppt,
         pct_fte = empft / emptot * 100) # full-time equivalent employment

###_______________________________________________________________________###

### Proceeding to descriptive statistics

### The relative shares of each fast-food chain within each state:

card_krueger_1994_mod %>%
  select(chain, state) %>%
  table() %>%
  prop.table(margin = 2)  %>%
  apply(MARGIN = 2,
        FUN = scales::percent_format(accuracy = 0.1)) %>%
  noquote

### Analizing the menas for each state at each date to see if they match the 
### values in the paper

card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

###_______________________________________________________________________###

### Next, replicate the distribution of wages in each state for each period

hist.feb <- card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha=0.5, position = "dodge", bins = 23) +
  labs(title = "February 1992", x = "Wage range", y = "Percent of stores", fill = "") +
  scale_fill_grey()

hist.nov <- card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha = 0.5, position = "dodge", bins = 23) +
  labs(title = "November 1992", x="Wage range", y = "Percent of stores", fill="") +
  scale_fill_grey()

ggarrange(hist.feb, hist.nov, ncol = 2, 
          common.legend = TRUE, legend = "bottom")

###_______________________________________________________________________###

### Finally, calculate the treatment effect.

### Obtaining the sample means of each group (observation, state)

differences <- card_krueger_1994_mod %>%
  group_by(observation, state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE))

# Treatment group (NJ) before treatment
njfeb <- differences[1,3]

# Control group (PA) before treatment
pafeb <- differences[2,3]

# Treatment group (NJ) after treatment
njnov <- differences[3,3]

# Control group (PA) after treatment
panov <- differences[4,3]


### the average treatment effect estimate is given by the difference of differences
(njnov-njfeb)-(panov-pafeb) 

### or
(njnov-panov)-(njfeb-pafeb)

###_______________________________________________________________________###

### The average treatment effect can also be found by linear regression.
### First, we create dummy variables that indicates the treatment (D_it in class)
### Here, D_it is the interaction between the time the treatment was implemented
### and which individuals were submitted to it.

card_krueger_1994_mod <- mutate(card_krueger_1994_mod,
                                time = ifelse(observation == "November 1992", 1, 0),
                                treated = ifelse(state == "New Jersey", 1, 0)
)

### The model is performed

did_model <- lm(emptot ~ time + treated + time:treated, data = card_krueger_1994_mod)
summary(did_model)

### The coefficient of the interaction variable is the average treatment effect
### and it is the same as before

did_model$coefficients["time:treated"]

### This is a LDSV estimator. We could also use fixed effects regression.

# Declare as panel data
panel <- pdata.frame(card_krueger_1994_mod, "sheet")

# Within model
did.reg <- plm(emptot ~ time + treated + time:treated, 
               data = panel, model = "within")

# obtain clustered standard errors
coeftest(did.reg, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"))


# rm(list = ls())
###_______________________________________________________________________###

library(haven)

### Question 2. We follow the Patrick Kline's paper "The impact of juvenile curfew
### laws on arrests of youth and adults", published in 2012.

# We first read the data
q2_data <- read_dta("data/econometrics1_assignment5_data.dta")

### Question 2. Item 1. In this item, we construct the variables D_{it}^k = 
### I(t = e_i + k), where e_i is the year the event happened for individual i. As
### in the paper, we consider only k = -6, ..., 6

q2_data_mod <- q2_data %>%
  mutate(Q_6 = if_else(t <= -6, 1, 0), #endpoints
         Q_5 = if_else(t == -5, 1, 0),
         Q_4 = if_else(t == -4, 1, 0),
         Q_3 = if_else(t == -3, 1, 0),
         Q_2 = if_else(t == -2, 1, 0),
         Q_0 = if_else(t == 0, 1, 0),
         Q1 = if_else(t == 1, 1, 0),
         Q2 = if_else(t == 2, 1, 0), 
         Q3 = if_else(t == 3, 1, 0),
         Q4 = if_else(t == 4, 1, 0),
         Q5 = if_else(t == 5, 1, 0),
         Q6 = if_else(t >= 6, 1, 0)) #endpoints 

### Notice that we did not include Q_it^(-1), as asked in the question. This 
### means that we normalized beta_(1) to zero.


### Question 2. Item 2. We assume a two-way error components fixed effects
### model such as y_it = b_1*D_it^(-6) + ... + b_13*D_it_6 + u_i + v_t + e_it.
### Similarly to Question 1, we use the plm function to estimate the model

q2_did.reg <- plm(lnarrests ~ Q_6 + Q_5 + Q_4 + Q_3 + Q_2 + Q_0 +
                 Q1 + Q2 + Q3 + Q4 + Q5 + Q6, 
               data = q2_data_mod, 
               index = c("city", "year"), model = "within", effect = "twoways")

# obtain clustered standard errors
q2_did_rob <- coeftest(q2_did.reg, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"))

q2_did_ci <- coefci(q2_did.reg, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"),
  level = 0.95)

### To check if the point estimators of the coefficients are what we expect, 
### we can use the one-way error components model and add dummies for each year.

q2_did.reg_test <- plm(lnarrests ~ Q_6 + Q_5 + Q_4 + Q_3 + Q_2 + Q_0 +
                    Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + factor(year), 
                  data = q2_data_mod,
                  index = (c("city","year")), model = "within", effect = "individual")

all.equal(q2_did.reg$coefficients,
          q2_did.reg_test$coefficients[c("Q_6", "Q_5", "Q_4", "Q_3", "Q_2", 
                                         "Q_0", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6")])

### Question 2. Item 3. Next, we plot the coefficients estimates 
### with their confidence intervals. First, we get the values of the confidence
### intervals and estimates

values <- data.frame(-6:6)
colnames(values) <- "time"
values["estimate"] <- c(q2_did.reg$coefficients[1:5], 0, q2_did.reg$coefficients[6:12])
values["upper_ci"] <- c(q2_did_ci[1:5,1], 0, q2_did_ci[6:12,1])
values["lower_ci"] <- c(q2_did_ci[1:5,2], 0, q2_did_ci[6:12,2])

ggplot(values, aes(x = time, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) + 
  geom_hline(yintercept = 0) +
  labs(title = "Treatment effect of curfews on log of arrests") +
  scale_x_discrete(limits = values$time) 

### If the enactment was in fact "random", we would expect the coefficients of
### the dummy variables Q_6 to Q_2 to be around zero. In the periods subsequent
### to the treatment, the average of log arrests seems to fall, and as we can see
### by the confidence interval the variation does not seem to be randomly generated,
### since almost all of the intervals are below zero.


###_______________________________________________________________________###

library(AER)
### Question 3. Item 1. Creating the lagged variables and the first difference

q3_data <- q2_data_mod %>%
  dplyr::select(c("year", "city",  "enacted", "lnarrests","Q_0", "Q1")) %>%
  group_by(city) %>%
  dplyr::mutate(lnarrests_diff = lnarrests - dplyr::lag(lnarrests, n=1)) %>%
  dplyr::mutate(lnarrests_diff_lag = dplyr::lag(lnarrests_diff, n=1)) %>%
  dplyr::mutate(lnarrests_lag = dplyr::lag(lnarrests, n=1)) %>%
  dplyr::mutate(Q_0_diff = Q_0 - dplyr::lag(Q_0, n=1)) %>%
  dplyr::mutate(Q1_diff = Q1 - dplyr::lag(Q1, n=1))

### We then run a linear regression of lnarrests_diff on a lag of 
### lnarrests_diff, Q_0 diff, Q_1_diff, and time dummies.

q3_model <- lm(data = q3_data, 
                formula = lnarrests_diff ~ lnarrests_diff_lag + 
                  Q_0_diff + 
                  Q1_diff + factor(year))

### We could also estimate the model directly with the plm function and compare

q3_model_test <- plm(lnarrests ~ lnarrests_lag + Q_0 + Q1 + factor(year), 
                 data = q3_data,
                 index = (c("city","year")), model = "fd", effect = "individual")

all.equal(as.vector(q3_model$coefficients[c('lnarrests_diff_lag', 'Q_0_diff', 'Q1_diff')]),
          as.vector(q3_model_test$coefficients[c('lnarrests_lag', 'Q_0', 'Q1')]))

 
### Question 3. Item 2. We perform three IV regressions, one using a 2-lagged
### log of arrests, one using a 2-lagged first difference of log of arrests,
### and one using both.

q3_data <- q3_data %>%
  group_by(city) %>%
  dplyr::mutate(lnarrests_diff_2lag = dplyr::lag(lnarrests_diff, n=2)) %>%
  dplyr::mutate(lnarrests_2lag = dplyr::lag(lnarrests, n=2))

iv_lag <- ivreg(lnarrests_diff ~ lnarrests_diff_lag + 
                Q_0_diff + Q1_diff + factor(year) | 
                Q_0_diff + Q1_diff + factor(year) + lnarrests_2lag,
              data = q3_data)

iv_diff <- ivreg(lnarrests_diff ~ lnarrests_diff_lag + 
                Q_0_diff + Q1_diff + factor(year) | 
                Q_0_diff + Q1_diff + factor(year) + lnarrests_diff_2lag,
              data = q3_data)

iv_both <- ivreg(lnarrests_diff ~ lnarrests_diff_lag + 
                Q_0_diff + Q1_diff + factor(year) | 
                Q_0_diff + Q1_diff + factor(year) + lnarrests_2lag + 
                lnarrests_diff_2lag,
              data = q3_data)
