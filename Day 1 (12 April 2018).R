# Day_1.R
#The first day of the stats class
# The purpose of this is to practice some of the concepts that will encounter
# 12 April 2018


# load libraries ----------------------------------------------------------

library(tidyverse)

# Integers ----------------------------------------------------------------

integer_r <- as.integer(seq(5, 22, by = 10))

#look at a brief summery of them
summary(integer_r)
#this are the normal (descrete)data


# Continuous Data ---------------------------------------------------------

#Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10)


# Dates -------------------------------------------------------------------

#one may perform some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-12")
#or for example 
seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#there is much more
summary(date_r)


# Data frames -------------------------------------------------------------

#create the base dataframe
df_r <- data.frame(integers = integer_r, 
                   numeric = numeric_r,
                   dates = dates_r)
                     
 #Then upgrade it to a tibble
df_r <- as_tibble(df_r)
summary(df_r)


# Categories --------------------------------------------------------------

#Electronics
elec_r <- as.factor(c("laptop",
                      "desktops",
                      "cell phones"))

# people
people_r <- as.factor(c("funny hair",
                      "beautiful",
                      "beanies"))

# colour
colour_r <- as.factor(c("red", "blue"))

#run a summary of factors
summary(colour_r)


# Ordinal data ------------------------------------------------------------

#Here we still have qualitative data
#but with some sort of order

colour_qual <- ordered(c("blue", "green",
                            "yellow", "orange", 
                            "red"),
                          levels = c("blue", "green",
                                     "yellow", "orange",
                                     "red"))



# Binary data -------------------------------------------------------------

#these are generally represented as: TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)



# Characters --------------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")
summary(sites_r)


# Missing Values ----------------------------------------------------------

#Number of eggs recorded in a nest
#The NA shows a nest that was not able to be sampled
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
# A summary
summary(chicks_nest)
#The mean
mean(chicks_nest)
#The STD
sd(chicks_nest)



# Chapter 3 ---------------------------------------------------------------

# Descriptive statistics --------------------------------------------------

#First create a data frame
chicks <- as_tibble(ChickWeight)

#count the data
chicks %>% 
  summarise(chicken_count = n())
#or
nrow(chicks)



# Measure of central tendency ---------------------------------------------

# Calculate the mean weight
chicks %>% 
  summarise(mean_wt = mean(weight))

#Be more specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

#Visulise the density of the data
ggplot(data = filter(chicks, Time == 21),
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = mean(weight),
                 colour = Diet), size = 1.5, linetype = "dashed")



# SKEWNESS ----------------------------------------------------------------

#calculate the numeric value
#first load library
library(e1071)

#compare difference in mean and median against skewness
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

#positive values are left skew and negative values are right skew


# kurtosis ----------------------------------------------------------------

#calculate the kurtosis of the tails of a distribution 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))


# Measures of Variability ----------------------------------------------------------------

#Below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75))











