#Day.3_R
#Kholofelo Sethebe
#Cullen and Frey Graph
#17 April 2018

library(fitdistrplus)
library(logspline)

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm) #which will give us a histogram 
descdist(r_norm, discrete = FALSE, boot = 100)

#Uniform Data
y <- runif(100)
par(mfrow = c (1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# Chapter 6 ---------------------------------------------------------------

# Inderences about one or two populations ---------------------------------
#tTest you can use when you have a basic hypothesis
#tTest when you are compering 2 thing and Anova when comparing more than 2 things
#The script 
library(tidyverse)

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# Check assumptions -------------------------------------------------------

#normality
#for this we may use the sharpiro-wilk test
shapiro.test(r_dat$dat)

#but that is testing all of the data together
#We must be abit more clever about how we make this test
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
#remember the data are normal when p> 0.05
#the data are non normal when the p < 0.05


# check homoscedasticity --------------------------------------------------

# The are many ways to check for homoscedasticity
# which is the similarity of variance between sample sets
# for now we will simply say that this assumptions is met when 
# the other variance of the samples are not more than 2 - 4 times greater
# than the other

# check everything at once..
# wrong
var(r_dat$dat)

#or do it the tidy
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
  
#this is how you test the normality of a distribution. the shapira test spits 2 velues but we are only interested in the second value (p value), we are only asking for the P value to be returned


# One-sample t-tests ------------------------------------------------------
#WE NEED TO KNOW THIS, SO READ OVER IT AGAIN
# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
#PERHAPS A VISUALISATION?

#RUN THE TEST
t.test(r_dat$dat, mu = 20)
#tell it what the population mean is.

#run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------

#are these data smaller/less than the population mean 
t.test(r_one$dat, mu = 20, alternative = "less")
#or greater
t.test(r_one$dat, mu = 20, alternative = "greater")

#But what about for the larger population mean?
#are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
#what about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")


# two sample t-test -------------------------------------------------------

#create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
#run a default/basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#pick a side
#is A less that B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, ALTERNATIVE = "less")
#is Agreater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, ALTERNATIVE = "greater")
