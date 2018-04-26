#Day.6_R
#Kholofelo Sethebe
# Today we will be forcusing on confidence intervals and Data transformation
#26 April 2018

# Chapter 10 Confidence intervals -----------------------------------------

# Load Data ---------------------------------------------------------------

library(tidyverse)
library(rcompanion)
Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# by bootstrapping
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95, digits = 3, R = 10000, boot = TRUE,
              traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3, R = 10000, boot = TRUE,
              traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

#Produce a graph of mean and confidence intervals, plot the mean as a dot and around it plot the confidence interval
#Might be in an exam

#naming 
population <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3, R = 10000, boot = TRUE,
                            traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)
#How its done
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3)

dat1  
  
  
  
  


ggplot(data = population, aes(x = Teacher, y = Mean, colour = sex)) + 
  geom_line() +
  geom_point()

# Create The Graph --------------------------------------------------------
ggplot(data = dat1, aes(y = Mean, x = Sex)) + 
  geom_point(aes(colour = Teacher)) + 
  geom_errorbar(aes(ymin = Mean - Trad.lower, 
                    ymax = Mean + Trad.upper, colour = Teacher)) +
  coord_flip() +
  facet_wrap(~Teacher)
#add coordflip, to flip your graphs

# Chapter 11 --------------------------------------------------------------


# Testing assumptions or: How I learned to stop worrying and transform the data----------------------

#Let us recap—the most important assumptions that we need to make sure are met before doing t-tests, 
#ANOVAs or linear regressions are:

#The dependent variable must be continuous.
#The data must be independent of each other.
#The data most be normally distributed.
#The data must be homoscedastic.
library(ggplot2)
# LOG TRANSFORM -----------------------------------------------------------
# Natural log 
# Log10
# Cube root
# Square root transform
 library(ggplot2)

# Bootstrapping: A modern approach ----------------------------------------
groupwiseMean(Steps ~ Teacher + Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

#in the Shapiro test, there is no difference in variance and the p-value is > 0.05 
#for the data to be normal.


library(bindrcpp)
data2<-data %>% 
  mutate(log10.step = log10(Steps),
         ln.step = log(Steps),
         cube.step = Steps^(1/3),
         sqrt.step = sqrt(Steps))


geom_point(aes(colour = Teacher))+
  geom_errorbar(aes(ymin = Mean - Trad.lower, ymax = Mean + Trad.upper,
                    colour = Teacher))+
  coord_flip()+
  facet_wrap(~Teacher)

#create log transform(log base 10) data and then square root and cubic root coloums  
ggplot(data = data2, aes(x=log10.step, colour = Teacher))+
  geom_histogram()+
  facet_wrap(~Teacher*Sex)

hist(data = data2$log10.step)

dat2<-data %>% 
  mutate(log10.step = log10(Steps),
         ln.step = log(Steps),
         cube.step = Steps^(1/3),
         sqrt.step = sqrt(Steps)) %>% 
  select(-Student, -Rating) %>% 
  gather(key = "data.type", value = "trans.data",
         -Sex, -Teacher)

ggplot(data = dat2, aes(x=trans.data, colour = Teacher))+
  geom_histogram(aes(fill = Sex), position = "dodge")+
  facet_grid(data.type ~ Teacher, scales = "free_x")

#OR

plot1 <- ggplot(data = filter(dat2, data.type == "cube.step"), aes(x=trans.data))+
  geom_histogram(aes(fill = Sex), position = "dodge")

plot2 <-  ggplot(data = filter(dat2, data.type == "ln.step"), aes(x=trans.data))+
  geom_histogram(aes(fill = Sex), position = "dodge")

plot3 <- ggplot(data = filter(dat2, data.type == "sqrt.step"), aes(x=trans.data))+
  geom_histogram(aes(fill = Sex), position = "dodge")

plot4 <- ggplot(data = filter(dat2, data.type == "log10.step"), aes(x=trans.data))+
  geom_histogram(aes(fill = Sex), position = "dodge")
library(ggpubr)  
ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, labels = "Auto")  
#select funcrion is used to subset the data



# Some Revision -----------------------------------------------------------

#ANOVA
#Example

#load data
iris.dat <- as.tibble(iris)

iris.dat

#H0: There is no significant difference in petal.width between the three iris species

#first test if the data is normal---->One of the assumptions
##perform the Shapiro Test


shapiro.test(iris$Petal.Width)

#or

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))

#Because the p-values are non-normal, all the data can be assunmed to be non-normal
#Do a Kruskal-Wallis test instead of an ANOVA

kruskal.test(Petal.Width ~ Species, data = iris)

#p-value<0.05, therefore we reject the null hypothesis. 




#practice
dat2 <- data %>% 
  mutate(in.step = log(Steps),
         ln.step = log10(Steps),
         cube.step = Steps^(1/3),
         sqrt.step = sqrt(Steps)) %>% 
  select(-Student, -Rating) %>%
  gather(key = "data.type", value =  "trans.data", 
         -Sex, -Teacher)
  
ggplot(data = dat2, aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex, position = dodge)) + 
  facet_grid(data.type ~ Teacher, scale = "free_x")

plt1 <- ggplot(data = filter(dat, data.type = "cube.step"), aes(x = trans.data)) +
  geom_histogram(aes(fill = Sex, position = dodge))


shapiro.test(iris$Petal.Width)

shapiro-walk

