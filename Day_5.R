#Day.5_R
#Kholofelo Sethebe
#Today we will be focusing on the Null Hypothesis
#19 April 2018


# Load Library ------------------------------------------------------------

library(tidyverse)
# library(Rmisc) # Unfortunately this overrides many dplyr functions

# Load Data ---------------------------------------------------------------

snakes <- read.csv("snakes.csv") %>% 
  mutate(day = as.factor(day))
#mutate means to change

# Summarise the data ------------------------------------------------------

snakes_summary <- snakes %>%
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))
# when calculate the mean and the sd

snakes$day <- as.factor(snakes$day) 

#$ makes availabe to act upon the data that is available for you to use

# Formulate a hypothesis --------------------------------------------------

#H0: There is no difference in the number of openings from day to day
#H1: There is a difference in the number of openings from day to day

#Null Hypothesis: (in a statistical test) 
#the hypothesis that there is no significant difference between specified populations,
#any observed difference being due to sampling or experimental error.


# Test a hypothesis -------------------------------------------------------

#First calculate the SE and CI
snakes.summary2 <- Rmisc::summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))
#The visualise it
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day,
                                           y = openings - ci,
                                           yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) + 
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

#But wait, we have two factor, so we need another null hypothesis

#H0:There is no difference between snakes with respect to
 #the number of openings at which they habituate.
#H1:There is no difference between days in terms of 
 #the number of openings at which they habituate.

#Test just the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

#when writing your results you will need to report on the following
#we need the df(degrees of freedom), we need the sum sq, we need the F value, 
#pr=the probability is less than 0.05

#Test both hypothesis
snakes.all.aov <- aov(openings ~ day, data = snakes)
summary(snakes.all.aov)


# Testing assumptions afterwards ------------------------------------------

#First visualise normality of data
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

#Then visualise homoscedasticity of results
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

#Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

#Visualise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) + 
  geom_line(size = 3) + 
  geom_point(size = 4)


# Excercise ---------------------------------------------------------------

#get the moth data from github
#Run a 2 way ANOVA ON THEM

#Load data
moths <- read.csv("moths.csv") %>% 
  gather(key = "trap", value = "count", -Location)

# Summarise the data ------------------------------------------------------
moths_summary <- moths %>%
  group_by(trap) %>% 
  summarise(moths_mean = mean(count),
            moths_sd = sd(count))     #Not runnung

moths.aov <- aov(count ~ trap * Location, data = moths)
summary(moths.aov)

#H0: There is no different in the number of moths counted 
#with respect to different trap types at each location
#H1:  There is difference in the number of moths counted 
#with respect to different trap types at each location

#Visualise homoscedasticity of results
plot(fitted(moths.aov), residuals(moths.aov))

#Check Tukey results
moths.tukey <- TukeyHSD(moths.aov, which = "Location")
plot(moths.tukey)

plt1 <- ggplot(data = moths, aes(x = Location, y = count)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.05, shape = 21)

plt2 <- ggplot(data = moths, aes(x = trap, y = count)) + 
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plt3 <- ggplot(moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap )) +
  geom_jitter(width = 0.05, shape = 21)

library(ggpubr)

ggarrange(plt1, plt2, plt3, labels = "AUTO", ncol = 1, nrow = 3)


# END OF THE Excercise -------------------------------------------------------------


# Regressions -------------------------------------------------------------

#Regressions test the statistical significance of the dependence of 
#one continuous variable on one ormany independent continuous variables.

#residuals(is the difference bwtween observed value and predicted vale(the line) )

#for the explanation of this statistical analysis 
#we are going to use the eruption data from o1' Faithful

#Look at the topw of the data
head(faithful)

#Plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, colour = "hotpink")

#FORM A HYPOTHESIS
#H0: Waiting time doen NOT influence the duration of an eruption
#H1: waiting time does influence the duration of an eruption


# Test a hypothesis -------------------------------------------------------
#linear model = lm

faithful.lm <- lm(eruptions ~ waiting, data = faithful)
#adjusted R explains the amount of variantion.
summary(faithful.lm)


# Chapter 9: Correlations -------------------------------------------------

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv")



# Formulate a hypothesis --------------------------------------------------

#H0: There is no relationship between stipe length and stipe mass
 #For the kelp Ecklonia maxima
#H1: There is a relationship between stipe length and stipe mass
#For the kelp Ecklonia maxima


# Test Hypothesis ---------------------------------------------------------

cor.test(ecklonia$stipe_length, ecklonia$stipe_mass)

#Visualise the data
ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_mass)) + 
  geom_point()


# Run hecka tests at once -------------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor


# spearman rank test ------------------------------------------------------

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))

#The run the spearmen test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman") 


# Kendall rank correlation test------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall") 

#Visualise the data
ecklonia_pearson <- cor(ecklonia_sub)
corrplot(ecklonia_pearson, method = "circle")









