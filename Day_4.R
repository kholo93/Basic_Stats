#Day.4_R
#Kholofelo Sethebe
#Today we will be focusing on ANOVA
#19 April 2018

#Just reminding our selves what we did. 
#doing the t-test to remind out selves

library(tidyverse)


# t-test ------------------------------------------------------------------
# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#t-test
t.test(weight ~ Diet, data = chicks_sub)

#We continue with the chicken data. 
#The t-test showed that Diets 1 and 2 resulted in the same chicken masses at the end of the experiment at Day 21.
#What about the other two diets? Our null hypothesis is that, at Day 21,  
#Is there a statistical difference between chickens fed these four diets,
#or do we retain the null hypothesis? 
#The R function for an ANOVA is aov(). 
#To look for significant differences between all four diets on the last day of sampling we use this one line of code:

#Research question: is there a difference in chicken mass attained after
#21 days after the chickens having been feed four different diets?
#
#Null Hypothesis: There is no difference in chicken mass at 21 days after
# having been fed one of the four  diets.

chicks_21 <- chicks %>% 
  filter(Time == 21) 
  
  chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))

  chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
  
#Task: what does the outcome say about the chicken masses? which ones are different from each other?

# Design a graph to explain the ANOVA Results
  chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
  summary(chicks.aov1)
  ggplot(data = chicks_21, aes(x = Diet, y = weight)) + 
    geom_boxplot(aes(fill = Diet), notch = TRUE)
#The the notch overlap, there is no sig difference between the Diets


# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)
#there is a sig difference between the factors when the lowerconfidence itervals is positive 
  

# Load Libraries ----------------------------------------------------------

  library(tidyverse)

# Visuals -----------------------------------------------------------------

chicks_21 <- ChickWeight %>% 
    filter(Time == 21)
  
#ANOVA
  summary(aov(weight ~ Diet, data = chicks_21))
  
#Tukey
  TukeyHSD(aov(weight ~ Diet, data = chicks_21))
  
#Boxplot
  ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) + 
    geom_boxplot(notch = TRUE, colour = "grey50") +
    geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))
  
#segment showing confidence intervals
#Dataframe of segments
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pair <- as.factor(row.names(chicks_Tukey))

ggplot(data = chicks_Tukey) +
  geom_segment(aes(x = pair, xend = pair, y = lwr, yend = upr)) +
  geom_errorbar(aes(x = pair, ymin = lwr, ymax = upr)) +
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed") + 
  coord_flip()
#Don't know how to automatically pick scale for object of type function. 
#Defaulting to continuous.Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  
#: arguments imply differing number of rows: 0, 6) 
  #This is what I get when running the above code, and I am not show what to do.  
    # RWS: You had named your column "pair" not "pairs", which is what you were telling ggplot2 to look for
    # Just that one little extra "s" was the issue

# or juyst plot confidence intervals the base R way...
#shame
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))



# Multiple factors ANOVA --------------------------------------------------------

#what is the relationship of Diet and Time of the chickens

#H0: There is no change in chicken mass (kg) from day 1 to day 21.
chicks_1_21 <- ChickWeight %>% 
  filter(Time %in% c(0,21))

#Vituatiise the data
ggplot(data = chicks_1_21, aes(x = as.factor(Time), y = weight))+
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

#Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_1_21))

#perform a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_1_21))

#Look at the confidence interval
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_1_21)))

#Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#or simply look at all of the time
#....which is not the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))

#Note the increase in the degrees of freedom for the tie factor
#but no increase for the d.f. for Diet

#Now look at interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#Lets look at the Tukey Results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

#create a line graph to help explain this concept 
#first create mean values by Time and Diet
chick_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weighted.mean = mean(weight, na.rm = T))

#The visualise it
ggplot(data  = chick_mean, aes(x = Time, y = weighted.mean, colour = Diet)) + 
  geom_line(size = 2) + 
  geom_point(shape = 15, size = 5) 


# Non parametric tests ----------------------------------------------------

#But what if...
#...we dont have normal data

# For the t-test we rather use wilcox rank sum tets
wilcox.test()# and then one fills this in the same as for t.test ()

# And now for the Kruskall-Wallis
kruskal.test(weight ~ Diet, data = chicks_1_21)

#Load this for a non parametric post-hot test
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_1_21)


#Tukey multiple comparisons of means
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
 
