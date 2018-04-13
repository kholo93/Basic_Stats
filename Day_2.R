# Day_2.R
#The seconda day of the stats class
# Data visualisation and distribution 
# 13 April 2018


# load libraries ----------------------------------------------------------

library(tidyverse)

# Manual calculations -----------------------------------------------------

#the mean
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                    sample = "A")

#QUICK VITUALISATION
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

#The mean
#sum of all the points
#divided by 
#the number of all the points
r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))
            
#if you want the number of samples, the function is n

#the median
#brute force with base R
r_dat$dat[(length(r_dat$dat)+1)/2]
#or use tidy
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)
#or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))

#Figure out how to order your data from smallest to biggest and then just take the middle (calculating the median)

#The variance
#the sum of
#each value minus the mean
   #squared
#divided by
   #the count of samples minus one 
r_dat %>%
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            #or use the build in function
            r_var_func = var(dat))
  
 # The Standard deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

  
  
  summarise(r_sum = sum(dat),
            r_mean = mean(dat))
  

# Exercise 1 --------------------------------------------------------------

  #notice how the data summary for chicken weights contained within
  #wt_summary is very similar to the summary returned for weight
  #when we apply summary(chicks),please use the summarise() approach
  #and construct a data summary with exactly the same summary statistics 
  #for weight as that which summary() returns.
#we start off with the data set 
  
summary(ChickWeight$weight)

 ChickWeight %>%
   summarise(min_weight = min(weight),
             quart_1 = quantile(weight, 0.25),
             med_weight = median(weight),
             mean_weight = mean(weight),
             quart_3 = quantile(weight, 0.75),
             max_width = max(weight))


# Visualisations ----------------------------------------------------------

#first load our libraries
#these few packages contain most functions necessary
#to make publication ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis) 

#load our SA time data
sa_time <- read_csv("SA_time.csv")

# Edit our data
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("cape town", "George", "PE"), times = 6),
         rep("joburg", 2)))

#create long data
sa_long <- sa_time %>% 
  group_by(human) %>% 
  gather(key = "time_type", value = "minutes", -human)
 
# Qualitative -------------------------------------------------------------
 
# create a count of qualitative values
sa_count <- sa_long %>%
   count(time_type) %>% 
   mutate(prop = n/sum(n))
          
 # Stacked bar graphs
 ggplot(data = sa_count, aes(x = "", y =n, fill = time_type)) + 
   geom_bar(width = 1, stat = "identity") + 
   labs(title = "stacked bar graph", subtitle = "cumulative sum",
        x = NULL, y = "count") + 
   theme_minimal()
 
 # Stacked proportion bar graphs
 ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) + 
   geom_bar(width = 1, stat = "identity") +
   scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00))
   labs(title = "stacked bar graph", subtitle = "relative proportions",
        x = NULL, y = "proportion") + 
   theme_minimal()
 
# A pie chart
   ggplot(data = sa_count, aes(x = "", y =n, fill = time_type)) + 
     geom_bar(width = 1, stat = "identity") + 
     labs(title = "pie chart", subtitle = "but why though",
          x = NULL, y = NULL) + 
     coord_polar("y", start = 0) + 
     theme_minimal()
   
     
     
# side by side bar graph (the graph with separated bars)
     ggplot(data = sa_long, aes(x = time_type, fill = time_type)) + 
       geom_bar(show.legend = FALSE) + 
       labs(title = "sa_time data", subtitle = "nothing really",
            x = "time_type" , y = "Time(minutes)") + 
       theme_minimal()

# Continuous data ---------------------------------------------------------

#Histograms
     ggplot(data = sa_long, aes(x = minutes))+
       geom_histogram()

#oh no!
#lets get rid of the one value...
sa_clean <- sa_long %>% 
  filter(minutes < 300)

#try again

ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge") + 
  facet_wrap(~time_type, scales = "free_x")
     
#A faceted wrapped histogram
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge") + 
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#Relative proportion histogram
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(y = ..density.. ,fill = time_type),
                 position = "dodge", binwidth = 1) + 
  facet_wrap(~time_type, ncol = 1, scales = "free_x")
     

#BOXPLOT
#when ever we create a figure we always start with inseting a function
#boxplots have both x and y axis
#boxplot is a mixture of both qualitative and quantitative data
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) + 
  geom_boxplot(aes(fill = time_type))


#the black line in the middle of the the histograme is the median value
#the dot is the max value, but why is it a dot? the lines are the tails (whiskers). 
#in between the q1 and the g3 is the interquartile range
#multiply the interq range by 1.5 to find the tail
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) + 
  geom_boxplot(aes(fill = time_type), notch = TRUE)

#there is no statistical difference due to the notch overlaping 

#CALCULATING SUMMARY STATS FOR PLOTTING OVER THE BOXPLOTS
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

#plot these means over the boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) + 
  geom_boxplot(aes(fill = time_type), notch = TRUE) + 
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")


# Relationships -----------------------------------------------------------

sa_time_clean <- sa_time %>% 
  filter(just_now)

#A basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))

#Adding trend lines
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
     
     