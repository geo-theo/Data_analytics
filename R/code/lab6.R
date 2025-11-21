# Fall 2025
# Computer Data Analysis I
# Lab 6
# ====================================================================== #
# The purpose of this lab is to learn how to use the "apply" function    #
# to apply functions to all columns or rows of a matrix or data frame,   #
# use the "cbind" and"rbind" functions to bind together multiple columns #
# or rows into a matrix, use the summaryBy function to compute multiple  #
# statistics by a grouping variable, to use for loops to conduct simu-   #
# lations.  Finally, two simulation examples are presented at the end of #
# the script to illustrate the use of some of these functions.           #
# ====================================================================== #

# Change the default directory
# ============================
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab5")  # Sets working directory
rm(list=ls())                                     # Removes all objects

# ============== #
# apply Function #
# ============== #
View(airquality)         # The "airquality" dataset is a built-in dataset with
                         #   air quality measurements over 5 months in NY.
mean(airquality$Temp)    # Computes the mean temperature over the 153 days
apply(airquality,2,mean) # Finds the mean of each variable by "applying" the
                         #   mean function to each column (2) of "airquality"
apply(airquality,2,      # Computes the mean in each column (2) using a mean
  function(a) mean(a,    #   function that removes the NA's.  The vector "a"
  na.rm=T))              #   is a local object to the function.
apply(airquality,1,      # Computes the number of non-missing values from
  function(a) length(    #   each row (1) of the "airquality" data frame
  a[!is.na(a)]))

# ====================== #
# cbind, rbind functions #
# ====================== #
x = c(1:15)               # Defines the vector x = (1,2, ..., 15)
y = rep(2,15)             # Defines the vector y = (2,2, ..., 2); the "rep"
                          #   function replicates elements Of vectors
dat = cbind(x, y)         # Combines R objects by columns
z <- matrix(c(16,2,17,2,  # Defines a 3x2 matrix z with elements 16, 17, 18,
  18,2),nrow=3,byrow=T)   #   2,2,2.  "byrow=T" builds the matrix row by row.
dat2 <- rbind(dat,z)      # Combines R objects by rows
matrix(dat2, nrow=6)      # Creates a matrix with 5 rows from "dat2" 
matrix(dat2, ncol=18)     # creates a matrix with 18 columns from "dat2"
matrix(dat2, ncol=18,     # creates a matrix with 18 columns from "dat2",
  byrow=T)                #   placing items in the matrix row by row
t(dat2)                   # Transposes the original matrix "dat2"

# ============================== #
# Examples of summaryBy function #
# ============================== #
mouse <- read.csv("mousediets.txt",           # Reads in the mousediets data
                  as.is=F)
install.packages("doBy")                      # installs the "doBy" library
library(doBy)                                 # Loads the "doBy" library

fun = function(x)                             # Function to compute stats by group
  c(min   = min(x,na.rm=T),                   # Computes the minimum value
    Q1    = quantile(x,na.rm=T,0.25,type=5),  # Computes the first quartile
    M     = median(x,na.rm=T),                # Computes the meadian
    Q3    = quantile(x,na.rm=T,0.75,type=5),  # Computes the 3rd quartile
    max   = max(x,na.rm=T),                   # Computes the maximum
    m     = mean(x,na.rm=T),                  # Computes the mean
    sd    = sd(x,na.rm=T),                    # Computes the standard deviation
    n     = length(x[!is.na(x)]),             # Computes the sample size
    Lower = quantile(x,na.rm=T,0.25,type=5)-  # Computes the lower fence of
      1.5 * IQR(x,na.rm=T),                   #   the 1.5*IQR Rule
    Upper = quantile(x,na.rm=T,0.75,type=5)+  # Computes the upper fence of
      1.5 * IQR(x,na.rm=T))                   #   the 1.5*IQR Rule

summaryBy(lifetime~diet,data=mouse,FUN=fun)   # Computes lifetime summaries by diet

cars99 <- read.csv("cars99.txt",as.is=F)      # Data set on car attributes
summaryBy(HwyMPG~Type,data=cars99,FUN=fun)    # Summary of Highway MPG by car type
summaryBy(cbind(HwyMPG,CityMPG)~Type,         # Summaries of multiple variables
  data=cars99,FUN=fun)                        #   by car type using "cbind"

# ========= #
# FOR LOOPS #
# ========= #

# Example 1
# ---------
for (i in 1:10){                     # Loops through values of i=1,2,...,10
  print(i+1)                         # Prints (i+1) for each "i"
}                                    # End of for loop

# Example 2
# ---------
n = length(x)                        # Computes the length of the vector x
for(i in 1:n){                       # Loops through values of i=1,2,...,n
  print(x[i]*y[i])                   # Multiplies x and y element-wise
}                                    # End of for loop
x*y                                  # Easier way to multiply vectors x and y
x %*% y                              # matrix multiplication

# -------------------------------------------------------------------------
# Simulation for a 5-response multiple choice test with 100 questions:
# Suppose you take a multiple choice test of 100 questions where each
# question has 5 possible answers.  If we guess at every question, we would
# expect to get 1/5 or 20 out of 100 questions correct.  How likely is it
# that we would get 30 or more questions correct just by guessing?
# -------------------------------------------------------------------------
nsim <- 10000               # Number of simulations
correct <- rep(0,nsim)      # Defines a vector containing nsim 0's
for(i in 1:nsim){           # For loop from i = 1,2,..., nsim
  dat <- rbinom(100,1,0.2)  # Generates 100 random numbers from a
                            #   Binomial(n=1,p=0.2) distribution
  correct[i] <- sum(dat)    # Calculate no. of correct answers for each simulation
}                           # End of for loop
hist(correct)               # Histogram of number correct in 10000 tests
p_value <- sum(correct>=30)/# Calculates the probability of answering at least
  nsim                      #   30 correct in a sample of 100 questions by just
                            #   guessing

# ==========================================================================
# Example: A cancer cluster is defined by the Centers for Disease Control
#   (CDC) as a "greater than expected number of cancer cases that occurs
#   within a group of people in a geographic area over a period of time."
#   Suppose that a particular cancer has an incidence of 1 case per thousand
#   people per year.  So if you followed a cohort of 1,000 people for 10
#   years, you would expect to see about 10 cases.  If you saw 12 cases,
#   that would not be very surprising, but if you saw more than 20 cases,
#   that might indicate a cancer cluster.
#
# (a) Simulate the number of cancer cases in a cohort of 2000 people
#     followed over 10 years with a probability in any given year of
#     having cancer of 1/1000.  Produce a frequency table and histogram
#     of the number of cancer cases resulting from 10,000 such simulated
#     cohorts.

prob <- 1/1000                        # Prob. one person has cancer in one year
npeople <- 2000                       # Number of people in cohort
nyears <- 10                          # Number of years     
nsim <- 10000                         # Number of simulated cohorts
numcases <- rep(0,nsim)               # Defines a vector of zeros
for (i in 1:nsim){                    # Beginning of for loop
  cancer <- rbinom(npeople*nyears,1,  # Generates an (npeople*nyears)x1 vector of
                   prob)              #   ones and zeros (ones with prob. "prob")
  cancer <- matrix(cancer,ncol=nyears)# Converts "cancer" to npeople x nyears matrix
  cases <- apply(cancer,1,sum)        # Counts the number of ones in each row (1)
                                      #   of "cancer" matrix
  numcases[i] <- sum(cases>0)         # Counts the number of people with at least
                                      #   one case of cancer
}                                     # End of for loop
table(numcases)                       # Creates a table of frequencies of number of
                                      #   cancer cases
hist(numcases,xlab="Number of Cases", # Histogram of the number of cancer cases
  cex.axis=1.5,cex.lab=1.6,cex.main=
  1.4,mgp=c(2.7,1,0),main="Histogram of the Number of Cancer Cases")

(pval <- sum(numcases>=30)/nsim)

# (b) The CDC is obligated to investigate potential causes of a cancer
#     cluster if the likelihood of having the number of cases observed
#     is less than 5%.  When this happens, we say this is a statistically
#     significant result.  According to your simulation, quantify how
#     likely it is to see 30 or more cases of this particular cancer just
#     by chance.  If 30 cases occurred in a real cohort of this type,
#     would the CDC be obligated to investigate?  What if there were 27
#     cases?
# ===========================================================================