# Fall 2025
# Computer Data Analysis I
# Lab 8
# ============================================================== #
# The purpose of this lab is to provide examples of performing   #
# probability calculations for the binomial, Poisson, and normal #
# distributions, as well as to demonstrate the Central Limit     #
# Theorem through simulation.                                    #
# ============================================================== #

# Change the default directory
# ============================
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab8")  # Sets working directory
rm(list=ls())                                     # Removes all objects

# ================================================================ #
# POISSON DISTRIBUTION #
# Problems 1 & 2: A wildlife biologist is studying turtles that    #
# have been exposed to oil spills in the Gulf of Mexico.  Previous #
# studies have determined that a particular blood disorder occurs  #
# in turtles exposed for a length of time to oil at a rate of 1 in #
# every 8 exposed turtles.  The biologist examines 12 turtles      #
# exposed for a considerable period of time to oil.  If the rate   #
# of occurrence of the blood disorder has not changed, what is the #
# probability of each of the following events?  She finds the      #
# disorder in:                                                     #
#                                                                  #
# (a) None of the 12 turtles.                                      #
# (b) At least 2 of the 12 turtles.                                #
# (c) No more than 4 turtles.                                      #
# ================================================================ #
dbinom(x=0,size=12,prob=1/8) # (a) Computes P(X=0) for Binomial(12,0.125)
1-pbinom(1,12,1/8)           # (b) Computes P(Y>=2) = 1-P(Y<=1) for Bin(12,0.125)
sum(dbinom(0:4,12,1/8))      # (c) Computes P(0<=Y<=4) for Binomial(12,0.125)
pbinom(4,12,1/8)             #     Computes P(0<=Y<=4)

# ======================================================================== #
# Problem 4, HW 6: A firm is considering using the Internet to supplement  #
# its traditional sales methods.  Using data from an industry association, #
# the firm estimates that 1 of every 1200 Internet hits results in a sale. #
# Suppose the firm has 3300 hits per day.  Letting Y = the number of sales #
# by the firm in a randomly selected day, it can be argued that Y is       #
# approximately distributed as a Poisson random variable.  Suppose the     #
# mean number of sales per day is lambda = 4 sales/day.                    #
# ======================================================================== #

# (a) What is the probability of making 3 sales in a day
#     if the average sales rate is 4 per day?  5 sales?                                          #
# ------------------------------------------------------
dpois(3, lambda = 4)         # Computes P(Y=3) for Poisson(lambda=4)
dpois(5, lambda = 4)         # Computes P(Y=5) for Poisson(lambda=4)

# (b) What is the probability of making between 3 and 5 sales
#     inclusive in a day if the average sales rate is 4 per day?
# --------------------------------------------------------------
sum(dpois(3:5, lambda = 4))  # Computes P(3<=Y<=5) for Poisson(lambda=4)

#================================================================= #
# Problem 6: The scores of a reference population on the Wechsler  #
# Intelligence Scale for Children (WISC) are normally distributed  #
# with mean mu = 100 and standard deviation sigma = 15.  Let X =   #
# the WISC score for a randomly chosen child from this population. #
# ================================================================ #
# What is the probability that the WISC score is no higher than 80?
# More than 80?
# -----------------------------------------------------------------
pnorm(q=80,mean=100,sd=15)    # Computes P(X<80) for a N(mu=100,sigma=15)
1-pnorm(80,100,15)            # Computes P(X>80) for a N(mu=100,sigma=15)

# How many standard deviations above the average is a child with a
# WISC score of 135 and how likely is it to score at least 135?
# ----------------------------------------------------------------
z = (135-100)/15           # Computes standardized value z
1- pnorm(z)                # Computes P(X>=135) = P(Z>2.33)

# What is the probability a WISC score is between 90 and 120?
# -----------------------------------------------------------
pnorm(120,100,15)-pnorm(90,100,15) # Computes P(90<X<120) for N(100,15)

# What is the 90th percentile of WISC scores?
# -------------------------------------------
qnorm(p=0.90,mean=100,sd=15)     # Finds 90th percentile (P(X < ???)=0.90)
                                 #   for Normal(mean=100,sd=15)

a = qnorm(0.10,100,15)           # Finds a and b in P(a?< X < b?)=0.80
b = qnorm(0.90,100,15)           #   for Normal(mean=100,sd=15)

# ============================================= #
# Problem 8: Discover the Central Limit Theorem #
# ============================================= #
sal = read.csv("mlbsalaries25.csv") # Reads in the mlbsalaries25.csv file
summary(sal)                        # Summary of the sal dataset
hist(sal$salary,xlab="Salary")      # Histogram of the MLB salaries

nsim <- 2000                        # Number of simulations
n <- 5                              # Sample size
means <- rep(0,nsim)                # Initializes zero vector of length nsim
set.seed(15)                        # Sets the random number seed
for(i in 1:nsim){                   # Begins a loop to be run nsim times
  samp <- sample(sal$salary,n)      # Sample of size n from the salaries
  means[i] <- mean(samp)            # Mean of the n salaries sampled
}                                   # End of for loop

library(ggplot2)
sal$salary2 <- sal$salary/1000
ggplot(data=sal,aes(x=salary2)) +        # Histogram with ggplot
  geom_histogram(color="black",fill="white") + 
  ggtitle("Histogram of MLB Salaries") +
  xlab("Salary (thousands of $)") +
  ylab("Frequency") +
  theme(plot.title=element_text(color="red",size=18,face="bold.italic",hjust=0.5),
        axis.title.x=element_text(color="blue",size=14,face="bold"),
        axis.title.y=element_text(color="blue",size=14,face="bold"))

meandat <- data.frame(means=means/1000)
ggplot(meandat,aes(means)) +
  geom_histogram(color="black",fill="white") +
  ggtitle(paste("Means of ",n," MLB Salaries",sep="")) +
  xlab("Salary (thousands of $)") +
  ylab("Frequency") +
  theme(plot.title=element_text(color="red",size=18,face="bold.italic",hjust=0.5),
        axis.title.x=element_text(color="blue",size=14,face="bold"),
        axis.title.y=element_text(color="blue",size=14,face="bold"))

par(mfrow=c(3,2))                   # Creates a 3x2 graphics window

n = c(5, 10, 15, 20, 25, 30)

# Wrap a loop through these 6 n-values around the code above to create the
# six histograms similar to what is requested in the HW.
for (j in 1:length(n)){

  nsim <- 2000                        # Number of simulations
  n <- 30                              # Sample size
  means <- rep(0,nsim)                # Initializes zero vector of length nsim
  set.seed(15)                        # Sets the random number seed
  for(i in 1:nsim){                   # Begins a loop to be run nsim times
    samp <- sample(sal$salary,n[j])      # Sample of size n from the salaries
    means[i] <- mean(samp)            # Mean of the n salaries sampled
  }                                   # End of for loop
  means <- means/1000
  hist(means, xlab='salary (thousands of $)', ylab="Frequency", main=paste("histrogram of n =", n[j], "Means", sep=""))
}
