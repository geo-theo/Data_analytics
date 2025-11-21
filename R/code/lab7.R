# Fall 2025
# Computer Data Analysis I
# Lab 7
# ============================================================= #
# The purpose of this lab is to practice performing small-scale #
# simulations, introduce the binomial distribution and related  #
# functions, and spend some time working on the HW simulation.  #
# ============================================================= #

# Change the default directory
# ============================
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab7")  # Sets working directory
rm(list=ls())                                     # Removes all objects

# ====================================================================== #
# Example (Find the ace): A technology store holds a contest to attract  #
# shoppers.  Once an hour, someone at the checkout, is chosen at random  #
# to play a game.  Here's how it works.  An ace and four other cards are #
# shuffled and placed face down on the table.  The customer gets to turn #
# over cards one at a time, looking for the ace. The persons wins $100   #
# of store credit if the ace is the first card, $50 if it is the second  #
# card, and $20, $10, or $5 if it is the third, fourth, or last card     #
# chosen.  What is the average dollar amount of store credit given away  #
# in the contest for a day if the store is operational for 12 hours?     #
# Perform a simulation to estimate this average amount.                  #
# ====================================================================== #
set.seed(13)       #  Sets the seed of R's random number generator, which
                   #    is useful for creating simulations or random
                   #    objects that can then be reproduced.
x <- sample(x=c(100,50,20,10,5),  # Takes a sample of size 1000 from the
  prob=c(.2,.2,.2,.2,.2),size=    #   values (100,50,20,10,5) with equal
  1000,replace=T)                 #   probabilities, with replacement
mean(x)                           # Average winnings of one play
mean(x)*12                        # Daily average winnings (12 times)

# How do we find the TRUE average winnings on one play of the game?

#========================================================================== #
# If we flip a fair coin 10 times, what is the distribution of the          #
# number of heads we obtain.  Run 10000 simulations to see its distribution #
# ========================================================================= #
set.seed(9)
heads <- rbinom(n=10000,size=10,  # Generates 1000 random numbers from
                prob=0.5)         #   a binomial(n=10,p=0.5) distribution
tab1  = prop.table(table(heads))  # Relative frequency table of number of heads
hist(heads,xlab="# Heads",breaks= # Histogram of number of heads obtained in
  seq(-0.5,10.5,1),cex.axis=1.5,  #   10 coin flips, simulated 10,000 times
  cex.lab=1.6,main="")

# ========================================================================== #
# BINOMIAL DISTRIBUTION: In class on Wednesday, we will be introduced to the #
#     binomial distribution.  A binomial experiment occurs when we run a     #
#     fixed number of independent trials n, each of which results in a       #
#     success with probability p and failure with probability 1-p, where     #
#     we count the number of successes in those n trials.  For example,      #
#     if we flip a coin 10 times and count the number of heads, this is a    #
#     binomial experiment where X = the number of heads is a random          #
#     variable with has a binomial distribution with two parameters n=10     #
#     and p = 1/2 (written X ~ Binom(n,p)).                                  #
#                                                                            #
#     To calculate probabilities from a binomial distribution, we use a      #
#     series of "binom" functions as illustrated below.                      #
#         1. dbinom: P(Y = y)                                                #
#         2. pbinom: P(Y <= y)                                               #
#         3. rbinom: Random sample of binomial counts                        #
#         4. qbinom: Gives the value y for which P(Y <= y) = p               #
# ========================================================================== #
dbinom(7,10,0.5)          # Computes P(X = 7) for X ~ Binomial(n=10, p=0.5)
dbinom(7:10,10,0.5)       # Computes P(X = 7), P(X = 8), P(X = 9), and
                          #   P(X = 10) for X ~ Binomial(n=10, p=0.5)
sum(dbinom(7:10, 10, .5)) # Computes P(7 <= X <= 10) or P(6 < X < 11) for
                          #   X ~ Binomial(n=10, p=0.5)
pbinom(7, 10, .5)         # Computes P(X <= 7) or P(X < 8) for X ~ Binom(10,0.5)
1 - pbinom(7, 10, .5)     # Computes P(8 <= X <= 10) or P(X>8) for
                          #   X ~ Binom(10,0.5)
qbinom(0.80,10,0.5)       # Finds the 80th percentile from a Binom(10,0.5)
y <- rbinom(1000,10,0.5)  # Takes a random sample of 1000 binomimal counts
                          #   from  Binom(10,0.5) distribution

# ============================================= #
# To plot the theoretical binomial distribution #
# ============================================= #
n = 10                             # Number of binomial trials
k = seq(0,n,1)                     # Sequence 0, 1, 2, ..., 10
plot(k,dbinom(k,n,0.5),type="h",   # "h" creates separate lines for each value
     ylab="Probability",cex.axis=
     1.5,cex.lab=1.6,lwd=2,main="Binomial Distribution\n(n=10,p=0.5)")

# ====================================================================== #
# For Loop Simulation: Recall from lab 6 the simulation for a 5-response #
# multiple choice test with 100 questions where each question had 5      #
# possible answers.  If we guessed at every question, we would expect to #
# get 1/5 or 20 out of 100 questions correct.  How likely was it         #
# that we got 30 or more questions correct just by guessing?             #
# ====================================================================== #
set.seed(17)                # Sets random number seed
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

# ======================================================================== #
# Homework 5, Problem 11: A proud legislator claims that your state's new  #
#       law against talking on a cell phone while driving has reduced cell #
#       phone use to just 10% of all drivers.  While waiting for your bus  #
#       the next morning, you notice that 8 of the 30 people who drive by  #
#       are using their cell phones.  Does this cast doubt on the legis-   #
#       lator's figure of 10%?  To address this question, simulate the act #
#       of observing 30 drivers and counting how many are on their cell    #
#       phones.  To perform one simulation, R-code was provided as given   #
#       below.  Use this code to perform 10,000 simulations of this        #
#       experiment (using a for loop) and make a histogram of the number   #
#       of people on their cell phones.                                    #
# ======================================================================== #

sim_counts <- numeric(10000)

for (i in 1:10000) {sim_counts[i] <- sum(rbinom(30, size = 1, prob = 0.1))}

hist(sim_counts,
     breaks = seq(-0.5, max(sim_counts) + 0.5, 1),
     main = "Phone Counts after 10,000 runs",
     xlab = "Number of Drivers on Phone (from 30 observations)",
     ylab = "Frequency")

# Compute approximate p-value (how unusual is 8 or more?)
p_value <- mean(sim_counts >= 8)
cat("Proportion of simulations with 8 or more phone users:", p_value, "\n")
