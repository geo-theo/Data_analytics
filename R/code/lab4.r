# Fall 2025
# Computer Data Analysis I
# Lab 3 
# =============================================================== #
# The purpose of this lab is to finish discussion of data frames, #
# and then introduce code for producing bar plots in base R and   #
# ggplot.  We will also discuss how to compute proportions from   #
# tables of categorical variables, all to help with Homework 2.   #
# =============================================================== #

# Change the default directory
# ============================
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab4")  # Sets working directory

# ================================================================= #
# Install and Load Packages: To download a package (library) in R,  #
#                            click on Tools - Install Packages,     #
#                            type in the package name (e.g.: MASS), #
#                            and click Install.                     # 
# ================================================================= #
install.packages("MASS")    # Installs library MASS
library(MASS)               # Loads library into R's memory

# ======================================= #
# Read in built-in dataset from a library #
# ======================================= #
data(Boston)                # Loads "Boston" data frame from MASS
?Boston                     # Requests a help menu on "Boston"
head(Boston)                # Shows the first 6 rows of "Boston"
summary(Boston)             # Summary of "Boston" dataset

# ==================================================================== #
# Histogram and Kernel Density Plots: Useful for Problems 3, 4, 5, & 8 #
# ==================================================================== #
hist(Boston$crim)           # Simplest way to create a histogram of the
                            #   crime rates from the Boston dataset
 
# Frequency Histogram of log crime rates
# --------------------------------------
logcrime = log(Boston$crim)   # Takes base-10 logarithm of Boston crime rates
hist(logcrime,breaks=12,      # FREQUENCY histogram of log crime rates, with
  xlab="Log Crime Rate",      #   12 bars, x- and y-axis labels, label and
  ylab="Frequency",cex.lab=   #   title font sizes, red bars, and a title on 
  1.2,cex.main=1.4,col="red", # the plot
  cex.axis=1.5,main = "Frequency Histogram of Log Crime Rates")

# Relative Frequency Histogram of log crime rates
# -----------------------------------------------
h <- hist(logcrime,breaks=12,plot=F) # Blank histogram of log crime rates
h$counts <- h$counts/sum(h$counts)   # Computes relative frequencies
plot(h,xlab="Log Crime Rate",ylab=   # RELATIVE FREQUENCY histogram of log crime
  "Relative Frequency",cex.lab=1.2,  #   rates, 12 bars, x- and y-axis labels,
  cex.main=1.4,col="red",axes=F,     #   red bars, and a title on the plot
  main = "Relative Frequency Histogram of Log Crime Rates")
axis(1,cex.axis=1.5)
axis(2,pos=-6,cex.axis=1.5)

# Density Histogram of log crime rates
# ------------------------------------
hist(logcrime,breaks=12,prob=T,  # DENSITY histogram of log crime rates, with
     xlab="Log Crime Rate",      #   12 bars, x- and y-axis labels, label and
     ylab="Frequency",cex.lab=   #   title font sizes, red bars, and a title on 
       1.2,cex.main=1.4,col="red", #   the plot
     cex.axis=1.5,main = "Density Histogram of Log Crime Rates")

# Add a kernel density curve to see the shape of the distribution
# ---------------------------------------------------------------
lines(density(logcrime, # Kernel density function plotted in overlay, with
  adjust=1),lwd=2)      #   adjust option to control smoothness    
lines(density(logcrime, # Kernel density function plotted in overlay, with
  adjust=2),lwd=2,lty=  #   a smoother density (adj=2), line width 2, line
  8,col="darkred")      #   type 8 (dashed line), colored "dark red"

# ================================ #
# Lattice of Histograms: Problem 3 #
# ================================ #
disease <- read.csv("diseases.txt")     # Reads in disease data
head(disease)                           # First 6 rows of disease data
summary(disease)                        # Summary of disease data
attach(disease)                         # Promotes the "disease" dataset
library(lattice)                        # Loads the lattice library

histogram(~AIDS+Syphilis+Tuberculosis,  # Makes a 3x1 panel of histograms
  layout=c(1,3),data=disease,ylab=      #   with x- and y-axis labels,
  "Percent",col=8,breaks=11,            #   gray bars (col=2), with about
  xlab="Number of Reported Cases")      #   11 bars
median(AIDS)                            # Median number of AIDS cases
median(Syphilis)                        # Median number of Syphilis cases
median(Tuberculosis)                    # Median number of Tuberculosis cases
 
# ======================= #
# Histogram using ggplot2 #
# ======================= #
library(ggplot2)                              # Loads ggplot2 library
ggplot(data=Boston,aes(x=log(Boston$crim))) + # GGPLOT call with aesthetics
  geom_histogram(binwidth=0.5)                # Histogram with bin widths of 0.5
 
# Draw with black outline, white fill
# -----------------------------------
ggplot(data=Boston,aes(x=log(Boston$crim))) + # GGPLOT call with aesthetics
  geom_histogram(binwidth=0.5,color="black",  # Histogram with black outlined
    fill="white")                             #   bars filled in white

 
# Density curve
# -------------
ggplot(data=Boston,aes(x=log(Boston$crim))) + # GGPLOT call with aesthetics
  geom_density()                              # Kernel density plot
 
# Histogram overlaid with kernel density curve
# --------------------------------------------
ggplot(data=Boston,aes(x=log(Boston$crim))) + # GGPLOT call with aesthetics 
  geom_histogram(aes(y=..density..),          # Histogram with density instead of
    binwidth=.5,color="black",fill="white") + #  count on y-axis
    geom_density(alpha=.2,fill="#FF6666")     # Overlay with transparent kernel
                                              #   density plot
 
# Add a vertical line for the mean
# --------------------------------
ggplot(data=Boston,aes(x=log(crim))) +      # GGPLOT call with aesthetics
  geom_histogram(binwidth=.5,color="black", # Histogram with white bars of
    fill="white") +                         #   width 0.5, outlined in black
  geom_vline(aes(xintercept=mean(log(       # Vertical dashed line, in red,
    crim),na.rm=T)),color="red",            #   at the mean crime rate, with
    linetype="dashed",linewidth=1)          #   NA's removed.
 
# =========================================================== #
# Histogram and density plots with multiple groups: Problem 5 #
# =========================================================== #

# Overlaid histograms
# -------------------
summary(Boston$age)
Boston$age <- ifelse(Boston$age<mean(Boston$age,na.rm = T),"Young","Old")
Boston$age <- factor(Boston$age,levels=c("Young","Old"))

ggplot(data=Boston,aes(x=log(crim),fill=age)) +
    geom_histogram(binwidth=.5,alpha=.5,position="identity")

# Interleaved histograms
# ----------------------
ggplot(data=Boston,aes(x=log(crim),fill=age)) +
  geom_histogram(binwidth=.5,position="dodge")
 
# Overlaid Kernel Density plots
# -----------------------------
ggplot(data=Boston,aes(x=log(crim),fill=age)) +
  geom_density(alpha=1)
 
# Overlaid Kernel Density plots with semi-transparent fill
# --------------------------------------------------------
ggplot(data=Boston,aes(x=log(crim),fill=age)) +
  geom_density(alpha=0.4)

# ==================================================== #
# Overlaid Kernel Density Plots with base R: Problem 5 #
# ==================================================== #
hotdog = read.csv("hotdog.txt")        # Reads in hot dog dataset
head(hotdog)                           # First 6 rows of dataset
summary(hotdog)                        # Summary of hot dog dataset
attach(hotdog)                         # Promotes the hot dog dataset
cal_beef <- calories[type=="Beef"]     # Calories for beef hot dogs
cal_meat <- calories[type=="Meat"]     # Calories for meat hot dogs
cal_poul <- calories[type=="Poultry"]  # Calories for poultry hot dogs
   
plot(density(cal_beef,adjust = 2),lwd= # Kernel density plot of beef calories,
  2,lty=1,ylim=c(0,0.013),cex.lab=1.6, #   with smoothness adjustment, line
  cex.axis=1.5,cex.main=1.6,           #   width 2, line tyupe 1 (solid line)
  main="Kernel Density Plots of Calories\nLevels by Hot Dog Type",
  xlab="Calories level",mgp=c(2.7,1,0))
lines(density(cal_meat,adjust=2),      # Overlays meat kernel density with
  lty=8,lwd=2)                         #   line type 8
lines(density(cal_poul,adjust=2),      # Overlays poultry kernel density with
  lty=4,lwd=2)                         #   line type 4
legend("topright",c("Beef","Meat",     # Puts a legend on the plot in the top
  "Poultry"),lty=c(1,8,4),             #   right corner, with the 3 line types
  lwd=c(2,2,2),cex = 1.5)              #  and line widths specified

# ============================ #
# Time Series Plots: Problem 6 #
# ============================ #
defense = read.csv("defense2025.txt")  # Reads in defense spending dataset
head(defense)                          # First 6 rows of defense dataset
summary(defense)                       # Summary of defense dataset
attach(defense)                        # Promotes defense dataset
plot(year,expenditure,type="b",pch=16, # Time series plot of expenditure by year,
  xlab="Year",ylab="Expenditure",      # type="b" asks for points and lines,
  cex.lab=1.4,cex.main=1.6,cex.axis=   #   with axis labels and a title
  1.2,main="Expenditures vs. Time")

# =========================================== #
# Computing Means and Medians: Problems 7 & 8 #
# =========================================== #
drug = read.csv("marijuana2017.txt") # Reads in marijuana plants/arrests data
head(drug)                           # First 6 rows of marijuana dataset
summary(drug)                        # Summary of marijuana dataset
attach(drug)                         # Promotes the marijuana dataset

# Computes the mean
# -----------------
mean(arrests)                          # Computes the mean of "arrests"
mean(arrests,na.rm=T)                  # Remove NA values from the calculation
mean(arrests[arrests!=185000],na.rm=T) # Remove outlier using logical statement

# Computes the median
# -------------------
median(arrests)                          # Computes the median number of arrests
median(arrests,na.rm=T)                  # Removes NA values from the calculation
median(arrests[arrests!=185000],na.rm=T) # Remove outlier using logical statement

# Find the mode using frequency table
# -----------------------------------
table(arrests)                        # Table of arrests values

# Computes the trimmed mean
# -------------------------
mean(arrests,trim=0.1,na.rm=T)        # 10% trimmed mean
mean(arrests,trim=0.2,na.rm=T)        # 20% trimmed mean
