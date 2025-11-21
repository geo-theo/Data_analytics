# Fall 2025
# Computer Data Analysis I
# Lab 5
# ======================================================================= #
# The purpose of this lab is to learn how to compute the various measures #
# of center and spread, 5-number summary, percentiles, and CV.  It also   #
# shows how to create boxplots and scatterplots in both base R and        #
# ggplot, how to conduct the 1.5*IQR Outlier test, and introduces a       #
# useful package "doBy" that facilitates calculating summary statistics   #
# by a grouping variable.                                                 #
# ======================================================================= #

# Change the default directory
# ============================
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab5")  # Sets working directory
rm(list=ls())                                     # Removes all objects

# ============================================================= #
# Computing Various Measures of Center and Spread: All Problems #
# ============================================================= #
drug = read.csv("marijuana2017.txt") # Reads in marijuana plants/arrests data
head(drug)                           # First 6 rows of marijuana dataset
summary(drug)                        # Summary of marijuana dataset
attach(drug)                         # Promotes the marijuana dataset

# Measures of center
# ------------------
mean(arrests,na.rm=T)                 # Sample mean number of arrests
mean(arrests[arrests<185000],na.rm=T) # Removes outlier using logical statement
median(arrests,na.rm=T)               # Sample median number of arrests
table(arrests)                        # Table for sample mode no. of arrests
mean(arrests,trim=0.1,na.rm=T)        # 10% trimmed mean number of arrests

# Measures of Spread
# ------------------
quantile(arrests,type=5,na.rm=T)      # 5-Number summary
quantile(arrests,0.90,type=5,na.rm=T) # Estimate of 90th percentile
quantile(arrests,prob=c(.20,.30,.40), # Estimates of 20th, 30th, and 40th
  type=5,na.rm=T)                     #   percentile of number of arrests
IQR(arrests,na.rm=T)                  # IQR of number of arrests
sd(arrests,na.rm=T)                   # Standard deviation of no. of arrests
var(arrests,na.rm=T)                  # Variance of number of arrests

# ================================================ #
# Creating side by side boxplots: For problems 5,6 #
# ================================================ #
nba = read.csv("NBAwins2025.txt")     # Reads in the NBA dataset
head(nba)                             # First 6 rows of dataset
summary(nba)                          # Summary of NBA wins data
GSW = nba$GSW/nba$Games               # Winning percentages for Golden State
BOS = nba$BOS/nba$Games               # Winning percentages for Boston
LAL = nba$LAL/nba$Games               # Winning percentages for Los Angeles
CHI = nba$CHI/nba$Games               # Winning percentages for Chicago
SAS = nba$SAS/nba$Games               # Winning percentages for San Antonio
boxplot(GSW,BOS,names=c("GSW","BOS"), # Side-by-side boxplots of winning
  cex.lab=1.5,cex.axis=1.5,ylab=      # percentages for Golden State & Boston
  "Winning Percentage",xlab="Teams")
boxplot(GSW,BOS,LAL,CHI,SAS,ylab=     # Side-by-side boxplots of the 5
  "Winning Percentage",cex.lab=1.5,   # winning percentages
  xlab="Teams",border="brown",col=
  "orange",notch=T,names=c("GSW","BOS","LAL","CHI","SAS"), cex.axis=1.5)

# ============================================ #
# Creating side by side boxplots using ggplot2 #
# ============================================ #
library(ggplot2)
n <- length(GSW)
team <- rep(c("GSW","BOS","LAL","CHI","SAS"),each=n)
prop <- c(GSW,BOS,LAL,CHI,SAS)
NBAprop <- data.frame(Team=team,Prop=prop)
ggplot(NBAprop,aes(x=Team,y=Prop)) +   # Basic boxplot
  geom_boxplot()

ggplot(NBAprop,aes(x=Team,y=Prop)) +   # Rotates the boxplot
  geom_boxplot() +
  coord_flip()

ggplot(NBAprop,aes(x=Team,y=Prop)) +   # Notched boxplot
  geom_boxplot(notch=T)

ggplot(NBAprop,aes(x=Team,y=Prop)) +   # Boxplot controlling color,
  geom_boxplot(outlier.colour="red",   #   size, shape of outliers
    outlier.shape=8,outlier.size=4)

ggplot(NBAprop,aes(x=Team,y=Prop)) +   # Boxplots with all data values
  geom_boxplot() +                     #   plotted in overlay
  geom_dotplot(binaxis='y',stackdir=
    'center',dotsize=1)

ggplot(NBAprop,aes(x=Team,y=Prop,color=Team)) + # Boxplots with different
  geom_boxplot()                                #   border colors by groups

ggplot(NBAprop,aes(x=Team,y=Prop,fill=Team)) + # Boxplots with different
  geom_boxplot() +                             #   colors for each box
  theme_classic()

ggplot(NBAprop,aes(x=Team,y=Prop,fill=Team)) + # Boxplot with legend at the
  geom_boxplot() +                             #   top instead of the side
  xlab("Team") +
  ylab("Proportion of Wins") +
  ggtitle("Boxplots of NBA Winning Proportions") +
  theme(legend.position="top",
        legend.text = element_text(size=18),
        legend.title = element_text(color="black",size=18,face="bold.italic"),
        axis.text=element_text(size=12,face="bold"),
        plot.title = element_text(color="red", size=18, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

# =================================== #
# Removing Data Values: For problem 3 #
# =================================== #

roll = read.csv("papertowels.txt") # Reads in paper towel data
head(roll)                         # First 6 rows of paper towel data
summary(roll)                      # Summary of paper towel data
attach(roll)                       # Attaches paper towel data
mean(PriceperRoll)                 # Mean price per roll
sd(PriceperRoll)                   # SD of price per roll
PriceperRoll[4] <- 14.9            # Changes 4th value to 14.9
mean(PriceperRoll)                 # Recomputes the mean
sd(PriceperRoll)                   # Recomputes the SD
PriceperRoll[4] <- 149             # Changes 4th value to 149
mean(PriceperRoll)                 # Recomputes the mean
sd(PriceperRoll)                   # Recomputes the SD

# ======================================= #
# Coefficient of Variation: For problem 4 #
# ======================================= #

PriceperRoll[4] <- 1.49       # Resets 4th value to original 1.49
mean(PriceperRoll)            # Computes mean of price per roll
sd(PriceperRoll)              # Computes standard deviation of price per roll
coef_var = sd(PriceperRoll)/  # Computes coefficient of variation (CV) of
  mean(PriceperRoll)          #   price per roll

# =============================================================== #
# Scatterplots of Relationship between Two Quantitative Variables #
#   - Useful for Problems 1 & 4                                   #
# =============================================================== #

plot(SheetsperRoll,PriceperRoll,xlab=   # Scatterplot of Price per Roll (y)
     "Sheets per Roll",cex.lab=1.6,     #   vs. Sheets per Roll (x) with filled
     ylab="Price per Roll ($)",pch=16,  #   circles (pch=16) and axis labels
     cex.axis=1.5,cex.main=2,main=
     "Scatterplot of Price vs. Sheets per Roll",mgp=c(2.7,1,0))

# Using ggplot
# ------------
ggplot(roll,aes(x=SheetsperRoll,y=PriceperRoll)) +  # Aesthetics for ggplot
  geom_point(shape = 16) +   # Scatterplot of y vs. x
  geom_smooth(method = lm,   # Adds linear regression line
              se = FALSE)    # Removes shaded confidence bands

# ======================================== #
# Applying the 1.5*IQR Rule: For Problem 5 #
# ======================================== #
mouse <- read.csv("mousediets.txt",   # Reads in the mousediets data
  as.is=F)
head(mouse)                           # First 6 rows of mouse data
summary(mouse)                        # Summary of mouse data
mouse$diet <- factor(mouse$diet,      # Reorders the factor levels to
  levels(mouse$diet)[c(5,2,4,6,1,3)]) #   the original group order
sorted_NP <- sort(mouse$lifetime[     # Sorts the mouse lifetimes for
  mouse$diet=="NP"])                  #   the "NP" group
median(sorted_NP)                     # Median lifetime for the NP group
iqr <- IQR(sorted_NP)                 # IQR of lifetimes for NP group
quantile(mouse$lifetime[              # 5-number summary of lifetimes
  mouse$diet=="NP"],type=5)           #   for the NP group
Q1 <- quantile(mouse$lifetime[        # Computes the first quartile of
  mouse$diet=="NP"],0.25,type=5)      #   lifetimes for NP group
Q3 <- quantile(mouse$lifetime[        # Computes the 3rd quartile of
  mouse$diet=="NP"],0.75,type=5)      #   lifetimes for NP group
Q3-Q1                                 # Computes IQR from the quartiles
Lower <- Q1 - 1.5*iqr                 # Lower fence for 1.5*IQR test
Upper <- Q3 + 1.5*iqr                 # Upper fence for 1.5*IQR test
which(sorted_NP<Lower)                # Identifies cases below lower fence
sorted_NP[which(sorted_NP<Lower)]     # Identifies lifetimes below lower fence
which(sorted_NP>Upper)                # Identifies cases above upper fence

# ============================= #
# Example of summaryBy function #
# ============================= #

install.packages("doBy")                       # installs the "doBy" library
library(doBy)                                  # Loads the "doBy" library

fun = function(x)                              # Function to compute stats by group
  c(min   = min(x,na.rm=T),                   # Computes the minimum value
    Q1    = quantile(x,na.rm=T,0.25,type=5),  # Computes the first quartile
    M     = median(x,na.rm=T),                # Computes the meadian
    Q3    = quantile(x,na.rm=T,0.75,type=5),  # Computes the 3rd quartile
    max   = max(x,na.rm=T),                   # Computes the maximum
    m     = mean(x,na.rm=T),                  # Computes the mean
    sd    = sd(x,na.rm=T),                    # Computes the standard deviation
    n     = length(x[!is.na(x)]),             # Computes the sample size
    Lower = quantile(x,na.rm=T,0.25,type=5)-  # Computes the lower fence of
               1.5 * IQR(x,na.rm=T),           #   the 1.5*IQR Rule
    Upper = quantile(x,na.rm=T,0.75,type=5)+  # Computes the upper fence of
               1.5 * IQR(x,na.rm=T))           #   the 1.5*IQR Rule

summaryBy(lifetime~diet,data=mouse,FUN=fun)    # Computes lifetime summaries by diet

