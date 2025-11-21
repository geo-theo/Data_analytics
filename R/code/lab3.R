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
setwd("C://Jon-Classes/STAT 457/Fall 2025/Lab3")  # Sets working directory

# ========================================================================= #
# EXAMPLE FOR PROBLEM 9, HOMEWORK 2: Marginal and Conditional Distributions #
# ========================================================================= #

# Working with Categorical Data by Case
# =====================================
births <- read.csv("births.txt",as.is=F) # Reads a dataset with variable names
View(births)                             # View a dataset in a new tab
summary(births)                          # Summary to check dataset
levels(births$prenatal)                  # Gives levels of variables
table(births)                            # Create a 2-way table for dataset

# Change the order of levels of a categorical variable
# ====================================================
births$prenatal = factor(births$prenatal,
  levels = c("Inadequate","Adequate","Intensive"))
levels(births$prenatal)                          

# Computing marginal and conditional distributions
# ================================================
tab1 <- table(births)                # Assigns "tab1" to table
addmargins(tab1)                     # To add marginal sums
margin.table(tab1, 1)                # Marginal sum of row(1) var. (prenatal)
                                     #   Column variable is (2)
xtabs(~prenatal,data=births)         # Another way to compute marginal sums
prop.table(tab1, 1)                  # Conditional distribution of row variable
                                     #   (prenatal), for a given type of birth
prop.table(margin.table(tab1, 1))    # Marginal distribution of row var. (prenatal)

# ===================================================================== #
# Example for Problem 7, HOMEWORK 2: Basic Bar charts and pie charts    #
#   Consider the following data on the number of non-roadway fatalities #
#   occurring in the US in 2022 (US Dept. of Transportation).           #
# ===================================================================== #
deaths <- read.csv("fatalities.txt",as.is=F)
summary(deaths)
deaths$perc <- round(100*deaths$count/           # Calculates percentage of deaths
                      sum(deaths$count),2)       #   for the 5 groups
deaths$source <- as.factor(deaths$source)        # Converts "program" to a factor
deaths$source <- factor(deaths$source,levels=    # order the program levels by percentage
  c("Pipeline","Transit rail","Air","Water",     #   expenditure from largest to smallest
  "Railroad"))

# Bar plot and pie chart in base R
# --------------------------------
par(mar = c(5, 14, 4, 2) + 0.1)                  # Adds space (14) for labels on the LHS
barplot(perc~source,data=deaths,ylab="",xlab=    # Bar graph of death percentages,
          "Percentage Deaths",horiz=T,las=1,     #   turned horizontally, with x-axis
        xlim=c(0,40),cex.lab=1.6,cex.names=1.5,  #   from 0 to 20, and axis labels as
        main="Non-Roadway Fatalities (2022)",    #   given
        cex.axis=1.5,cex.main=1.6)
pie(deaths$perc,labels=deaths$source,col=        # Pie chart of percentage deaths
  c("red","blue","green","yellow","violet"),
  cex=1.5)

# =================================================================== #
# Package ggplot2: To install a package, go to Tools menu, then click #
# on Install Packages, Or just type:                                  #                     # 
# =================================================================== #
install.packages("ggplot2")                   # Installs ggplot package

library(ggplot2)                              # Loads ggplot package into R

# ===================================================================== #
# All data used in ggplot are in data frames.  The first line for any   #
# plot in ggplot sets the "aesthetics" of the plot by mapping variables #
# to visual properties, as seen below.  In addition to setting the      #
# variable names for "x" and "y", the option "aes" also can set the     #
# color, fill, size, shape, transparency (alpha), line type, etc.       #
# lines thart appear after the initial "aes" statement determine the    #
# type of plot made and other plot attributes, as explained below.      #
# ===================================================================== #

bar <- ggplot(deaths,aes(x=source,y=count)) +    # Frequency barplot of
  geom_bar(stat="identity",                      #   activity by count
           width=0.8,                            # Width of barplot bars
           color="blue",                         # Color of bar boundaries
           fill="blue") +                        # Fill color for bars
  ggtitle("Barplot of Non-Roadway Fatalities") + # Barplot title
  xlab("Source") +                               # X-axis label
  ylab("Frequency")                              # Y-axis label
bar                                              # Prints the bar graph

# ================================================================= #
# Note that we can continue adding options to the plot with the "+" #
# symbol and the plot will continually update.  In base R, the      #
# attributes of the plot were all contained in a one function call. #
# ================================================================= #

pbar <- ggplot(deaths,aes(x=source,y=perc)) + # Percentage barplot of
  geom_bar(stat="identity",width=0.8,color=   # source by count
           "blue",fill="blue") +
  ggtitle("Barplot of Fatality Sources") +    # Barplot title
  xlab("Source") +                            # X-axis label
  ylab("Percentage")                          # Y-axis label
pbar

pbar + theme_minimal()                        # Minimal theme
pbar + coord_flip()                           # Flips the axes
pbar + geom_text(aes(label = perc),           # Add label outside bars
                 vjust = -0.3, size = 5.0)
pbar + geom_text(aes(label = perc),vjust=1.6, # Add white labels
                 color="white",size=5.0)      #   inside bars
pbar + theme(
  axis.text = element_text(size = 12),
  plot.title = element_text(color="red", size=18, face="bold.italic", hjust = 0.5),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold"))

# ==================================================================== #
# Example for Problem 8, HOMEWORK 2: segmented or clustered bar charts #
# ==================================================================== #
obesity <- read.csv("obesity.txt",as.is=F)    # Reads in obesity data
View(obesity)

xtabs(count~activity+BMI,data=obesity)        # Create a two-way table of counts
                                              #   when activity is a row variable
                                              #   and BMI is a column variable 
obesity$activity <- factor(obesity$activity,  # Change the order of activity levels
  levels(obesity$activity)[c(1, 2, 4, 3)])    #   to a meaningful order
obesity$BMI <- factor(obesity$BMI,            # Change the order of BMI levels
  levels=c("Normal","Overweight","Obese"))    #   to a meaningful order
tab2 <- xtabs(count~activity+BMI,data=obesity)# Recreate the 2-way table
addmargins(tab2)                              # Adds marginal totals
xtabs(count~activity,data=obesity)            # Marginal counts for activity
xtabs(count~BMI,data=obesity)                 # Marginal counts for BMI
tab3 = prop.table(tab2, 2)                    # Conditional distribution of BMI
                                              #   (Column proportions; 1 gives
                                              #   activity proportions)
# Segmented Bar Graph in base R
# -----------------------------
par(mfrow=c(1,1))                             # Sets a 1x1 graphics window
barplot(100*tab3,beside=F,xlim=c(0,7),        # Creates a segmented bar graph,
  legend.text = c("Inactivity",               #   where tab3 must have column
  "Irregular Active","Regular Not Intense",   #   proportions
  "Regular Intense"),args.legend=list(        # Puts a title on the legend
  title="Activity",cex=1.3),xlab="BMI",       # X-axis label & Y-axis label
  ylab="Percent",cex.names=1.2,cex.axis=1.2,  
  mgp=c(2.7,1,0),cex.lab=1.5)

# Clustered Bar Graph in base R
# -----------------------------
barplot(100*tab3,beside=T,args.legend=list(   # Creates a clustered bar graph,
  x=8.5,y=40,title = "Activity", cex = 0.8),  #   where tab3 must have column
  legend.text = c("Inactivity",               #   proportions. (x,y) gives the
  "Irregular Active","Regular Not Intense",   #   position of the top left corner
  "Regular Intense"),xlab="BMI",ylab=         #   of the legend, and "col" sets
  "Percent",cex.names=1.2,cex.axis=1.4,       #   the colors for the four activities
  cex.lab=1.4,col=c("gray","black","yellow4",
  "blue"))

# Segmented Bar Graph in ggplot
# -----------------------------
ggplot(obesity,aes(fill=activity,y=count,x=BMI)) + # Segmented bar graph
  geom_bar(position="fill",stat="identity") +      # "fill" = Segmented
  xlab("BMI") +                                    # X-axis label
  ylab("Proportion") +                             # Y-axis label
  guides(fill=guide_legend(title="Activity")) +    # Legend title
  theme(legend.position="right",                   # Legend position
        legend.text = element_text(size=18),
        legend.title = element_text(color="black",size=18,face="bold.italic"),
        axis.text=element_text(size=12,face="bold"),
        plot.title = element_text(color="red", size=18, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))

# Clustered Bar Graph in ggplot
# -----------------------------
obesity$perc <- 100*c(t(tab3))                     # Computes BMI conditional dist.
ggplot(obesity,aes(fill=activity,y=perc,x=BMI)) + 
  geom_bar(position="dodge",stat="identity") +
  xlab("BMI") +                                    # X-axis label
  ylab("Percent") +                                # Y-axis label
  guides(fill=guide_legend(title="Activity")) +    # Legend title
  scale_fill_manual(values = c("red","blue","green","yellow")) +
  theme(legend.position="right",                   # Legend position
        legend.text = element_text(size=18),
        legend.title = element_text(color="black",size=18,face="bold.italic"),
        axis.text=element_text(size=12,face="bold"),
        plot.title = element_text(color="red", size=18, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))
