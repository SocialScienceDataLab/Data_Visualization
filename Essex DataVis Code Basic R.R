##############################################################################################
# Basic R 
##############################################################################################
# R Basics
# Vectors
vec <- c(1, 2, 3, 4)
vec.2 <- c("nie", "manchmal", "oft", "immer")

class(vec)
class(vec.2)

vec[1]
vec[3:4]
vec[vec>2 & vec<5]

vec.2[1]
vec.2[3:4]
vec.2[vec.2!="immer"]


# Matrices
mat <- cbind(vec, vec.2)

class(mat)

mat[1,]
mat[,2]
mat[1,2]
mat[,"vec"]
mat[vec>2,]

class(mat[,1])
class(mat[,2])

mat.2 <- rbind(vec, vec.2)

# Data Frames
dat <- as.data.frame(cbind(vec, vec.2))

dat[,1]
dat[,"vec"]
dat$vec

class(dat$vec)
class(dat$vec.2)

dat$vec <- as.numeric(dat$vec)
class(dat$vec)

##############################################################################################
# Set Working Directory 
# ATTENTION: R only knows "/" no "\"!
setwd("/Users/richardtraunmuller/Dropbox/")

######################################################################################
# Getting Data into R
# CSV-files
data <- read.csv("Subnational RAS Switzerland.csv", sep=";")

# Excel-files
install.packages("xlsx")
library(xlsx)
data <- read.xlsx("Teaching/Lehre Frankfurt/1415 Datenvisualisierung/freedomhouse2014.xlsx", 1)

# STATA-Files
install.packages("foreign")
library(foreign)
data <- read.dta("Elites/elites_data.dta", convert.factors=T)

# SPSS-Files
data <- read.spss()

######################################################################################
# Look at Data Set

dim(data)

names(data)

head(data)

View(data)

######################################################################################
# Look at Single Variables
class(data$partei)

summary(data$partei)

table(data$partei)

barplot(table(data$partei), horiz=T, border=F, space=.8, col="blue")


#####################################################################################
# Generate and Re-Code Varibales
data$female <- ifelse(data$E1=="2. weiblich", 1, 0)  # Gender

data$age <- 2015 - data$E2 # Age

data$leftright <- data$C2 # Left-Right-Ideology

# Simplify Number of Parties
data$party.2 <- NA
data$party.2[data$party=="1. SPD"] <- "SPD"
data$party.2[data$party=="2. CDU" | data$partei=="3. CSU"] <- "Union"
data$party.2[data$party=="4. FDP"] <- "FDP"
data$party.2[data$party=="5. Grüne"] <- "Grüne"
data$party.2[data$party=="6. Linke"] <- "Linke"
data$party.2[data$party=="12. AfD"] <- "AfD"

# Attidudes (Agree 1-5)
data$redist <- data$C1_9
data$women <- data$C1_5
data$assimilation <- data$C1_1

data$redist <- as.numeric(data$redist)
data$women <- as.numeric(data$women)
data$assimilation <- as.numeric(data$assimilation)

# z-standardize Age and Ideology
data$age.z <- (data$age-mean(data$age, na.rm=T))/sd(data$age, na.rm=T)
data$lefright.z <- (data$lefright-mean(data$lefright, na.rm=T))/sd(data$lefright, na.rm=T)


# Sort Data
ord <- order(data$leftright)
data.ord <- data[ord,]

# Sample from Data
samp <- sample(1:2647, 20)
data.samp <- data[samp,]


#####################################################################################
# Again, look at single variables

class(data$leftright)
summary(data$leftright)
table(data$leftright)

mean(data$leftright, na.rm=T) 
median(data$leftright, na.rm=T) 

min(data$leftright, na.rm=T) 
max(data$leftright, na.rm=T) 
range(data$leftright, na.rm=T)
quantile(data$leftright, na.rm=T)
quantile(data$leftright, .95, na.rm=T)

sd(data$leftright, na.rm=T)  
var(data$leftright, na.rm=T)  

hist(data$linksrechts, col="blue", border=F)
hist(data$linksrechts, col="blue", border=F, breaks=10)


#####################################################################################
# Cross-Tabs

my.tab <- table(data$female, data$party.2, dnn=c("Female", "Party"))

my.tab

margin.table(my.tab, 1) # Row Sums
margin.table(my.tab, 2) # Col Sums

prop.table(my.tab) # Cell Percentage
prop.table(my.tab, 1) # Row Percentage 
prop.table(my.tab, 2) # Col Percentage

#####################################################################################
# Measures of Correlation

summary(my.tab)  # Chi^2

install.packages("vcd")
library(vcd)

assocstats(my.tab) # Phi, Cramer's V

cor(data$age, data$leftright, use="complete.obs") # Pearson's r

cor(data$assimilation, data$leftright, use="complete.obs")

cor(data$assimilation, data$leftright, use="complete.obs", method=c("kendall")) # Kendall's tau

cor(data$assimilation, data$leftright, use="complete.obs", method=c("spearman")) # Spearman's rho

# Scatterplot
plot(data$leftright, data$assimilation, pch=19, cex=.5)

#############################################################################################
# Simple Linear Regression

install.packages("arm")
library(arm)

m.1 <- lm(assimilation ~ age.z, data=data) # Linear Model

display(m.1, digits=2)

print(m.1)

summary(m.1)

m.2 <- lm(women ~ leftright.z, data=data, x=T) # Lineares Modell

display(m.2, digits=2)


# Viusal Display
plot(jitter(data$leftright.z), jitter(data$assimilation),  pch=19, cex=.6)

abline(m.2, col="red", lwd=2)

######################################################################################
