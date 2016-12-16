##############################################################################################
# Basic Graphs
###############################################################################################
# Set Working Directory 
setwd("/Users/richardtraunmuller/Dropbox/")

# Getting Data into R
data <- read.csv("Subnational RAS Switzerland.csv", sep=";")
FH14 <- read.csv("Teaching/Lehre Frankfurt/1415 Datenvisualisierung/freedomhouse2014.csv", sep=";")

# Look at Data Set
head(data)
head(FH14)

# Look at Single Variables
class(FH14$Status)
class(FH14$PR.Rating)

summary(FH14$Status)
summary(FH14$PR.Rating)

table(FH14$Status)
table(FH14$PR.Rating)

table(FH14$Status, FH14$PR.Rating)

prop.table(table(FH14$Status, FH14$PR.Rating))

#########################################################################
# Some Data Preparation
# Combine single items into one index of "State Support of Religion"
data$support <-  data$l01x + data$l02x + data$l03x + data$l04x + data$l05x +
                 data$l06x + data$l07x + data$l08x + data$l09x + data$l10x +
                 data$l11x + data$l12x + data$l13x + data$l14x + data$l15x +
                 data$l16x + data$l17x + data$l18x + data$l19x + data$l20x +
                 data$l21x + data$l22x + data$l23x + data$l24x + data$l25x +
                 data$l26x + data$l27x + data$l28x + data$l29x + data$l30x +
                 data$l31x + data$l32x + data$l33x + data$l34x + data$l35x +
                 data$l36x + data$l37x + data$l38x + data$l39x + data$l40x +
                 data$l41x + data$l42x + data$l43x + data$l44x + data$l45x + 
                 data$l46x + data$l47x + data$l48x + data$l49x + data$l50x +
                 data$l51x 


# Sort Data
ord <- order(data$support)
data.ord <- data[ord,]

# Sample from Data
samp <- sample(1:234, 20)
data.samp <- data[samp,]


#############################################################################################
# High Level Functions

# The Plot Function
plot(data.samp$support)

plot(data.samp$support, type="p") 
plot(data.samp$support, type="l")
plot(data.samp$support, type="b")
plot(data.samp$support, type="o")
plot(data.samp$support, type="h")
plot(data.samp$support, type="s")
plot(data.samp$support, type="n")

data.samp$support.f <- as.factor(data.samp$support) 
plot(data.samp$support.f)

plot(table(data.samp$support))


# Other High Level Functions
barplot(data.samp$support)
barplot(table(data.samp$support))

pie(data.samp$support)
pie(table(data.samp$support))

dotchart(data.samp$support)
dotchart(table(data.samp$support))

hist(data.samp$support)

boxplot(data.samp$support)

# Using 2 Variables
FH.samp <- sample(1:195, 20)
FH.samp <- FH14[FH.samp,]

plot(FH.samp$PR.Rating, FH.samp$CL.Rating)
plot(FH.samp$Status, FH.samp$PR.Rating)
plot(FH.samp$PR.Rating, FH.samp$Status)
plot(table(FH.samp$PR.Rating, FH.samp$Status))

FH.samp.2 <- FH14[FH14$Country=="Afghanistan" | FH14$Country=="United Kingdom" | FH14$Country=="Pakistan",]
barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating))


dotchart(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating))

boxplot(FH.samp$PR.Rating, FH.samp$CL.Rating)

##############################################################################################
# Some Arguments

# Some arguments are specific to certain high level functions
barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating), horiz=T)
barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating), beside=T)
barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating), beside=T, horiz=T)

barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating), beside=T, names.arg=c("Political Rights", "Civil Liberties"))
barplot(cbind(FH.samp.2$PR.Rating, FH.samp.2$CL.Rating), beside=T, names.arg=c("Political Rights", "Civil Liberties"), legend=FH.samp.2$Country)

# Use Help to find more arguments
?barplot()


# Standard arguments work for most high level functions
# Graphical parameters
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, col="red") 
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=17)
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=17, col="green")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=17, col="green", cex=2)

plot(data.samp$support, type="l", lty="dashed")
plot(data.samp$support, type="l", lwd=3)
plot(data.samp$support, type="l", lwd=3, lty="dashed")
plot(data.samp$support, type="l", lwd=3, lty="dashed", col="purple")

# Text, Labels, and Axes
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", ylim=c(.5, 7.5), xlim=c(.5, 7.5))

plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3)
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

# Some more argumens with the par() function
par(mgp=c(2, .5, 0))
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
par(mgp=c(2, .5, 0), las=1)
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
par(mgp=c(2, .5, 0), las=1, tck=-0.005)
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
par(mgp=c(2, .5, 0), las=1, tck=-0.005, bg="lightgrey")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
par(mgp=c(2, .5, 0), las=1, tck=-0.005, bg="lightgrey", fg="grey")
plot(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="red", main="Freedom House Scores 2014", xlab="Political Rights", ylab="Civil Liberties", 
     ylim=c(.5, 7.5), xlim=c(.5, 7.5), family="Garamond", font.main=1, font.lab=3,
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)

##############################################################################################
# Some Low Level Functions

grid(col="white", lty=1)

abline(v=4, col="white", lwd=3)
abline(h=4, col="white", lwd=3)

text(FH.samp$PR.Rating, FH.samp$CL.Rating, FH.samp$Country, col="white", pos=1, cex=.6)

points(FH.samp$PR.Rating, FH.samp$CL.Rating, pch=19, col="maroon3")

box(col="white", lwd=3)

##############################################################################################
# Arranging Multiple Plots

par(mfrow=c(3,2))

par(mfrow=c(3,2), mar=c(3,3,3,3))


##############################################################################################

