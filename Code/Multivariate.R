# Visualizing Multivariate Data

setwd("/Users/richardtraunmuller/Dropbox/")
data <- read.dta("Elites/elites_data.dta", convert.factors=T)


# Generate and Re-Code Varibales
data$female <- ifelse(data$E1=="2. weiblich", 1, 0)  # Gender

data$age <- 2015 - data$E2 # Age

data$leftright <- data$C2 # Left-Right-Ideology

# Simplify Number of Parties
data$party <- NA
data$party[data$partei=="1. SPD"] <- "SPD"
data$party[data$partei=="2. CDU" | data$partei=="3. CSU"] <- "Union"
data$party[data$partei=="4. FDP"] <- "FDP"
data$party[data$partei=="5. Grüne"] <- "Grüne"
data$party[data$partei=="6. Linke"] <- "Linke"
data$party[data$partei=="12. AfD"] <- "AfD"

# Create Attidudes (Agree 1-5)

for(i in 1:10){
  data[,paste("C1_", i, sep="")] <- as.numeric(data[, paste("C1_", i, sep="")])
}

data$assimilation <- data$C1_1
data$polecon <- data$C1_2
data$environ <- data$C1_3
data$samesex <- data$C1_4
data$women <- data$C1_5
data$criminal <- data$C1_6
data$migecon <- data$C1_7
data$socsec <- data$C1_8
data$redist <- data$C1_9
data$demreform <- data$C1_10


### Keep only the Variables Needed
data <- data[, c("female", "age", "leftright", "party", "assimilation", "polecon", "environ", "samesex", "women", "criminal", "migecon", "socsec", "redist", "demreform")]

#######################################################################
par(mfrow=c(1,1), mgp=c(1.5,.3,0), tck=.0, fg="grey", mar=c(3,3,2,1), cex.main=.8, col.axis="grey", col.lab="grey")

# Turn party to numeric (needed to be able to plot it as scatter plot)
data$party.2 <- as.factor(data$party)
data$party.2 <- as.numeric(data$party.2)


# "Contingency Table"
plot(jitter(data$redist), jitter(data$party.2), pch="", cex=.6, xlab="Support for Redistribution", ylab="", axes=F)
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), labels=names(table(data$party)), col="white", col.axis="black")
for(i in 1:6){
  text(c(1:5), rep(i, 5),  my.tab[i,])
}
box(bty="l")


# Simple Scatter Plot of Categorical Variables
plot(jitter(data$redist), jitter(data$party.2), pch=19, cex=.6, xlab="Support for Redistribution", ylab="", axes=F)
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), labels=names(table(data$party)), col="white", col.axis="black")


# Simple Heat Map of Contingency table
my.tab <- table(data$party.2, data$redist)
my.scale <- function(x, d){(x-1)/(d-1)}  

image(t(my.tab), axes=F, col=rev(heat.colors(10)))
axis(2, at=my.scale(1:6, 6), label=names(table(data$party)), col="white", col.axis="black", xlab="Support for Redistribution")
axis(1, at=my.scale(1:5, 5), label=c(1:5), las=1, col="white", col.axis="black")


######################################################################
# Area Charts

my.tab <- table(data$party.2, data$redist)
rownames(my.tab) <- names(table(data$party))

# Re-order Levels of Categorical Variable (Logically)
data$party.ord <- factor(data$party, levels=c("Linke", "Grüne", "SPD", "Union", "FDP", "AfD"))
my.tab <- table(data$party.ord, data$redist)

rownames(my.tab) <- names(table(data$party.ord))

# Bubbles
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  circle=sqrt(my.tab[i,]/200/pi), add=T, inches=F, fg="black")
}
axis(1, , col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")

# Filled Bubbles
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  circle=sqrt(my.tab[i,]/200/pi), add=T, inches=F, bg="grey", fg="grey")
}
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")

# Squares
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  square=sqrt(my.tab[i,]/200), add=T, inches=F, bg="grey", fg="grey")
}
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")

### Once more with Color 
# Bubbles
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  circle=sqrt(my.tab[i,]/200/pi), add=T, inches=F, fg=brewer.pal(5, "PRGn"))
}
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")

# Filled Bubbles
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  circle=sqrt(my.tab[i,]/200/pi), add=T, inches=F, bg=brewer.pal(5, "PRGn"), fg="grey")
}
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")

# Squares
par(mgp=c(1.5, .3, 0))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), axes=F, xlab="Support for Redistribution", ylab="")
for(i in 1:dim(my.tab)[1]){
  symbols(c(1:dim(my.tab)[2]), rep(i, dim(my.tab)[2]),  square=sqrt(my.tab[i,]/200), add=T, inches=F, bg=brewer.pal(5, "PRGn"), fg="grey")
}
axis(1, col="white", col.axis="black")
axis(2, at=c(1:6), label=rownames(my.tab), las=1, col.axis="black", col="white")


# Add Numbers to Plot
for(i in 1:6){
  text(c(1:5), rep(i, 5),  my.tab[i,])
}

######################################################################
# Mosaic Plot
par(fg="black", col.lab="black", mfrow=c(2,2))
plot(0, 0, pch="", xlim=c(0.5, 5.5), ylim=c(0.5, 6.5), xlab="", ylab="", axes=F)
rect(0, 0, 6, 7, col="grey", border=F)

mosaicplot(table(data$party), xlab="Political Party", border=F, main="")
mosaicplot(table(data$party, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="")
mosaicplot(table(data$party, data$redist, data$female), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="")

# Different Sortings
mosaicplot(table(data$party, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="")
mosaicplot(table(data$redist, data$party), xlab="Support for Redistribution", ylab="Political Party", las=1, border=F, main="")
mosaicplot(table(data$party.ord, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="")
mosaicplot(table(data$redist, data$party.ord), xlab="Support for Redistribution", ylab="Political Party", las=1, border=F, main="")

# Add Color
mosaicplot(table(data$party, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="", col=brewer.pal(5, "PuOr"))
mosaicplot(table(data$redist, data$party), xlab="Support for Redistribution", ylab="Political Party", las=1, border=F, main="", col=rev(unique(data$part.col)[-6]))
mosaicplot(table(data$party.ord, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="", col=brewer.pal(5, "PiYG"))
mosaicplot(table(data$redist, data$party.ord), xlab="Support for Redistribution", ylab="Political Party", las=1, border=F, main="", col=rev(unique(data$part.col)[-6][c(6,3,1,2,4,5)]))

# Color for Residuals
mosaicplot(table(data$party.ord), xlab="Political Party", border=T, main="", shade=T)
mosaicplot(table(data$party.ord, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=T, main="", shade=T)
mosaicplot(table(data$party.ord, data$redist, data$female), xlab="Political Party", ylab="Support for Redistribution", las=1, border=T, main="", shade=T)


mosaicplot(table(data$party, data$redist, data$female), xlab="Political Party", ylab="Support for Redistribution", las=1, border=T, main="", shade=T)

# Comparing Across Policy Areas
mosaicplot(table(data$party.ord, data$assimilation), xlab="Political Party", ylab="Support for Assimilation", las=1, border=F, main="", col=brewer.pal(5, "PRGn"))
mosaicplot(table(data$party.ord, data$redist), xlab="Political Party", ylab="Support for Redistribution", las=1, border=F, main="", col=brewer.pal(5, "PRGn"))
mosaicplot(table(data$party.ord, data$demreform), xlab="Political Party", ylab="Support for Democratic Reform", las=1, border=F, main="", col=brewer.pal(5, "PRGn"))
mosaicplot(table(data$party.ord, data$women), xlab="Political Party", ylab="Support for Women", las=1, border=F, main="", col=brewer.pal(5, "PRGn"))

# Mosaic Plot With Color for Residuals:  (obs-exp)/sqrt(exp) 
library(vcd)
mosaic(table(data$party), shade=T, direction=c("v"))
mosaic(table(data$party, data$redist), shade=T, direction=c("v", "h"))
mosaic(table(data$party, data$redist, data$female), shade=T, direction=c("v", "h", "v"))

###########################################################################
# Heat Map of Full Data
layout(rbind(c(1,2), c(1,2)), width=c(4,1))
par(mar=c(3,2,3,1))
image(t(as.matrix(data[,5:14])),  col=brewer.pal(5, "PRGn"), axes=F)
axis(1, at=my.scale(1:10, 10), label=colnames(data[,5:14]), las=1, col="white", col.axis="black")

par(mar=c(3,2,3,6))
image(t(as.matrix(c(1:5))), col=brewer.pal(5, "PRGn"), axes=F)
axis(4,  at=my.scale(1:5, 5), label=c("1", "2", "3", "4", "5"), col="white", col.axis="black")

# Order variables by mean support
var.ord <- apply(data[,5:14], 2, mean, na.rm=T)
var.ord <- as.vector(order(var.ord))  

layout(rbind(c(1,2), c(1,2)), width=c(4,1))
par(mar=c(3,2,3,1))
image(t(as.matrix(data[,var.ord+4])),  col=brewer.pal(5, "PRGn"), axes=F)
axis(1, at=my.scale(1:10, 10), label=colnames(data[,var.ord+4]), las=1, col="white", col.axis="black")

par(mar=c(3,2,3,6))
image(t(as.matrix(c(1:5))), col=brewer.pal(5, "PRGn"), axes=F)
axis(4,  at=my.scale(1:5, 5), label=c("1", "2", "3", "4", "5"), col="white", col.axis="black")

# Order oberservations by party
ord <- order(data$party.ord, na.last=T)

layout(rbind(c(1,2), c(1,2)), width=c(4,1))
par(mar=c(3,2,3,1))
image(t(as.matrix(data[ord,5:14])),  col=brewer.pal(5, "PRGn"), axes=F)
axis(1, at=my.scale(1:10, 10), label=colnames(data[,5:14]), las=1, col="white", col.axis="black")

par(mar=c(3,2,3,6))
image(t(as.matrix(c(1:5))), col=brewer.pal(5, "PRGn"), axes=F)
axis(4,  at=my.scale(1:5, 5), label=c("1", "2", "3", "4", "5"), col="white", col.axis="black")

# Order by both

layout(rbind(c(1,2), c(1,2)), width=c(4,1))
par(mar=c(3,2,3,1))
image(t(as.matrix(data[ord, var.ord+4])),  col=brewer.pal(5, "PRGn"), axes=F)
axis(1, at=my.scale(1:10, 10), label=colnames(data[,var.ord+4]), las=1, col="white", col.axis="black")

par(mar=c(3,2,3,6))
image(t(as.matrix(c(1:5))), col=brewer.pal(5, "PRGn"), axes=F)
axis(4,  at=my.scale(1:5, 5), label=c("1", "2", "3", "4", "5"), col="white", col.axis="black")


########################################################################
# Scatterplot Matrix
pairs(data[, 5:14])

##########################
# Improve on Default

# Create own Jittering Function
jit <- function(x){x + runif(length(x), -.3, .3)} 
jitt.data <- matrix(NA, dim(data)[1], 10)

for(i in 5:14){  
  jitt.data[,i-4] <- jit(data[, i])
}

colnames(jitt.data) <- colnames(data[,5:14])

# Plot SPLOM
rand <- sample(1:10, 10, replace=F) # random order
pairs(jitt.data[,rand], upper.panel=NULL, row1attop=F, pch=19, cex=.1, col=rgb(120,00,120, 40, max=255), gap=0)

# Add Smoothers
pairs(jitt.data[,rand], panel=panel.smooth, col.smooth="red", upper.panel=NULL, row1attop=F, pch=19, cex=.1, col=rgb(120,00,120, 40, max=255), gap=0)


########################################################################
# Parallel Coordinates Plot

# Default
parcoord(data[, 5:14], lwd=.3, col=rgb(120,00,120, 255, max=255), var.label=T)

# Default
parcoord(data[, 5:14], lwd=.3, col=rgb(120,00,120, 40, max=255), var.label=T)

# With Jitter & Alpha Blending
parcoord(jitt.data, lwd=.3, col=rgb(120,00,120, 40, max=255))

# Randomize Axes Order
par(mfrow=c(3,1))
rand <- sample(1:10, 10, replace=F) # random order of axes
parcoord(jitt.data[,rand], lwd=.3, col=rgb(120,00,120, 40, max=255), cex.axis=.6)

#  Order Axes According to Mean Support
mean.ord <- apply(data[,5:14], 2, mean, na.rm=T) 
mean.ord <- order(mean.ord)
parcoord(jitt.data[ , mean.ord], lwd=.3, col=rgb(0,0,0, 30, max=255), cex.axis=.6)

# standardize variables

stan.data <- apply(data[,5:14], 2, function(x){
  (x - mean(x, na.rm=T))/sd(x, na.rm=T)
})

plot(stan.data[1,], type="l", col="white", ylim=c(-3, 3), axes=F, ann=F)
abline(v=c(1:10), lwd=.5, col="lightgrey")
for(i in 1:dim(stan.data)[1]){
  points(stan.data[i,], col=rgb(120,00,120, 40, max=255), type="l")
}
axis(1, at=c(1:10), label=colnames(stan.data))
axis(2, las=1)

# again, jitter standardized data
jitt.stan.data <- matrix(NA, dim(data)[1], 10)  
for(i in 1:10){  
  jitt.stan.data[,i-4] <- jit(stan.data[, i])
}

colnames(jitt.stan.data) <- colnames(stan.data)

plot(stan.data[1,], type="l", col="white", ylim=c(-3, 3), axes=F, ann=F)
abline(v=c(1:10), lwd=.5, col="lightgrey")
for(i in 1:dim(stan.data)[1]){
  points(jitt.stan.data[i,], col=rgb(120,00,120, 40, max=255), type="l")
}
axis(1, at=c(1:10), label=colnames(stan.data))
axis(2, las=1)

# Brushing
data$col.code <- ifelse(data$redist>=5, "orange", rgb(00,00,00, 40, max=255))
col.ord <- order(data$col.code, na.last=F) # order, so that colored observations are added last

rand <- sample(1:10, 10, replace=F) # randomize order of axes

parcoord(jitt.data[col.ord, rand], lwd=.3, col=rgb(00,00,00, 40, max=255), cex.axis=.6)

parcoord(jitt.data[col.ord, rand], lwd=.3, col=data$col.code[col.ord], cex.axis=.6)

# Filtering
data$col.code <- ifelse(data$redist>=5, "orange", NA)
col.ord <- order(data$col.code, na.last=F) # order, so that colored observations are added last

parcoord(jitt.data[col.ord, rand], lwd=.3, col=data$col.code[col.ord], cex.axis=.6)



# Examining the Ideology of Parties
data$part.col <- NA
data$part.col[data$party=="AfD"] <- brewer.pal(6, "Set1")[2]
data$part.col[data$party=="FDP"] <- brewer.pal(6, "Set1")[6]
data$part.col[data$party=="Grüne"] <- brewer.pal(6, "Set1")[3]
data$part.col[data$party=="Linke"] <- brewer.pal(6, "Set1")[4]
data$part.col[data$party=="SPD"] <- brewer.pal(6, "Set1")[1]
data$part.col[data$party=="Union"] <- "black"

parcoord(jitt.data[,rand], lwd=.3, col=data$part.col, cex.axis=.6)
parcoord(jitt.data[,rand], lwd=.3, col=data$part.col, cex.axis=.6, add=T)


########################################################################
# Lab Exercise
# Examining Freedom House Data
Political Rights:
A: Vote freely in legitimate elections;
B: Participate freely in the political process;
C: Have representatives that are accountable to them;

Civil Liberties:
D: Exercise freedoms of expression and belief;
E: Be able to freely assemble and associate;
F: Have access to an established and equitable system of rule of law;
G: Enjoy social and economic freedoms, including equal access to economic opportunities and the right to hold private property.

Sub-Categories: Political Rights  				Sub-Categories: Civil Liberties				
A: Electoral Process					D: Freedom of Expression and Belief				
B: Political Pluralism and Participation					E: Associational and Organizational Rights				
C: Functioning of Government					F: Rule of Law				
G: Personal Autonomy and Individual Rights				

FH14 <- read.csv("Teaching/Lehre Frankfurt/1415 Datenvisualisierung/freedomhouse2014.csv", sep=";")

# rename variables
FH14$elections <- FH14$A
FH14$pluralism <- FH14$B
FH14$government <- FH14$C
FH14$expression <- FH14$D
FH14$association <- FH14$E
FH14$ruleoflaw <- FH14$F
FH14$individual <- FH14$G

FH14.subset <- FH14[, 12:18]
rand <- sample(1:7, 7, replace=F)
parcoord(FH14.subset[, rand], lwd=.3, col=rgb(0,0,0, 120, max=255), cex.axis=.6, var.label=T)

jitt.data <- matrix(NA, 195,7)
for(i in 1:7){  
  jitt.data[,i] <- jit(FH14.subset[, i])
}
colnames(jitt.data) <- colnames(FH14.subset)
parcoord(jitt.data[, rand], lwd=.3, col=rgb(0,0,0, 180, max=255), cex.axis=.6, var.label=T)


test.dat <- cbind(c(1, 2, 3), c(1, 2, 3))

plot(test.dat, pch=19)
parcoord(test.dat)
axis(2, at=c(1, 2, 3))


#######################################################################
