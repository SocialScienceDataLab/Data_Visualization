
setwd("/Users/richardtraunmuller/Dropbox/Teaching/Essex 2016/")

# Freedom House Data 03-14
data <- read.csv("FH_disaggregated_03_14.csv", sep=";")

# turn into numeric variable type
for(i in 2:25){
 data[,i] <- as.numeric(data[,i])  
}

# Define some paramaters used for the plots later 
startYear <- 2003
endYear <- 2014

maxVal <- max(data[,2:13], na.rm=T) + 1
minVal <- min(data[,2:13], na.rm=T) - 1

which(data$Country=="United Kingdom")
which(data$Country=="Germany")
which(data$Country=="Norway")


# optional: sample 30 countries 
data <- data[sample(1:197, 30),]

#########################################################################
# Plot a Simple Line Chart of Political Rights over Time

# R Default
par(mar=c(3,3,2,2), yaxs="r")
plot(0, 0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), pch="",  ylab="Political Rights Score", xlab="Year")

for(i in 1:197){
   lines(c(startYear:endYear), data[i,2:13])
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col="red", lwd=3)
lines(c(startYear:endYear), data[which(data$Country=="Germany"),2:13], col="yellow", lwd=3)
lines(c(startYear:endYear), data[which(data$Country=="Norway"),2:13], col="green", lwd=3)


#############################################################################
### Re-designs

### Newspaper
par(mar=c(4, 4, 3, 1), oma=c(0,0,0,0), xaxs="r", yaxs="i", mgp=c(2.1,.6,0), las=1, lend=1)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, cex.axis=0.8, cex.lab=0.8, main="Democracy Levels in Comparative Perspective", xlab="", ylab=expression(bold("Political Rights")), family="Helvetica")

grid(NA, NULL, col="black", lty="dotted", lwd=0.3)

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col="dark grey", lwd=0.7)
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col=rgb(36, 74, 93, 255, max=255), lwd=3)


### Feltron
par(bg=rgb(54, 57, 74, 255, max=255), mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xaxs="r", yaxs="i", mgp=c(2.8,0.3,0.5), col.lab="white", col.axis="white", col.main="white", font.main=1, cex.main=0.8, cex.axis=0.8, cex.lab=0.8, family="Helvetica", tck=0, las=1)

plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main="DEMOCRACY LEVELS IN COMPARATIVE PERSPECTIVE", xlab="", ylab="", xaxt="n", yaxt="n")
axis(1, tick=FALSE, col.axis="white")
axis(2, tick=FALSE, col.axis="white")

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col="white", lwd=0.35)
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col=rgb(227, 223, 12, 255, max=255), lwd=3)


### FiveThirtyFive
par(mar=c(3, 4, 3, 2), oma=c(0,0,0,0), bg=rgb(240, 240, 240, 255, max=255), xaxs="r", yaxs="i", mgp=c(2.1,.3,0), las=1, col.axis=rgb(67, 67, 67, 255, max=255), col.main=rgb(67, 67, 67, 255, max=255), tck=0)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main="Democracy Levels in Comparative Perspective", xlab="", ylab="Political Rights", family="Helvetica", cex.main=1.5, cex.axis=0.8, cex.lab=0.8, xaxt="n", yaxt="n")
grid(NULL, NULL, col=rgb(222, 222, 222, 255, max=255), lty="solid", lwd=0.9)
axis(1, tick=FALSE, cex.axis=0.9)
axis(2, tick=FALSE, cex.axis=0.9)

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col="dark grey", lwd=1)
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col=rgb(0, 142, 212, 255, max=255), lwd=3)


### The Economist
par(oma=c(0,0,0,0), mar=c(0,0,0,0), bg=rgb(220, 230, 236, 255, max=255), xaxs="i", yaxs="i")
plot(0, 0, type = "n", bty = "n", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100))
rect(0,100,2,94, col="red", border=NA)

par(mar=c(4, 3, 3, 2), oma=c(0,0,0,0), xaxs="i", yaxs="i", mgp=c(1.8,.2,0), cex.main=1.5, cex.axis=0.7, cex.lab=0.7, col.lab="black", col.axis="black", col.main="black", tck=0.02, yaxp=c(minVal, maxVal, 2), new=TRUE)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main=expression(bold("Democracy Levels in Comparative Perspective")), xlab=expression(italic("Years")), ylab=expression(italic("Political Rights")), family="Helvetica")
grid(NA, NULL, col="white", lty="solid", lwd=1.5)

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col=rgb(51, 165, 162, 255, max=255), lwd=1.25)
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col=rgb(0, 142, 212, 255, max=255), lwd=3)


### Tukey
par(las=1, tck=0.02, mgp=c(2.8,0.3,0.5), cex.lab=0.85, cex.axis=0.8, cex.main=0.9, bg="white")
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", main=expression(bold("Democracy Levels in Comparative Perspective")), xlab="", ylab="Political Rights")

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col=rgb(204, 204, 204, 255, max=255), lwd=1.25)
}

points(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], lwd=1.2, type="o", pch=4)

### Old School Console
par(bg="black", mar=c(4,4,3,2), las=1, tck=0, mgp=c(2.8,0.3,0), cex.lab=0.85, cex.axis=0.8, cex.main=0.9, col.axis="white", col.main="white", col.lab="white")
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", main=expression(bold("Democracy Levels in Comparative Perspective")), xlab="Year", ylab="Political Rights")
grid(NULL, NULL, lty="solid", col="white", lwd=0.5)

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col=rgb(243, 11, 170, 255, max=255), lwd=1.2)
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], lwd=3, col="green")

###################################################################################################

### What Would Tufte Do?!

### My Design:

par(mar=c(4,4,4,2), yaxs="i", bg="white", fg="black", col.axis="black", col.lab="black", col.main="black", family="Gill Sans MT", mgp=c(2, .35, 0))
plot(0, 0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), pch="", main=expression(bold("Democracy Levels in Comparative Perspective")), ylab="Political Rights Score", xlab="Year", axes=F)
axis(1)
axis(2, col="white")
axis(4, at=data[which(data$Country=="United Kingdom"),13], col.axis="maroon3", label="UK", )

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13], col=rgb(0, 0, 0, 50, max=255))
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col="maroon3", lwd=3)
box(bty="l")




#######################################################################
# Building a Few-Style Bullet Graph

measure <- 7
standard <- 6.8

par(mar=c(3,8,2,2), oma=c(13, 1, 13, 1), yaxs="i")
plot(0, 0, xlim=c(0, 10), ylim=c(.45, .55), pch="",  ylab="", xlab="", axes=F)
axis(1)
rect(0, .45, 10, .55, col="grey90", border=F)
rect(6, .45, 10, .55, col="grey94", border=F)
rect(8, .45, 10, .55, col="grey98", border=F)
segments(0, .5, measure, .5, lwd=10, lend=1)
points(standard, .5, pch="|", col="red", cex=2)
axis(2, at=.5, label="Some Variable")


par(mar=c(3,8,2,2), oma=c(4, 1, 4, 1), yaxs="i")
p <- barplot(as.numeric(data[1:4, 4]), horiz=T, space=4, border=F, col="black", ylim=c(0, 22), axes=F)

for(i in 1:4){
  rect(0, p[i]-1, 25, p[i]+1, col="grey90", border=F)
  rect(25, p[i]-1, 30, p[i]+1, col="grey94", border=F)
  rect(30, p[i]-1, 35, p[i]+1, col="grey98", border=F)
}

barplot(as.numeric(data[1:4, 4]), horiz=T, space=4, border=F, col="black", ylim=c(0, 22), add=T, names.arg=c("A", "B", "C", "D"))
points(c(10, 13, 8, 30), p, pch="|", cex=2, col="red")

#######################################################################
# Building a Tufte-Style Slope Graph

