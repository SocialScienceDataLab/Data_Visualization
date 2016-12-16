####################################################################################################
# Visual Comparisons
library(RColorBrewer)

data <- readRDS("/Users/richardtraunmuller/Dropbox/footballdata.rds")
f.data <- data[data$Team=="ACF Fiorentina",] # pick AC Fiorentina

#####################################################################################################
### Simple Barplot in R

par(mar=c(3,8,1,2), tck=-0.02)
barplot(f.data$Attack, names.arg=f.data$Name)

barplot(f.data$Attack, names.arg=f.data$Name, horiz=T, cex.name=.6, xlab="Attack", las=1)

ord <- order(f.data$Attack) # Never underestimate a simple sort!
barplot(f.data$Attack[ord], names.arg=f.data$Name[ord], horiz=T, cex.name=.6, xlab="Attack")

ord <- order(f.data$Attack) # Compare to a standard
barplot(f.data$Attack[ord], names.arg=f.data$Name[ord], horiz=T, cex.name=.6, border=F, space=.5, col="purple3", xlab="Attack")
  grid(NULL, NA, col="white", lty="solid", lwd=0.7)
    abline(v=mean(f.data$Attack), lty=1, col="red", lwd=2)
      text(mean(f.data$Attack), 1, "Average", cex=.6, col="red", pos=4)

############################################################################
# Compare by Position

bar.names <- c()
bar.length <- c()

for (i in length(unique(f.data$Position)):1) {
  
  bar.names <- c(bar.names, NA, toupper(unique(f.data$Position)[i]))
  bar.length <- c(bar.length, NA, NA)
  
  position <- subset(f.data, Position==unique(data$Position)[i])
  ord <- order(position$Attack, decreasing=T)
  bar.names <- c(bar.names, position$Name[ord])
  bar.length <- c(bar.length, position$Attack[ord])
  
}

par(mfrow=c(1,1), mar=c(3,8,2,2))
barplot(rev(bar.length), names.arg=rev(bar.names), horiz=T, cex.name=.6, cex.axis=.8, border=F, space=.4, col="purple4", xlab="Attack")
  grid(NULL, NA, col="white", lty=1)
   abline(v=mean(f.data$Attack), lty=1, col="red")
    text(mean(f.data$Attack), 1, "Average", cex=.6, col="red", pos=4)

########################################################################################
# Add a second Variable
bar.names <- c()
bar.length <- c()
bar.length.2 <- c()

for (i in length(unique(f.data$Position)):1) {
  
  bar.names <- c(bar.names, NA, toupper(unique(f.data$Position)[i]))
  bar.length <- c(bar.length, NA, NA)
  bar.length.2 <- c(bar.length.2, NA, NA)
  
  position <- subset(f.data, Position==unique(data$Position)[i])
  ord <- order(position$Attack, decreasing=T)
  bar.names <- c(bar.names, position$Name[ord])
  bar.length <- c(bar.length, position$Attack[ord])
  bar.length.2 <- c(bar.length.2, position$Defence[ord])
}

barplot(t(cbind(rev(bar.length.2), rev(bar.length))), names.arg=rev(bar.names), beside=T, horiz=T, cex.name=.6, cex.axis=.8, border=F, col=c("maroon3", "darkolivegreen2"), xlim=c(0, 90))
grid(NULL, NA, col="white", lty=1)

#########################################################################
# Probably better to split it in two graphs

par(mfrow=c(1,3))
par(mar=c(3,1,1,0))
 plot(rep(1, 33), c(1:33), pch="", xlim=c(.9,1), axes=F, ann=F)
  text(rep(1, 33), c(1:33), rev(bar.names), cex=.6, pos=2)

par(mar=c(3,1,1,2))
 barplot(rev(bar.length),  horiz=T, cex.axis=.8, border=F, space=.4, col="darkolivegreen2", xlab="Attack")
  grid(NULL, NA, col="white", lty=1)

par(mar=c(3,1,1,2), tck=-0.02)
 barplot(rev(bar.length.2),  horiz=T,  cex.axis=.8, border=F, space=.4, col="maroon3", xlab="Defence")
  grid(NULL, NA, col="white", lty=1)

#######################################################################################################
# Dot Plot as an Alternative
par(mfrow=c(1,1), mar=c(3,8,1,2))
plot(rev(bar.length), c(1:33), pch=19, col="purple4", cex=.8, axes=F, xlim=c(30, 100), ylab="", xlab="")
axis(2, at=c(1:33), label=rev(bar.names), cex.axis=.6, col="white")
axis(1, cex.axis=.8)

rect(30, 26.5, 100, 31.5, col="grey92", border=F)
rect(30, 15.5, 100, 24.5, col="grey92", border=F)
rect(30, 4.5, 100, 13.5, col="grey92", border=F)
rect(30, 0.5, 100, 2.5, col="grey92", border=F)

points(rev(bar.length), c(1:33), pch=19, col="darkolivegreen2", cex=.6)
points(rev(bar.length.2), c(1:33), pch=19, col="maroon3", cex=.6)
grid(NULL, NA, col="white", lty=1)

##########################################################################
# Maybe this is also better in separate plots
par(mfrow=c(1,3))

par(mar=c(3,1,1,0))
plot(rep(1, 33), c(1:33), pch="", xlim=c(.9,1), axes=F, ann=F)
  text(rep(1, 33), c(1:33), rev(bar.names), cex=.6, pos=2)

par(mar=c(3,1,1,2))
plot(rev(bar.length), c(1:33), pch=19, axes=F, xlim=c(30, 100), ylab="", xlab="")
  axis(1, cex.axis=.8)
   rect(30, 26.5, 100, 31.5, col="grey92", border=F)
   rect(30, 15.5, 100, 24.5, col="grey92", border=F)
   rect(30, 4.5, 100, 13.5, col="grey92", border=F)
   rect(30, 0.5, 100, 2.5, col="grey92", border=F)
points(rev(bar.length), c(1:33), pch=19, col="darkolivegreen2", cex=.6)


par(mar=c(3,1,1,2))
plot(rev(bar.length.2), c(1:33), pch=19, axes=F, xlim=c(30, 100), ylab="", xlab="")
  axis(1, cex.axis=.8)
    rect(20, 26.5, 100, 31.5, col="grey92", border=F)
    rect(20, 15.5, 100, 24.5, col="grey92", border=F)
    rect(20, 4.5, 100, 13.5, col="grey92", border=F)
    rect(20, 0.5, 100, 2.5, col="grey92", border=F)
points(rev(bar.length.2), c(1:33), pch=19, col="maroon3", cex=.6)



####################################################################################################
# Yet Another Way to Compare Positions

# Define Colors for Different Subgroups
my.col <- brewer.pal(6, "Set2")[3:6]
my.col.pos <- my.col[data$Position]
my.col.g <- ifelse(data$Position=="Goalkeeper", "#8DA0CB", "grey")
my.col.d <- ifelse(data$Position=="Defender", "#E78AC3", "grey")
my.col.m <- ifelse(data$Position=="Midfielder", "#A6D854", "grey")
my.col.f <- ifelse(data$Position=="Forward", "#FFD92F", "grey")

my.col.bay <- ifelse(data$Team=="FC Bayern München", "red", "grey")
my.col.acf <- ifelse(data$Team=="ACF Fiorentina", "purple", "grey")


# Scatterplot
par(mfrow=c(1,1))
plot(data$Attack, data$Defence, pch=19, col=my.col.pos, cex=.5)

par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(data$Attack, data$Defence, pch=19, col=my.col.g, cex=.5)
plot(data$Attack, data$Defence, pch=19, col=my.col.d, cex=.5)
plot(data$Attack, data$Defence, pch=19, col=my.col.m, cex=.5)
plot(data$Attack, data$Defence, pch=19, col=my.col.f, cex=.5)


#####################################################################################################
# Attack vs. Defence
par(mfrow=c(2,2))
for(i in 1:4){
  
  plot(data$Attack[data$League==unique(data$League)[i]], data$Defence[data$League==unique(data$League)[i]], pch=19, col=my.col.pos[data$League==unique(data$League)[i]], cex=.5, main=unique(data$League)[i], xlab="Attack", ylab="Defence", xlim=c(0, 100), ylim=c(0,100))
  
}

# Attack vs. Defence
par(mfrow=c(4,4))
for(i in 1:4){
  
  plot(data$Attack[data$League==unique(data$League)[i]], data$Defence[data$League==unique(data$League)[i]], pch=19, col=my.col.g[data$League==unique(data$League)[i]], cex=.5, main=unique(data$League)[i], xlab="Attack", ylab="Defence", xlim=c(0, 100), ylim=c(0,100))
  plot(data$Attack[data$League==unique(data$League)[i]], data$Defence[data$League==unique(data$League)[i]], pch=19, col=my.col.d[data$League==unique(data$League)[i]], cex=.5, main=unique(data$League)[i], xlab="Attack", ylab="Defence", xlim=c(0, 100), ylim=c(0,100))
  plot(data$Attack[data$League==unique(data$League)[i]], data$Defence[data$League==unique(data$League)[i]], pch=19, col=my.col.m[data$League==unique(data$League)[i]], cex=.5, main=unique(data$League)[i], xlab="Attack", ylab="Defence", xlim=c(0, 100), ylim=c(0,100))
  plot(data$Attack[data$League==unique(data$League)[i]], data$Defence[data$League==unique(data$League)[i]], pch=19, col=my.col.f[data$League==unique(data$League)[i]], cex=.5, main=unique(data$League)[i], xlab="Attack", ylab="Defence", xlim=c(0, 100), ylim=c(0,100))
  
}

###########################################################################
