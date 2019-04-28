############################################################################
# Visual Inference
###########################################################################################
# Packages
library(foreign)
library(maps)
library(mapdata)
library(RColorBrewer)

############################################################################################################
# Visual Inference: Scatter Plot 

slop <- read.dta("slop_2009_agg_example.dta")

# Generate Random Plot Placement
placement <- sample((1:20), 20)
layout(matrix(placement, 4, 5))

# Plot 19 Null Plots
par(mar=c(.1, .1, .1, .1))
for(i in 1:19){
  random <- sample(c(1:dim(slop)[1]), dim(slop)[1])
  
  plot(slop$mkath[random], slop$cdu, axes=F, ann=F, cex=.4)
  box(bty="l", col="grey")
  
}

# Add True Plot
  plot(slop$mkath, slop$cdu, axes=F, ann=F, cex=.4)
  box(bty="l", col="grey")

# Reveal True Plot
box(col="red", lty=2, lwd=2)
which(placement==20)

############################################################################
# Visual Inference: Scatter Plot II 

# Generate Random Plot Placement
placement <- sample((1:20), 20)
layout(matrix(placement, 4, 5))

# Plot 19 Null Plots
par(mar=c(.1, .1, .1, .1))
for(i in 1:19){
  random <- sample(c(1:dim(slop)[1]), dim(slop)[1])
  
  plot(slop$mkath[random], slop$cdu, axes=F, ann=F, cex=.4)
  abline(lm(slop$cdu ~ slop$mkath[random]))
  box(bty="l", col="grey")
  
}

# Add True Plot
  plot(slop$mkath, slop$cdu, axes=F, ann=F, cex=.4)
  abline(lm(slop$cdu ~ slop$mkath))
  box(bty="l", col="grey")

# Reveal True Plot
box(col="red", lty=2, lwd=2)
which(placement==20)

###########################################################################
# Visual Inference: Group Comparison

# Generate Random Plot Placement
placement <- sample((1:20), 20)
layout(matrix(placement, 4, 5))

# Plot 19 Null Plots
par(mar=c(.1, .1, .1, .1))
for(i in 1:19){
  random <- sample(c(1:dim(slop)[1]), dim(slop)[1])
  
  plot(slop$bayern[random], slop$cdu, axes=F, ann=F, cex=.4, xlim=c(-1, 2))
  points(1, mean(slop$cdu[slop$bayern[random]==1]),   pch="-", col="purple4", cex=3)
  points(0, mean(slop$cdu[slop$bayern[random]==0]),   pch="-", col="darkolivegreen2", cex=3)
  box(bty="l", col="grey")
  
}

# Add True Plot
plot(slop$bayern, slop$cdu, axes=F, ann=F, cex=.4, xlim=c(-1, 2))
points(1, mean(slop$cdu[slop$bayern==1]),   pch="-", col="purple4", cex=3)
points(0, mean(slop$cdu[slop$bayern==0]),   pch="-", col="darkolivegreen2", cex=3)
box(bty="l", col="grey")

# Reveal True Plot
box(col="red", lty=2, lwd=2)
which(placement==20)

############################################################################
# Visual Inference: : Dot Map I 

data <- readRDS("sub_data.rds")
data$col <- ifelse(data$status=="kein Kontakt", "maroon3", "darkolivegreen2")

# Generate Random Plot Placement
placement <- sample((1:20), 20)
layout(matrix(placement, 4, 5))

# Plot 19 Null Plots
par(mar=c(.01, .01, .01, .01), oma=c(0, 0, 0, 0))
for(i in 1:19){
  random <- sample(c(1:15591), 15591) # generate random order
  map(database="worldHires", fill=F, col="darkgrey", xlim=c(6, 15), ylim=c(47.3, 55))
  points(data$g_lon, data$g_lat, cex=.1, pch=19, col=data$col[random])
  
}

# Add True Plot
map(database="worldHires", fill=F, col="darkgrey", xlim=c(6, 15), ylim=c(47.3, 55))
points(data$g_lon, data$g_lat, cex=.1, pch=19, col=data$col)

# Reveal True Plot
box(col="red", lty=2, lwd=2)
which(placement==20)

###############################################################################################
# Visual Inference: Dot Map II

par(mar=c(.01, .01, .01, .01))

# Generate Random Plot Placement
placement <- sample((1:20), 20)
layout(matrix(placement, 4, 5))

# Plot 19 Null Plots
for(i in 1:19){
  random <- sample(c(1:15591), 1831)
  map(database="worldHires", fill=F, col="darkgrey", xlim=c(6, 15), ylim=c(47.3, 55))
  points(data$g_lon[random], data$g_lat[random], cex=.3, pch=19, col=rgb(205, 41, 144, 10, max=255))
  
}

# Add True Plot
map(database="worldHires", fill=F, col="darkgrey", xlim=c(6, 15), ylim=c(47.3, 55))
points(data$g_lon[data$status=="kein Kontakt"], data$g_lat[data$status=="kein Kontakt"], cex=.3, pch=19, col=rgb(205, 41, 144, 10, max=255))

# Reveal True Plot
box(col="red", lty=2, lwd=2)
which(placement==20)

#############################################################################



