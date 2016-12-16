# Load Packages
  library(arm)
  library(foreign)
  library(RColorBrewer)

# Set Working Directory
  setwd("/Users/richardtraunmuller/Dropbox/Teaching/Essex 2016/")

# Load and Generate Data
  daten05 <- read.dta("slop_bw2005_agg.dta")
  daten05$bayern[daten05$kreis > 9000 & daten05$kreis < 10000] <- 1
  daten05$bayern[daten05$kreis < 9000 | daten05$kreis > 10000] <- 0
  daten05$badwue <- 0
  daten05$badwue[daten05$kreis > 8000 & daten05$kreis < 8000] <- 1

# Simple Examples
  par(mfrow=c(2,1), mgp=c(2, .5, 0), xaxs="r", yaxs="r", bg="white", fg="black", family="Gill Sans MT")
  
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  points(daten05$kath, daten05$cdu, pch=19)
  box(bty="l")

  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5))
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"), mgp=c(2,1,0), cex.axis=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  points(daten05$bayern, daten05$cdu, pch=19) 
  box(bty="l")

#######################################################################
### Solutions for Overplotting
  
  par(mfrow=c(3,2), mar=c(3,3,1,2), fg="black", bg="white", col.lab="black", col.axis="black", tck=0)

  # Size of Plot Symbols
  plot(daten05$kath, daten05$cdu, axes=F, pch="", cex.lab=1, tck=-.01, main="", xlab="Share of Catholics", ylab="Vote Share CDU/CSU %", las=1, cex=.5)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l")
  points(daten05$kath, daten05$cdu, pch=19, cex=.5)
  
  
  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), cex=.3)
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(daten05$bayern, daten05$cdu, pch=19, cex=.5)
  
# Remove Fill Color
  plot(daten05$kath, daten05$cdu, axes=F, pch="", cex.lab=1, tck=-.01, main="", xlab="Share of Catholics", ylab="Vote Share CDU/CSU %", las=1, cex=.5)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l")
  points(daten05$kath, daten05$cdu, pch=21)
  
  
  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), cex=.3)
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(daten05$bayern, daten05$cdu, pch=21)
  
  
# Other Symbols
  plot(daten05$kath, daten05$cdu, axes=F, pch="", cex.lab=1, tck=-.01, main="", xlab="Share of Catholics", ylab="Vote Share CDU/CSU %", las=1, cex=.5)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l")
  points(daten05$kath, daten05$cdu, pch=4)
  
  
  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), cex=.3)
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(daten05$bayern, daten05$cdu, pch=4)
  
  
# Jittering
  plot(daten05$kath, daten05$cdu, axes=F, pch="", cex.lab=1, tck=-.01, main="", xlab="Share of Catholics", ylab="Vote Share CDU/CSU %", las=1, cex=.5)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l")
  points(jitter(daten05$kath), daten05$cdu, pch=19)
  
  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), cex=.3)
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(jitter(daten05$bayern), daten05$cdu, pch=19)
  
  
# Alpha Blending
  plot(daten05$kath, daten05$cdu, axes=F, pch="", cex.lab=1, tck=-.01, main="", xlab="Share of Catholics", ylab="Vote Share CDU/CSU %", las=1, cex=.5)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l")
  points(jitter(daten05$kath), daten05$cdu, pch=19, col=rgb(00, 00, 00, 50, max=255))
  
  plot(daten05$bayern, daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), cex=.3)
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(jitter(daten05$bayern), daten05$cdu, pch=19, col=rgb(00, 00, 00, 50, max=255))



   
  
# "Big Data" Example  
  DATA <- readRDS("/Users/richardtraunmuller/Dropbox/rep_hegsamb/DATA_2.rds")
  
  par(las=1)
  plot(DATA$dev_diff[DATA$variable=="milper"], DATA$Est.z[DATA$variable=="milper"],  pch=21, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
  plot(DATA$dev_diff[DATA$variable=="milper"], DATA$Est.z[DATA$variable=="milper"], pch=19, cex=.1, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
  plot(DATA$dev_diff[DATA$variable=="milper"], DATA$Est.z[DATA$variable=="milper"],  col=rgb(0, 0, 0, 30, max=255), pch=19, cex=.1, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
# Density Encoding
  library(hexbin)
  hexbinplot(DATA$Est.z[DATA$variable=="milper"] ~ DATA$dev_diff[DATA$variable=="milper"], xlab="Model Fit", ylab="Coefficient Estimate")
  
# My own crack on it 
  layout( rbind(c(1,1), c(2,2)), height=c(3,2))
  layout.show(2)
  
  bin <- hexbin(DATA$dev_diff[DATA$variable=="milper"], DATA$Est.z[DATA$variable=="milper"]) 
  col.scheme <- colorRamp(c("yellow", "red", "purple"), bias=2.5)
  bincount <- (bin@count - min(bin@count)) / (max(bin@count)-min(bin@count))
  
  par(xaxs="r", yaxs="r")
  plot(bin@xcm, bin@ycm, col=rgb(col.scheme(bincount)[,1], col.scheme(bincount)[,2], col.scheme(bincount)[,3], 255, max=255), pch=18, cex=2.5, axes=F, ylab="Coefficient Estimate", xlab="Model Fit")
  axis(1, col="white")
  axis(2, las=1, col="white")
  
  pal <- colorRampPalette(c("yellow", "red", "purple"), bias=2.5)
  par(xaxs="i", yaxs="i")
  hist(bin@count, breaks=100, col=pal(100), main="", border=F, axes=F, xlab="Bin Count")
  axis(1, col="white")
  axis(2, col="white")
  
  
# Sampling
  samp.20 <- sample(length(DATA$Est.z[DATA$variable=="milper"]), round(length(DATA$Est.z[DATA$variable=="milper"])*.2))
  samp.10 <- sample(length(DATA$Est.z[DATA$variable=="milper"]), round(length(DATA$Est.z[DATA$variable=="milper"])*.1))
  samp.05 <- sample(length(DATA$Est.z[DATA$variable=="milper"]), round(length(DATA$Est.z[DATA$variable=="milper"])*.05))
  
  plot(DATA$dev_diff[DATA$variable=="milper"][samp.20], DATA$Est.z[DATA$variable=="milper"][samp.20], pch=19, cex=.1, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
  plot(DATA$dev_diff[DATA$variable=="milper"][samp.10], DATA$Est.z[DATA$variable=="milper"][samp.10], pch=19, cex=.1, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
  plot(DATA$dev_diff[DATA$variable=="milper"][samp.05], DATA$Est.z[DATA$variable=="milper"][samp.05], pch=19, cex=.1, axes=F, xlab="Model Fit", ylab="Coefficient Estimate")
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
########################################################################  
### Enhancing Scatter Plots
  
# Scatterplot with rugs
  par(mfrow=c(1,1), mar=c(3,3,3,3), oma=c(1,1,1,1), tck=0)
  par(mgp=c(2, .5, 0), mar=c(3,3,.2,1))
  plot(daten05$kath, daten05$cdu, pch=19, axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Share CDU/CSU %", las=1, xlim=c(0, .9), ylim=c(.1, .6))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, col="white")
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1, col="white")

  rug(daten05$kath, side=1)
  rug(daten05$cdu, side=2)
  
  
# Simple Boxplot
  boxplot(daten05$kath, axes=F, horizontal=T)

# Scatterplot with Boxplots
  layout(rbind(c(1,1,1,4), c(2,2,2,3), c(2,2,2,3), c(2,2,2,3)))
  layout.show(4)
  
  par(mar=c(.1,3,3,1))
  boxplot(daten05$kath, axes=F, horizontal=T)
  
  par(mgp=c(2, .5, 0), mar=c(3,3,.2,1), xaxs="r", yaxs="r")
  plot(daten05$kath, daten05$cdu, pch=19, axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1)
  box(bty="l") 
  
  par(mar=c(3,0,.2,4))
  boxplot(daten05$cdu, axes=F, horizontal=F)

### Alternative Design
 par(mfrow=c(1,1), mar=c(3,3,.2,1)) 
  
  plot(daten05$kath, daten05$cdu, pch=19, axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Share CDU/CSU %", las=1, ylim=c(.1, .55), xlim=c(-.05, .9))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, col="white")
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1, col="white")
  boxplot(daten05$kath, at=.1, axes=F, horizontal=T, add=T, boxwex=.05)
  boxplot(daten05$cdu, at=-.05, axes=F, horizontal=F, add=T, boxwex=.10)
  
### Slicing
  par(mfrow=c(2,1), mgp=c(2, .5, 0), mar=c(3,3,.2,1))
  plot(daten05$kath, daten05$cdu, pch=19, axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  abline(v=c(.10, .30, .50, .70), lty=2, lwd=2, col="red")
  box(bty="l")
  
  plot(daten05$kath, daten05$cdu, pch=19, axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1, col="lightgrey")
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1,  tck=-.01,)
  axis(1, at=c(.05, .2, .4, .6, .8), labels=c("5", "20", "40", "60", "80"), las=1,  tck=-.01,)
  box(bty="l")
  
  boxplot(daten05$cdu[daten05$kath < .1], add=T, at=.05, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .1 & daten05$kath < .3], add=T, at=.2, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .3 & daten05$kath < .5], add=T, at=.4, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .5 & daten05$kath < .7], add=T, at=.6, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .7], add=T, at=.8, boxwex=.1, axes=F)
  
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, tck=-.01, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1,  tck=-.01,)
  axis(1, at=c(.05, .2, .4, .6, .8), labels=c("<10", "<20", "<50", "<70", "<90"), las=1,  tck=-.01,)
  box(bty="l")
  
  boxplot(daten05$cdu[daten05$kath < .1], add=T, at=.05, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .1 & daten05$kath < .3], add=T, at=.2, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .3 & daten05$kath < .5], add=T, at=.4, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .5 & daten05$kath < .7], add=T, at=.6, boxwex=.1, axes=F)
  boxplot(daten05$cdu[daten05$kath >= .7], add=T, at=.8, boxwex=.1, axes=F)
    
  
### Fitting
# Parametric
  par(mfrow=c(1,2), mgp=c(2,.5,0), tck=-.02)
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  box(bty="l")
  
  points(daten05$kath, daten05$cdu, pch=19, col=rgb(120, 00, 120, 50, max=255))
  abline(lm(daten05$cdu~daten05$kath), col=rgb(.47, 00, .47))
  text(.55, .20, "y=.26+.19*x", col=rgb(.47, 00, .47), pos=4, cex=.9)
  text(.55, .17, "R2=.38", col=rgb(.47, 00, .47), pos=4, cex=.9) 

  plot(jitter(daten05$bayern), daten05$cdu, pch="", cex.lab=1, tck=-.01, axes=F, main="", xlab="", ylab="Vote Share CDU/CSU %", las=1, xlim=c(-.5,1.5), col=rgb(120, 00, 120, 50, max=255))
  axis(1, at=c(0,1), labels=c("Other State", "Bavaria"), mgp=c(2,1,0))
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1)
  box(bty="l")
  points(jitter(daten05$bayern), daten05$cdu, pch=19,  col=rgb(120, 00, 120, 50, max=255))
  abline(lm(daten05$cdu~daten05$bayern), col=rgb(.47, 00, .47))
  text(.7, .20, "y=.31+.09*x", col=rgb(.47, 00, .47), pos=4, cex=.9)
  text(.7, .17, "R2=.38", col=rgb(.47, 00, .47), pos=4, cex=.9)

  
# Linear regression line
  ols1 <- lm(daten05$cdu~daten05$kath)
  display(ols1)
  
  ols2 <- lm(daten05$cdu~daten05$bayern)
  display(ols2)
  
# Parametric Example 2
  par(mfrow=c(1,2), mgp=c(2,.5,0), tck=-.02)
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Katholikenanteil %", ylab="Anteil CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
 
  points(daten05$kath[daten05$bayern==0], daten05$cdu[daten05$bayern==0], pch=19, col=rgb(120, 00, 120, 100, max=255))
  abline(lm(daten05$cdu[daten05$bayern==0]~daten05$kath[daten05$bayern==0]), col=rgb(.47, 00, .47))
  text(.55, .23, "y=.27+.12*x", col=rgb(.47, 00, .47), pos=4, cex=.9)
  text(.55, .21, "R2=.29", col=rgb(.47, 00, .47), pos=4, cex=.9) 
  text(.55, .25, "Other State", col=rgb(.47, 00, .47), pos=4, cex=.9)
  box(bty="l")
  
  points(daten05$kath[daten05$bayern==1], daten05$cdu[daten05$bayern==1], pch=19, col=rgb(255, 165, 00, 100, max=255))
  abline(lm(daten05$cdu[daten05$bayern==1]~daten05$kath[daten05$bayern==1]), col=rgb(1, .65, 00))
  text(.55, .23, "y=.31+.15*x", col=rgb(1, .65, 00), pos=4, cex=.9)
  text(.55, .21, "R2=.26", col=rgb(1, .65, 00), pos=4, cex=.9)
  text(.55, .25, "Bavaria", col=rgb(1, .65, 00), pos=4, cex=.9)
  box(bty="l")
  
  
# loess 
  par(mar=c(3,3,3,1), mfrow=c(2,2), mgp=c(2,.5,0), tck=-.02)
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  lines(loess.smooth(daten05$kath, daten05$cdu, span=.8, degree=1), col=rgb(.47, 00, .47), lwd=2)
  box(bty="l")
  
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  lines(loess.smooth(daten05$kath, daten05$cdu, span=.5, degree=1), col=rgb(.47, 00, .47), lwd=2)
  box(bty="l")
  
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  lines(loess.smooth(daten05$kath, daten05$cdu, span=.3, degree=1), col=rgb(.47, 00, .47), lwd=2)
  box(bty="l")
  
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  lines(loess.smooth(daten05$kath, daten05$cdu, span=.1, degree=1), col=rgb(.47, 00, .47), lwd=2)
  box(bty="l")

# Add Confidence Intervalls
   
  par(mfrow=c(2,2))
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  #points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  box(bty="l")
  
  # Extract Fitted Values and SEs from loess
  l.p <- predict(loess(daten05$cdu ~ daten05$kath, span=.3), se=T)
  l.p <- cbind(daten05$kath, l.p$fit, l.p$fit - qt(0.975, l.p$df)*l.p$se, l.p$fit + qt(0.975, l.p$df)*l.p$se)
  l.p <- l.p[order(l.p[,1]),]
  
  # Plot as Lines
  lines(l.p[,1], l.p[,2], col=rgb(.47, 00, .47))
  lines(l.p[,1], l.p[,3], col=rgb(.47, 00, .47), lty=2)
  lines(l.p[,1], l.p[,4], col=rgb(.47, 00, .47), lty=2)
  
  # Plot as Polygon
  lines(l.p[,1], l.p[,2], col=rgb(.47, 00, .47))
  polygon(c(l.p[,1], rev(l.p[,1])), c(l.p[,3], rev(l.p[,4])), col=rgb(.47, 00, .47, .50), border=F)
  
  # Exract Fitted Values and Confidence Intervals from OLS
  ols <- lm(daten05$cdu~daten05$kath)
  r.p <- predict(ols, interval="confidence")
  r.p <- cbind(daten05$kath, r.p)
  r.p <- r.p[order(r.p[,1]),]
  
  # Plot as Lines
  lines(r.p[,1], r.p[,2], col=rgb(.47, 00, .47))
  lines(r.p[,1], r.p[,3], col=rgb(.47, 00, .47), lty=2)
  lines(r.p[,1], r.p[,4], col=rgb(.47, 00, .47), lty=2)
  
  # Plot as Polygon
  lines(r.p[,1], r.p[,2], col=rgb(.47, 00, .47))
  polygon(c(r.p[,1], rev(r.p[,1])), c(r.p[,3], rev(r.p[,4])), col=rgb(.47, 00, .47, .50), border=F)
  
  
### Aspect Ratio
  
  par(mfrow=c(2,2))
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1, asp=2.7/1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  #points(daten05$kath, daten05$cdu, pch=19, col=rgb(0, 0, 0, 60, max=255))
  box(bty="l")
  abline(v=seq(.2, .8, by=.2), col="lightgrey", lty=1)
  abline(h=seq(.2, .5, by=.1), col="lightgrey", lty=1)
  
  lines(l.p[,1], l.p[,2], col=rgb(.47, 00, .47))
  polygon(c(l.p[,1], rev(l.p[,1])), c(l.p[,3], rev(l.p[,4])), col=rgb(.47, 00, .47, .50), border=F)
  
### Bubbleplot
  
  par(mfrow=c(1,1), mgp=c(2,.5,0), tck=-.02)
  plot(daten05$kath, daten05$cdu, pch="", axes=F, cex.lab=1, main="", xlab="Share of Catholics %", ylab="Vote Share CDU/CSU %", las=1)
  axis(2, at=seq(.10, .60, by=.10), labels=c("10", "20", "30", "40", "50", "60"), las=1, tck=-.01,)
  axis(1, at=seq(.20, .80, by=.20), labels=c("20", "40", "60", "80"), las=1,  tck=-.01,)
  box(bty="l")

  symbols(daten05$kath, daten05$cdu, circles=daten05$n, inches=0.5, 
          bg=rgb(120, 00, 120, 50, max=255), fg=rgb(120, 00, 120,  max=255), 
          add=T)
  
  symbols(daten05$kath, daten05$cdu, circles=sqrt(daten05$n/pi), inches=0.5, 
          bg=rgb(120, 00, 120, 50, max=255), fg=rgb(120, 00, 120,  max=255), 
          add=T)
  
  symbols(daten05$kath, daten05$cdu, circles=sqrt(daten05$n/pi), inches=0.3, 
          bg=rgb(120, 00, 120, 50, max=255), fg=rgb(120, 00, 120,  max=255), 
          add=T)

#######################################################################
 
# Merge Population Size of 2010 to FH data  
  pop10 <- read.dta("/Users/richardtraunmuller/Dropbox/pop10.dta")
  pop10 <- pop10[pop10$year==2010,]
  pop10$Country <- pop10$country
  
  data <- read.csv("FH_disaggregated_03_14.csv", sep=";")  
  data <- merge(data, pop10, by="Country")
  
  saveRDS(data, "new_FH_data.rds")
  
  
# Create Bubble Plot for 2014  
  
  par(mfrow=c(1,1), mgp=c(2,.5,0), tck=.0)
  plot(data$pr_2014, data$cl_2014, pch="", axes=F, cex.lab=1, main="", xlab="Political Rights", ylab="Civil Liberties", las=1)
  axis(1)
  axis(2, las=1)
  box(bty="l")
  
  symbols(data$pr_2014, data$cl_2014, circles=sqrt(data$pop/pi), inches=0.3, 
  bg=rgb(120, 00, 120, 50, max=255), fg=rgb(120, 00, 120,  max=255), add=T)
  
  text(data$pr_2014, data$cl_2014, data$Country, cex=sqrt((data$pop/1345174272*10)/pi), col=rgb(0, 0, 0, 100, max=255), pos=2)

  
# Create Bubble Trace Plot 
  brewer.pal(9, "YlGnBu")[4]
  
  col.pal <- colorRampPalette(c("#FFFFD9", "#7FCDBB", "#081D58"), bias=.5)
  
  par(mfrow=c(4,4), mgp=c(1.5,.3,0), tck=.0, fg="grey", mar=c(3,3,2,1), cex.main=.8, col.axis="grey", col.lab="grey")
  
  for(i in 1:16){
  plot(data$pr_2014, data$cl_2014, pch="", axes=F, cex.lab=1, main="", xlab="Political Rights", ylab="Civil Liberties", las=1, xlim=c(15, 40), ylim=c(0, 60), asp=1/1, xaxs="r", yaxs="r")
  axis(1)
  axis(2, las=1, at=seq(0, 60, by=20))
  box(bty="l")
  
  r.data <- data[sample(1:207, 1), ]
  pr <- r.data[2:13]
  cl <- r.data[14:25]
  
  lines(pr, cl, col="grey")
  symbols(pr, cl, circles=rep(sqrt(r.data$pop/pi),12), inches=0.03, bg=col.pal(12), fg=col.pal(12), add=T)
  title(r.data$Country)
  }
  
######################################################################################
  
  
  
  
  
  
  
  
  