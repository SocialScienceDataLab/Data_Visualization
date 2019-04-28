#########################################################################
# Visualizing Statistical Models
##########################################################################

library(arm)
library(effects)

par(family="Gill Sans MT", lwd=.5, bty="l", mgp=c(1.5, .5, 0), mar=c(3,3,2,1), las=1, tck=-.02, cex.axis=.8, cex.lab=.8)


data(Arrests) # Load Data
data <- Arrests 

# Recode variables
data$released <- ifelse(data$released=="Yes", 1, 0) # Recode outcome variable
data$black <- ifelse(data$colour=="Black", 1, 0) # 
data$age.10 <- data$age/10 


###################################################################################
# Logistic Regression Model 

m <- glm(released ~  checks, data=data, family=binomial(link = "logit"))
display(m, digits=3)


# Plot Effect
par(mfrow=c(1,1), xaxs="r", yaxs="r", tck=-.02)
plot(jitter(data$checks, .5), jitter(data$released, .5), ylab="Pr(Released)", xlab="Number of Previous Records", cex=.5, pch=19, ylim=c(0, 1.1), col=rgb(0, 0, 0, 50, max=255))
abline(h=c(0, 1), lty=2, lwd=.5)
curve(invlogit(cbind(1, x)%*%coef(m)), col="maroon3", lwd=2, add=T)

# Add more covariates
m.2 <- glm(released ~  checks + black + employed + age.10, data=data, family=binomial(link = "logit"), x=T)
display(m.2, digits=3)

# Coefficient Plot
par(mar=c(3,5,3,2))
coefplot(m.2, main="Logit Coefficients for Released", col.main="black", cex.main=.8, xlim=c(-1, 1.5))


# Coefficient Plot per hand
coef.vec <- coef(m.2)   # Extract coefficient estimates
names.vec <- names(coef(m.2)) # Extract variable names
se.vec <- se.coef(m.2) # Extract standard errors

plot(coef.vec, length(coef.vec):1, pch=17, cex=.8, axes=F, ylab="", xlab="Logit coefficients", xlim=c(-1, 2))
  axis(1)
  axis(2, at=length(coef.vec):1, label=names.vec, col="white")
  abline(v=0, lty=2) # add zero reference line

segments(coef.vec - 1.96*se.vec,  length(coef.vec):1, coef.vec + 1.96*se.vec, length(coef.vec):1) # add confidence intervals


##################################################################################
# Predicted Probabilities
# Comparing Black vs. White (for employed=1 and average checks and average age)

pp.1 <- invlogit(cbind(1, mean(data$checks), 1, 1, mean(data$age.10)) %*% coef(m.2))
pp.2 <- invlogit(cbind(1, mean(data$checks), 0, 1, mean(data$age.)) %*% coef(m.2))
diff <- pp.1 - pp.2

# Plot
par(mfrow=c(1,2), mar=c(3, 3, 2, 0))
plot(c(1:2), c(pp.1, pp.2),  pch=c(19, 21), ylab="Predicted Probability", xlab="", xlim=c(.5, 2.5), ylim=c(0, 1), axes=F)
 axis(1, at=c(1:2), label=c("Black", "White"), col="white")
 axis(2)

par(mar=c(3, 0, 2, 12))
plot(1, diff,  pch=c(17), ann=F,  xlim=c(.9, 1.1), ylim=c(-.2, .2), axes=F, col="maroon3")
 rect(-2, -2, 2, 2, border=F, col="grey92")
   axis(1, at=1, label=c("Difference"), col.axis="Maroon3", col="white")
   axis(4) 
     abline(h=0, lty=2)
 points(1, diff,  pch=c(17), col="Maroon3")

# Let's add inferential uncertainty!
s <- 1000 # number of simulations
s.m.2 <- sim(m.2, s) # simulate the model s times

# Again, caculate predicted probabilities
s.pp.1 <- invlogit(cbind(1, mean(data$checks), 1, 1, mean(data$age.10)) %*% t(coef(s.m.2)))
s.pp.2 <- invlogit(cbind(1, mean(data$checks), 0, 1, mean(data$age.10)) %*% t(coef(s.m.2)))
s.diff <- s.pp.1 - s.pp.2


# Let's first look at the uncertainties in terms of 95 % confidence intervals
ci.1 <- quantile(s.pp.1, c(.025, .975))
ci.2 <- quantile(s.pp.2, c(.025, .975))
ci.diff <- quantile(s.diff, c(.025, .975))

# now let's add them to the plot
par(mfrow=c(1,2), mar=c(3, 3, 2, 0))
plot(c(1:2), c(pp.1, pp.2),  pch=c(19, 21), ylab="Predicted Probability", xlab="", xlim=c(.5, 2.5), ylim=c(.7, 1), axes=F)
  axis(1, at=c(1:2), label=c("Black", "White"), col="white")
  axis(2)

segments(c(1:2), c(ci.1[1], ci.2[1]), c(1:2), c(ci.1[2], ci.2[2])) # Add 95% Confidence Intervals 

par(mar=c(3, 0, 2, 12))
plot(1, diff,  pch=c(17), ann=F,  xlim=c(.9, 1.1), ylim=c(-.1, .05), axes=F, col="maroon3")
  rect(-2, -2, 2, 2, border=F, col="grey92")
    axis(1, at=1, label=c("Difference"), col.axis="Maroon3", col="white")
    axis(4) 
     abline(h=0, lty=2)
      points(1, diff,  pch=c(17), col="Maroon3")

segments(1, ci.diff[1], 1, ci.diff[2], col="Maroon3") # Add 95% Confidence Intervals


##################################################################################
# Interactions

m.3 <- glm(released ~  checks + black + employed + age.10 + black:age.10, data=data, family=binomial(link = "logit"), x=T)
display(m.3, digits=3)
coefplot(m.3)

# Plot 
par(mfrow=c(1,1), mar=c(3,3,2,3))
plot(data$age.10, data$released, pch="", ylab="Pr(Released)", xlab="Age/10", cex=.4,  ylim=c(.6, 1), xlim=c(0, 7))
curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(m.3)), col="maroon3", lwd=2, add=T)
curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(m.3)), col="darkolivegreen2", lwd=2, add=T)

text(0, .7, "Black", col="maroon3",  pos=4)
text(0, .95, "White", col="darkolivegreen2",  pos=4)


## Curves with Simulations for Inferential Uncertainty Added to the Plot
s <- 1000
s.m.3 <- sim(m.3, s)

for(i in 1:s){
  curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(s.m.3)[i,]), col=rgb(205, 41, 144, 30, max=255), add=T)
}

for(i in 1:s){
  curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(s.m.3)[i,]), col=rgb(188, 238, 104, 30, max=255), add=T)
}

rug(data$age.10, side=1, col="grey")
curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(m.3)), add=T, col="white", lwd=2)
curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(m.3)), add=T, col="white", lwd=2)



###################################################################################
# Alternative Visualization for Confidence Intervals

x <- seq(1, 7, by=.1) # Vary age/10 from 1 to 7 in steps of .1

mean.vec.1 <- matrix(NA, length(x), 1) # set up empty containers
CI.mat.1 <- matrix(NA, length(x), 2)

for(i in 1:length(x)){
  mean.vec.1[i,] <- mean(invlogit(c(1, mean(data$checks), 1, 1, x[i], 1*x[i]) %*% t(coef(s.m.3))))
  CI.mat.1[i,] <- quantile(invlogit(c(1, mean(data$checks), 1, 1, x[i], 1*x[i]) %*% t(coef(s.m.3))), c(.025, .975))
}

mean.vec.2 <- matrix(NA, length(x), 1)
CI.mat.2 <- matrix(NA, length(x), 2)

for(i in 1:length(x)){
  mean.vec.2[i,] <- mean(invlogit(c(1, mean(data$checks), 0, 1, x[i], 0*x[i]) %*% t(coef(s.m.3))))
  CI.mat.2[i,] <- quantile(invlogit(c(1, mean(data$checks), 0, 1, x[i], 0*x[i]) %*% t(coef(s.m.3))), c(.05, .95))
}

plot(c(1:length(x)), CI.mat.1[,1], ylim=c(.7, 1), type="l", col="maroon3", xlab="Age", ylab="Pr(Released)", axes=F)
  axis(1, at=c(1, 11, 21, 31, 41, 51, 61), label=c(10, 20, 30, 40, 50, 60, 70))
  axis(2, col="white")   
    lines(c(1:length(x)), CI.mat.1[,2], col="maroon3")
     polygon(c(1:length(x), length(x):1), c(CI.mat.1[,2], rev(CI.mat.1[,1])), col=rgb(205, 41, 144, 50, max=255), border=F) 
      lines(c(1:length(x)), mean.vec.1, col="maroon3")


    lines(c(1:length(x)), CI.mat.2[,1], col="darkolivegreen2")
     lines(c(1:length(x)), CI.mat.2[,2], col="darkolivegreen2")
      polygon(c(1:length(x), length(x):1), c(CI.mat.2[,2], rev(CI.mat.2[,1])), col=rgb(188, 238, 104, 100, max=255), border=F)
       lines(c(1:length(x)), mean.vec.2, col="darkolivegreen2")


axis(4, at=mean.vec.1[61], label="Black", col.axis="maroon3", col.tick="white", cex.axis=.8)
axis(4, at=mean.vec.2[61], label="White", col.axis="darkolivegreen2", col.tick="white", cex.axis=.8)

###################################################################################
# Exploratory Model Analysis

# Load package
library(meifly) 

data <- data[,-c(2, 4)]

############################################################################
# Fit all 2^k-1 model combinations 
# ATTENTION: This can easily become BIG DATA!!
all.models <- fitall(data$released, data[,2:8])

############################################################################
# Extract coefficients
# Load my function to extract coefficients (for some reason the function in the meifly-package does not work!)
load("extract_fun.R")

coefs <- coef_summary(all.models)

# change to wide format
coefs.est <- dcast(coefs, model ~ variable, value.var="Est")
coefs.se <- dcast(coefs, model ~ variable, value.var="SE")

# set NAs to 0
coefs.est[,2:8] <- apply(coefs.est[,2:8], 2, function(x){ifelse(is.na(x)==T,  0, x)})
coefs.se[,2:8] <- apply(coefs.se[,2:8], 2, function(x){ifelse(is.na(x)==T,  0, x)})

c.names <- paste("SE", colnames(coefs.se[,2:8]), sep="")
colnames(coefs.se[,2:8]) <- c.names

# Extract model fit
mods <- summary(all.models)

# Merge all together
DATA <- merge(mods, coefs.est, by="model")
DATA <- merge(DATA, coefs.se, by="model")

# Now you have a data set of model quantities - you know what to do!
##########################################################################
