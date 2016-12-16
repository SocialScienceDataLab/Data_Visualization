###############################################################################
### Pop Visualization
###############################################################################
library(plotrix)
library(foreign)

data <- read.dta("Elites/elites_data.dta", convert.factors=T)
data$female <- ifelse(data$E1=="2. weiblich", 1, 0)  # Gender
data$women <- data$C1_5

par(bg=rgb(54, 57, 74, 255, max=255), fg="white", xaxs="r", yaxs="i", mgp=c(2.8,0.3,00), col.lab="white", col.axis="white", col.main="white", font.main=1, cex.main=0.8, cex.axis=0.8, cex.lab=0.8, family="Helvetica Light", tck=0, las=1)

################################################################################################
# Donut Chart
par(mar=c(2,2,2,2))
my.colors <- c("orange"," maroon3", "darkolivegreen2", "lightskyblue2", "yellow2")
pie(table(data$women), col=my.colors, border=F, main="Silly Donut Chart", cex.main=2)
points(0,0, col=rgb(54, 57, 74, 255, max=255), cex=18, pch=19)

#################################################################################################
# Symbolbarplot
my.tab <- table(data$female, data$women)
symbolbarplot(my.tab, col=c("maroon3", "darkolivegreen2"), beside=T, symbbox=F, main="Symbol Bar Chart", cex.main=2)

#################################################################################################
# Unit Chart
par(family="Symbol Signs Basis set", mfrow=c(1,1), mar=c(1,1,1,1), oma=c(1,1,1,1), mfrow=c(1,2))

perc1 <- 24
perc2 <- 68

s.row <- sort(rep(seq(.1, 1, by=.1), 10), decreasing=T) 
s.col <- rep(seq(.1, 1, by=.1), 10)
s.mat <- cbind(s.row, s.col)
plot(s.mat[1:100,1], s.mat[1:100,2], pch="M", col=brewer.pal(5, "Blues")[1], cex=2.5, xlim=c(0,1), ylim=c(0,1), axes=F, ann=F)
points(s.mat[1:perc1, 2], s.mat[1:perc1, 1], pch="M", col=brewer.pal(5, "Blues")[3], cex=2.5)
mtext(side=1, line=3, paste(perc1, "%", sep=""), col=brewer.pal(5, "Blues")[3], cex=7, family="Helvetica Light")
mtext(side=3, line=3, "Unit Chart", col="white", cex=2, family="Helvetica Light", adj=0)

plot(s.mat[1:100,1], s.mat[1:100,2], pch="F", col=brewer.pal(5, "PuRd")[1], cex=2.5, xlim=c(0,1), ylim=c(0,1), axes=F, ann=F)
points(s.mat[1:perc2, 2], s.mat[1:perc2, 1], pch="F", col=brewer.pal(5, "PuRd")[3], cex=2.5)
mtext(side=1, line=3, paste(perc2, "%", sep=""), col=brewer.pal(5, "PuRd")[3], cex=7, family="Helvetica Light")

##################################################################################
# Stacked Line Chart

data <- readRDS("new_FH_data.rds")
for(i in 2:25){
  data[,i] <- as.character(data[,i])  
  data[,i] <- as.numeric(data[,i])  
}
data <- data[sample(1:197, 20),2:13]


par(las=1, tck=-.005, mar=c(2,2,2,2), oma=c(0,1,0,1), family="Helvetica Light", mfrow=c(1,2))
stackpoly(as.matrix(data), stack=T, xlab="Country", ann=F)
mtext(side=3, line=3, "Stacked Line Chart", cex=2, adj=0)
stackpoly(t(as.matrix(data)), stack=T,  xlab="", ylab="", main="")

#################################################################################### 













