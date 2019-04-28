##############################################################################################
# Visualizing Spatial Data
##############################################################################################

setwd("/Users/richardtraunmuller/Dropbox/Teaching/Essex 2016")

###########################################################################################
# Load Packages 
library(foreign)
library(RColorBrewer)

# Some packages for making maps
library(maps) # basic mapping
library(mapdata) # Supplement to maps package, providing the larger and/or higher-resolution databases

library(maptools) # provides functions to easily load and manage shapefiles
library(mapproj) # for map projections
library(rgdal) # for map projections

###########################################################################################
# The Maps Package
map()

# Zoom in 
map(database="worldHires", fill=T, border="grey92", lwd=1, col="purple4", xlim=c(-10, 30), ylim=c(30, 70))


map(database="worldHires", fill=T, border="grey92", lwd=1, col="purple4", xlim=c(-10, 30), ylim=c(30, 70))
axis(1)
axis(2)
grid(lty=2)
abline(v="0", lty=2, col="red")

# Draw Dots on a Map
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(30, 75), mar=c(1,1,1,1))
map(database="worldHires", region="UK", fill=T, col="purple4", border=F, add=T)

data(world.cities)
COL <- world.cities[world.cities$name=="Colchester" & world.cities$country.etc=="UK",]

# Put Colchester on the Map
map(database="worldHires", fill=F, col="purple4", xlim=c(-10, 5), ylim=c(50, 60))
map(database="worldHires", region="UK", fill=T, col="purple4", border=F, add=T)

points(COL[1,"long"], COL[1,"lat"],  col="maroon3", pch=19)
points(COL[1,"long"], COL[1,"lat"],  col="maroon3", pch=21, cex=3)
points(COL[1,"long"], COL[1,"lat"],  col="maroon3", pch=21, cex=6)
points(COL[1,"long"], COL[1,"lat"],  col="maroon3", pch=21, cex=9)
text(COL[1,"long"], COL[1,"lat"], COL[1,"name"], col="maroon3", cex=.7, pos=3)

# Map All Cities
map(database="worldHires", fill=F, col="purple4", xlim=c(-10, 10), ylim=c(50, 60))
map(database="worldHires", region="UK", fill=T, col="purple4", border=F, add=T)
UK <- world.cities[world.cities$country.etc=="UK",]
points(UK[,"long"], UK[,"lat"], pch=19, cex=.6, col="orange") 

# Bubbles of Population Size
map(database="worldHires", fill=F, col="purple4", xlim=c(-10, 5), ylim=c(50, 60))
map(database="worldHires", region="UK", fill=T, col="purple4", border=F, add=T)
UK <- world.cities[world.cities$country.etc=="UK",]
symbols(UK[,"long"], UK[,"lat"], sqrt(UK[,"pop"])/pi, inch=.3, add=T, bg=rgb(255, 165, 0, 80, max=255), fg=rgb(255, 165, 0, 80, max=255)) 

##########################################################################
# Draw Lines
# Travel to Colchester
travel <- rbind(
  world.cities[world.cities$name=="Colchester",][2,],
  world.cities[world.cities$name=="Hamburg",],
  world.cities[world.cities$name=="Oslo",],
  world.cities[world.cities$name=="Exeter",][2,],
  world.cities[world.cities$name=="Frankfurt",][1,]
)

par(bg="white")
map(database="worldHires", fill=F, col="purple4", xlim=c(-7, 20), ylim=c(45, 65))

points(travel[,c("long", "lat")], col="maroon3", cex=.6, pch=19)
text(travel[,c("long", "lat")], travel[,c("name")], col="orange", cex=.8, pos=4)
for(i in 2:5){
  lg <- c(travel[1, "long"],  travel[i, "long"])
  la <- c(travel[1, "lat"],  travel[i, "lat"])
  lines(lg, la, col="maroon3", lwd=2)
}


############################################################################
# Choropleth Maps

# Load Data
agg.data <- read.csv("swiss_agg_data.csv", sep=",")
agg.data$Canton <- agg.data$c

# Load Shapefile
swisshape <- readShapePoly("/Users/richardtraunmuller/Dropbox/Teaching/Essex 2016/Shapefiles/CHE_adm1.shp", proj4string=CRS("+proj=longlat"))

# Create ID in shapefile
swisshape$Canton <- swisshape$NAME_1
levels(swisshape$Canton) <- c("AG", "AR", "AI", "BL", "BS", "BE", "FR", "GE", "GL", "GR", "JU", NA, "LU", "NE", "NW", "OW", "SG", "SH", "SZ", "SO", "TG", "TI", "UR", "VS", "VD", "ZG", "ZH")

# Merge data to shapefile
swiss <- merge(swisshape, agg.data[,], by="Canton")
swiss <- swiss[-12,]

# Define cut points
c.mus <- as.factor(as.numeric(cut(swiss$agg_muslim, c(0, 2,  4,  6, 8, 10))))
levels(c.mus) <- c("0-2", "2-4", "4-6", "6-8", "8-10")
swiss$c.mus <- c.mus

# Choose color palette
my.palette <- brewer.pal(5, "Blues")

# Plot
par(mar=c(2,2,2,2))
plot(swisshape, col=my.palette[c.mus], border="grey92", lwd=.3, xlim=c(6, 10.5)) 
legend(10, 48, rev(levels(c.mus)), fill=rev(my.palette), border=F, bty="n", cex=.8)
mtext("Muslim Population Share %", side=3, line=-3)

###########################################################################
# Arrange and Combine
par(mfrow=c(2,2))
plot(swisshape, col=my.palette[c.mus], border="grey92", lwd=.3, xlim=c(6, 10.5)) 
mtext("Muslim Population Share %", side=3, line=1)

ord <- order(swiss$agg_muslim)
plot(swiss$agg_muslim[ord], col=my.palette[swiss$c.mus][ord], axes=F, pch=19)
axis(4, las=1, col="white")
axis(1, at=c(1:26), label=swiss$Canton[ord], col="white", cex.axis=.7)


my.color <- colorRampPalette(c("#4DAC26", "#F7F7F7", "#D01C8B"))
plot(swisshape, col=my.color(52)[round(swiss$mus,2)*100], border="grey92", lwd=.3, xlim=c(6, 10.5)) 
mtext("Too many Muslims in Switzerland %", side=3, line=1)

#ord <- order(swiss$mus)
plot(swiss$mus[ord], col=my.color(52)[round(swiss$mus,2)*100][ord], axes=F, pch=19)
axis(4, las=1, col="white")
axis(1, at=c(1:26), label=swiss$Canton[ord], col="white", cex.axis=.7)



par(mar=c(3, 3, 2, 2), las=1)
plot(swiss$agg_muslim, swiss$mus, pch="", axes=F, xlab="Share of Muslims", ylab="Too many Muslims")
axis(1)
axis(2)
text(swiss$agg_muslim, swiss$mus, swiss$Canton)
lines(loess.smooth(swiss$agg_muslim, swiss$mus), col="purple4")
box(bty="l")


image(matrix(c(0:52)), col=my.color(52), axes=F, ylim=c(0, .5))
my.scale <- function(x, d){(x-1)/(d-1)} 
axis(1, at=my.scale(seq(0,50, by=10), 52), label=c("0", "10", "20", "30", "40", "50"))


###########################################################################
# Check out some projections
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(10, 75), mar=c(1,1,1,1))

# Mercator
par(mfrow=c(2,2), oma=c(0, 0, 0, 0))
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(10, 75), proj="mercator")
map.grid(lty=1, col="darkgrey", font=1)

# Mollweide
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(10, 75), proj="mollweide")
map.grid(lty=1, col="darkgrey", font=1)

# Gilbert
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(10, 75), proj="gilbert")
map.grid(lty=1, col="darkgrey", font=1)

# Bonne
map(database="worldHires", fill=F, col="purple4", xlim=c(-25, 35), ylim=c(10, 75), proj="bonne", par=c(0))
map.grid(lty=1, col="darkgrey", font=1)


# Apply mercator projection to swiss shapefile
plot(swisshape, col="black", border="white")

swisshape.merc <- spTransform(swisshape, CRS=CRS("+proj=merc")) 
plot(swisshape.merc, col="black", border="white")

###########################################################################



my.color <- ifelse(swiss)
  
  
plot(swiss, col="grey", border="white", lwd=.3, xlim=c(6, 10.5)) 
mtext("Too many Muslims in Switzerland %", side=3, line=1)

#ord <- order(swiss$mus)
plot(swiss$mus[ord], col=my.color(52)[round(swiss$mus,2)*100][ord], axes=F, pch=19)
axis(4, las=1, col="white")
axis(1, at=c(1:26), label=swiss$Canton[ord], col="white", cex.axis=.7)





