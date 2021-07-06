#ModernMapEurasia.r
#Kathryn Turner, June 17, 2013
#With extensive help from Andy South
# https://alienplantation.wordpress.com/2013/06/18/nuts-and-bolts-modern-maps-of-eurasia-in-r/
#to create a reprojected map of the Northern Hemisphere
#color code countries for range
#and add collection sites
#in a Lambert azimuthal equal area projection, with the North Pole in the center 
rm(list=ls())
library(rgdal) # Commands for reprojecting the vector data.
library(rworldmap) # Recently updated mapping program.
library(rworldxtra) # Add-ons for rworldmap.
meta <- read.csv('data/siteMeta.csv')
meta$NatNon <- as.character(meta$NatNon)
meta$NatNon[meta$NatNon=="Introduced"] <- "Non-native"

###set output
pdf("output/Locations.rworldmap.code.pdf", useDingbats=FALSE)

#Lambert azimuthal equal area projection
#from http://gis.stackexchange.com/questions/30054/defining-datum-for-lambert-azimuthal-equal-area-and-converting-to-geographic-coo

###the view of the map is centered on the North pole, but slightly offset
#using the exact location causes an error
projectionCRS <- sp::CRS("+proj=laea +lon_0=0.001 +lat_0=89.999")# +ellps=sphere") 
#the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0))
#To determines the width of the margins for the map; the "i" designations make the map go to the edge of the plot window.
#par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
#including Antarctica causes an error, so I hope you don't need that
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
mapCountryData(sPDF, nameColumnToPlot="colCode", mapTitle='', 
               colourPalette=colourPalette, borderCol ='gray24', addLegend = FALSE,
               xlim=xlim, ylim=ylim, catMethod=c(0,1,2,3,4))
dev.off()
#sPDF <- spTransform(sPDF, CRS=projectionCRS)

###use setLims to reproject xlim and ylim, so that we can zoom in on the map
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS###
  #how this zooming works in non-intuitive, I suggest trial and error to find your numbers
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  #set CRS (coordinate reference system) for the points
  #assuming WGS84
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}

###to color code countries
#to view a list of country name formats
sPDF$ADMIN
#setup a color code column filled with 1's
sPDF$colCode <- 1
#set codes for specified countries
sPDF$colCode[ which(sPDF$ADMIN %in% c("Canada","United States of America"))] <- 2
sPDF$colCode[ which(sPDF$ADMIN %in% c("Japan"))] <- 3 
#                                      "Greece", "Moldova", "Romania","Russia", "Turkey",
#                                      "Ukraine", "Serbia"))] <- 3
sPDF$colCode[ which(sPDF$ADMIN %in% c("Italy", "France","Germany","Norway","Sweden","Denmark","Spain","Portugal","Morocco"))] <- 2

#create a color palette - note for each value not for each country
#colourPalette <- c("lightgray","#F8766D","#00BFC4", "cadetblue1")
colourPalette <- rep("lightgray",4)
#colourPalette can be set to either a vector of colors or one of :white2Black black2White palette heat topo terrain rainbow negpos8 negpos9


mapCountryData(sPDF, nameColumnToPlot="colCode", mapTitle='', 
               colourPalette=colourPalette, borderCol ='gray24', addLegend = FALSE,
               xlim=xlim, ylim=ylim, catMethod=c(0,1,2,3,4))
#note that catMethod defines the breaks and values go in a category if they are <= upper end
#I specified 4 different colors, so catMethod = c(0,1,2,3,4)

###to plot collection locations from GPS coordinates

# range lines
#y <- read.csv('range.nat.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="black")
#y <- read.csv('range.nat2.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="black")
#y <- read.csv('range.wNA.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="red")
#y <- read.csv('range.eNA.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="red")
#y <- read.csv('range.eur.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="red")
#y <- read.csv('range.eur2.csv'); points(x=y$x,y=y$y,type="l",lwd=6,col="red")


#coordinates must be projected to show up on map properly
#invasive populations
popInv <- meta[meta$Continent=="Japan",]
coordinates(popInv) = c("long", "lat")
proj4string(popInv) <- sp::CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(popInv, CRS=projectionCRS)
#points(sPointsDF, pch=20, cex=2)
points(sPointsDF, pch=20, cex=1.5,col="black",lwd=2)
#native populations
popNat <- samps[!samps$Country=="Japan",]
coordinates(popNat) = c("Longitude", "Latitude")
proj4string(popNat) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFNat <- spTransform(popNat, CRS=projectionCRS)
points(sPointsDFNat, pch=20, cex=1.5,col="red",lwd=2)
#points(sPointsDFNat, pch=20, cex=2,col="black")

###Adding things to map
#to plot latitude lines on to map
llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) 
#ndiscr=number points in lines, more makes a smoother curved line

#to plot latitude and longitude labels on the map, again, coordinates must be projected
markings <- data.frame(Latitude=as.numeric(c(75,60,45,30,15,85,85)), Longitude=as.numeric(c(-45,-45,-45,-45,-45,0,180)),name=c("75", "60","45","30","15","0","180"))
coordinates(markings) = c("Longitude", "Latitude")
proj4string(markings) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFmark <- spTransform(markings, CRS=projectionCRS)
text(sPointsDFmark, labels = sPointsDFmark$name, cex=1)

#to plot the pole
# pole <- data.frame(x=0, y=90)
# coordinates(pole) = c("x", "y")
# proj4string(pole) <- CRS("+proj=longlat +ellps=WGS84")
# pole <- spTransform(pole, CRS=projectionCRS)
# points(pole, pch=8, cex=2, lwd=2)

#legends for point type and color code
legend("bottomleft", c("Introduced", "Native"), fill=c("red", "black"),
       title="Origin", bg="white",cex=2)

#shameless plug !
#mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6)

box()
dev.off()
