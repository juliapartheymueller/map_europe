###############################################################
# Mapping data: European countries
###############################################################
library("maptools")
library("RColorBrewer")
library("sp")
library("classInt")

# Set directory
dir <- "../"

# Read shapes (Retrieved from: http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts)
europe.map<-readShapeSpatial(paste0(dir,"data/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013"))

# Select shapes by NUTS level
head(europe.map@data)
europe.map2 <- europe.map[europe.map$STAT_LEVL_ == 0,]
	
# Plot European countries with random colors
col = rainbow(length(levels(europe.map2$NUTS_ID)))
spplot(europe.map2, "NUTS_ID", col.regions=col,
	colorkey = FALSE, lwd=.4, col="white",
	main="European Countries",
	xlim = c(-13, 35), ylim = c(33, 72))

# Read some data to be plotted
some.dat <- read.csv(paste(dir,"data/Some_data.csv", sep=""), sep=";")

# Match external data by NUTS ID
europe.map2 <- SpatialPolygonsDataFrame(europe.map2, data= data.frame(europe.map2, some.dat[match(europe.map2$"NUTS_ID", some.dat$"NUTS_ID"),]))

# Select color scheme
pal = brewer.pal(7,"Blues")

# Create breaks
agg.some_data <- aggregate(europe.map2$some_data, by=list(europe.map2$NUTS_ID), FUN=mean)
summary(agg.some_data$x)
brks.qt = classIntervals(agg.some_data$x, n = 7, style = "quantile")
brks.jk = classIntervals(agg.some_data$x, n = 7, style = "jenks")
brks.eq = classIntervals(agg.some_data$x, n = 7, style = "equal")
brks.pr = classIntervals(agg.some_data$x, n = 7, style = "pretty")

# Plot
spplot(europe.map2, c("some_data"), names.attr =c("Some data"),
	at= brks.pr$brks,
	col=grey(.9), col.regions=pal,
	main="ADD TITLE",
	xlim = c(-13, 35), ylim = c(33, 72))

