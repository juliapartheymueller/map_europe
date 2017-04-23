###############################################################################################################
# Create a Map of Data for European Countries
###############################################################################################################

# Load required packages
library("maptools")
library("RColorBrewer")
library("sp")
library("classInt")

# Set directory
dir <- ".."

# Read some data to be plotted
some.dat <- read.csv(file.path(dir, "data", "Some_data.csv"), sep=";")

# Read shapes
## Retrieved from: Eurostat
## http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
europe.map<-readShapeSpatial(file.path(dir, "data", "NUTS_2013_01M_SH", "data", "NUTS_RG_01M_2013"))
europe.map2<-readShapeSpatial(file.path(dir, "data", "ne_10m_admin_0_countries", "ne_10m_admin_0_countries"))

# Select shapes by NUTS level / region
head(europe.map@data)
europe.map <- europe.map[europe.map$STAT_LEVL_ == 0,]
head(europe.map2@data)
europe.map2 <- europe.map2[europe.map2$CONTINENT == "Europe" | europe.map2$SOVEREIGNT =="Turkey",]
	
# Plot European countries with random colors
# col = rainbow(length(levels(europe.map$NUTS_ID)))
# p0 <- spplot(europe.map, "NUTS_ID", col.regions=col,
# 	colorkey = FALSE, lwd=.4, col="white",
# 	main="European Countries",
# 	xlim = c(-13, 35), ylim = c(33, 72))

# Match external data by key variable
europe.map <- SpatialPolygonsDataFrame(europe.map,
	data= data.frame(europe.map, some.dat[match(europe.map$"NUTS_ID", some.dat$"NUTS_ID"),]))
europe.map2 <- SpatialPolygonsDataFrame(europe.map2,
		data= data.frame(europe.map2, some.dat[match(europe.map2$"WB_A2", some.dat$"NUTS_ID"),]))	

# Number of intervals
num.int <- 7

# Select color scheme
pal = brewer.pal(num.int,"Blues")

# Create breaks
agg.some_data <- aggregate(europe.map2$some_data, by=list(europe.map2$NUTS_ID), FUN=mean)
summary(agg.some_data$x)
brks.qt = classIntervals(agg.some_data$x, n = num.int, style = "quantile")
brks.jk = classIntervals(agg.some_data$x, n = num.int, style = "jenks")
brks.eq = classIntervals(agg.some_data$x, n = num.int, style = "equal")
brks.pr = classIntervals(agg.some_data$x, n = num.int, style = "pretty")

# Plot
# p1 <- spplot(europe.map1, c("some_data"), names.attr =c("Some data"),
# 	at= brks.pr$brks,
# 	col=grey(.9), col.regions=pal,
# 	main="ADD TITLE",
# 	xlim = c(-13, 35), ylim = c(33, 72))
#
# # Save file
# png(file.path(dir, "graphs", "map_europe.png"), width=500, height=400)
# p1
# dev.off()

# Plot
p2 <- spplot(europe.map2, c("some_data"), names.attr =c("Some data"),
	at= brks.pr$brks,
	col=grey(.9), col.regions=pal,
	main="ADD TITLE",
	xlim = c(-12, 42), ylim = c(33, 72))
p2
	
# Save file
png(file.path(dir, "graphs", "map_europe2.png"), width=500, height=400)
p2
dev.off()




	

