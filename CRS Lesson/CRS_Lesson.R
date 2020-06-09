### Coordinate Reference System and Spatial Projection

library(rgdal)
library(ggplot2)
library(rgeos)
library(raster)
library(sf)

# turn off axis elements in ggplot for better visual comparison
newTheme <- list(theme(line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(), # turn off ticks
                       axis.title.x = element_blank(), # turn off titles
                       axis.title.y = element_blank(),
                       legend.position = "none")) # turn off legend

###GEOGRAPHIC CRS WGS84:units are degrees, coordinate system is lat/long, and the origin is where the equator meets the central meridian (0,0)

# read shapefile
worldBound <- readOGR(dsn = "CRS Lesson/data//ne_110m_land.shp")
# convert to dataframe
worldBound_df <- fortify(worldBound)

# plot map using ggplot
worldMap <- ggplot(worldBound_df, aes(long,lat, group = group)) +
  geom_polygon() +
  coord_equal() +
  labs(x = "Longitude (Degrees)",
       y = "Latitude (Degrees)",
       title = "Global Map - Geographic Coordinate System",
       subtitle = "WGS84 Datum, Units: Degrees - Latitude / Longitude")

worldMap

# define locations of Boulder, CO, Mallorca, Spain and  Oslo, Norway
# store coordinates in a data.frame
loc_df <- data.frame(lon = c(-105.2519, 10.7500, 2.9833),
                     lat = c(40.0274, 59.9500, 39.6167))

# add a point to the map
mapLocations <- worldMap +
  geom_point(data = loc_df,
             aes(x = lon, y = lat, group = NULL), colour = "springgreen",
             size = 5)

mapLocations

###PROJECTED CRS - ROBINSON: 

# reproject data from longlat to robinson CRS
worldBound_robin <- spTransform(worldBound,
                                CRS("+proj=robin"))

worldBound_df_robin <- fortify(worldBound_robin)

# force R to plot x and y values without rounding digits
# options(scipen=100)

robMap <- ggplot(worldBound_df_robin, aes(long,lat, group = group)) +
  geom_polygon() +
  labs(title = "World map (robinson)",
       x = "X Coordinates (meters)",
       y = "Y Coordinates (meters)") +
  coord_equal()

robMap

#Let's try to add the same three locations as before: Boulder, Mallorca, and Oslo

# add a point to the map
newMap <- robMap + geom_point(data = loc_df,
                              aes(x = lon, y = lat, group = NULL),
                              colour = "springgreen",
                              size = 5)

newMap #points are not in the correct location...first need to convert the points to the new projection - a process called REPROJECTION which uses the spTransform() function

#The points are stored in a data.frame (loc_df) which is not a spatial object. Thus, we need to convert that data.frame to a spatial data.frame to use spTransform().

# convert dataframe to spatial points data frame
loc_spdf <- SpatialPointsDataFrame(coords = loc_df, data = loc_df,
                                   proj4string = crs(worldBound))
loc_spdf

# reproject data to Robinson
loc_spdf_rob <- spTransform(loc_spdf, CRSobj = CRS("+proj=robin"))

#To make the transformed data plot nicely with ggplot, we need to once again convert back into a dataframe. This is done by extracting the coordinates() and turning that into a data.frame using as.data.frame()

# convert the spatial object into a data frame
loc_rob_df <- as.data.frame(coordinates(loc_spdf_rob))

# turn off scientific notation
options(scipen = 10000)

# add a point to the map
newMap <- robMap + geom_point(data = loc_rob_df,
                              aes(x = lon, y = lat, group = NULL),
                              colour = "springgreen",
                              size = 5)

newMap #now the points are projected to the correct place on this map

###COMPARING THE MAPS

#Both of the plots above look visually different and also use a different coordinate system. Let’s look at both, side by side, with the actual GRATICULES latitude and longitude lines rendered on the map.

#use a graticules layer that contains the meridian and parallel lines
## import graticule shapefile data
graticule <- readOGR("CRS Lesson/data",
                     layer = "ne_110m_graticules_15")
# convert spatial sp object into a ggplot ready, data.frame
graticule_df <- fortify(graticule)

# plot graticules
ggplot() +
  geom_path(data = graticule_df, aes(long, lat, group = group), linetype = "dashed", color = "grey70")

#import a bounding box to make plot look nicer
bbox <- readOGR("CRS Lesson/data/ne_110m_wgs84_bounding_box.shp")
bbox_df <- fortify(bbox)

latLongMap <- ggplot(bbox_df, aes(long,lat, group = group)) +
  geom_polygon(fill = "white") +
  geom_polygon(data = worldBound_df, aes(long,lat, group = group, fill = hole)) +
  geom_path(data = graticule_df, aes(long, lat, group = group), linetype = "dashed", color = "grey70") +
  coord_equal() +  labs(title = "World Map - Geographic (long/lat degrees)")  +
  newTheme +
  
  scale_fill_manual(values = c("black", "white"), guide = "none") # change colors & remove legend

# add your location points to the map
latLongMap <- latLongMap +
  geom_point(data = loc_df,
             aes(x = lon, y = lat, group = NULL),
             colour = "springgreen",
             size = 5)

#now, reproject graticules and bounding box to the Robinson projection

#reproject grat into robinson
graticule_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(graticule_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

# plot using robinson
finalRobMap <- ggplot(bbox_robin_df, aes(long, lat, group = group)) +
  geom_polygon(fill = "white") +
  geom_polygon(data = worldBound_df_robin, aes(long, lat, group = group, fill = hole)) +
  geom_path(data = grat_df_robin, aes(long, lat, group = group), linetype = "dashed", color = "grey70") +
  labs(title = "World Map Projected - Robinson (Meters)") +
  coord_equal() + newTheme +
  scale_fill_manual(values = c("black", "white"), guide = "none") # change colors & remove legend

# add a location layer in robinson as points to the map
finalRobMap <- finalRobMap + geom_point(data = loc_rob_df,
                                        aes(x = lon, y = lat, group = NULL),
                                        colour = "springgreen",
                                        size = 5)
#plot the two maps on top of each other to make them easier to compare. To do this, use the grid.arrange() function from the gridExtra package.
require(gridExtra)
# display side by side
grid.arrange(latLongMap, finalRobMap)

###You may be wondering, why bother with different CRSs if it makes your analysis more complicated? Well, each CRS is optimized to best represent the: shape and/or scale / distance and/or area of features in the data. And no one CRS is great at optimizing all three elements: shape, distance AND area. Some CRSs are optimized for shape, some are optimized for distance and some are optimized for area. Some CRSs are also optimized for particular regions - for instance the United States, or Europe. Discussing CRS as it optimizes shape, distance and area is beyond the scope of this tutorial, but it’s important to understand that the CRS that you chose for your data will impact working with the data.
