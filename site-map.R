## ---------------------------------------------------------- ##
        # Moranz et al. Asclepias tuberosa Project
## ---------------------------------------------------------- ##
# Code written by Nick J Lyon

# PURPOSE ----
## Creates a map of the sites used in this project

## ------------------------------------------------ ##
# Housekeeping ----
## ------------------------------------------------ ##

# Call any needed libraries here (good to centralize this step)
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, sf, supportR)

# Clear environment
rm(list = ls())

# Identify colors (will use these in the map)
stock_colors <- c("None" = "#8073ac", "SLS" = "#fdb863", "IES" = "#b35806")

## ------------------------------------------------ ##
# Map Preparation ----
## ------------------------------------------------ ##

# Identify which site shapefiles are present
shps <- data.frame("x" = dir(path = file.path("site_shapefiles"))) %>%
  # Filter to only the primary .shp file
  dplyr::filter(stringr::str_sub(string = x,
                                 start = (nchar(x) - 3),
                                 end = nchar(x)) == ".shp")

# Make an empty object
sites <- NULL

# For each site shapefile
for(k in 1:nrow(shps)){
  
  # Identify the file name & site abbreviation
  file_name <- shps$x[k]
  site_abbrev <- stringr::str_sub(string = file_name, start = 1, end = 3)
  
  # Read in the file
  raw_shp <- sf::st_read(dsn = file.path("site_shapefiles", file_name)) %>%
    # And add a column for site name
    dplyr::mutate(Site = site_abbrev) %>%
    # Pare down to only this and geometry
    dplyr::select(Site, geometry)
  
  # Bind it onto the sites object
  sites %<>%
    dplyr::bind_rows(raw_shp) }

# Convert the sites coordinates into lat/long
sites_actual <- sites %>%
  sf::st_transform(x = ., crs = 4326)

# Make named vector of colors
site_colors <- c("GIL" = "#b35806", "LTR" = "#b35806", "PYN" = "#b35806", 
                 "PYW" = "#fdb863", "RCH" = "#fdb863", "PYS" = "#fdb863", "RIS" = "#fdb863",
                 "RIN" = "#8073ac",  "PAW" = "#8073ac")

# Exploratory plot
sites_actual %>%
  ggplot(aes(fill = Site)) +
  geom_sf() +
  scale_fill_manual(values = site_colors) +
  supportR::theme_lyon() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Exploratory plot
# plot(sites_actual, axes = T)



states <- sf::st_read(dsn = file.path("other_shapefiles", "cb_2015_us_state_500k.shp"))
plot(states)

# Load in site shapefiles
for(focal_site in unique(stringr::str_sub(string = shps, start = 1, end = 3))){
  
  # Read in file
  focal_shp <- sf::st_read()
  
  
}



















# End ----

# Necessary libraries
# library(rgdal); library(maps); library(mapdata); library(mapproj); library(devEMF)

# Set the working directory
# setwd("~/Documents/School/Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bfly.Project")

# Clear the environment
rm(list = ls())

##  ----------------------------------------------------------------------------------------------------------  ##
# Site GPS Points Prep ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Pulled GPS point for transect start from one patch of each pasture
## Tried to go with most 'central' patch for each pasture
## Patch and whittaker are listed in the first dataframe
utms <- read.csv("./Site Map/mapdata_raw.csv")

# Ditch the sites I no longer use
utms.v2 <- subset(utms, Pasture != "RC2" & Pasture != "DUN" & Pasture != "BSH" & Pasture != "STE")

# Get R to see the utm coords as a spatial object
utmcoor <- SpatialPoints(cbind(utms.v2$UTM.X, utms.v2$UTM.Y), proj4string = CRS("+proj=utm +zone=15"))
## utms$X and utms$Y are corresponding to UTM Easting and Northing, respectively
## zone = UTM zone

# Get latitude and longitude
longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat"))

# Get it into dataframe form again
longlat <- as.data.frame(longlatcoor)

# The shapefile is shifted slightly south
## SO need to ammend the points to be on the right side of the IA/MO border
longlat$coords.x2 <- longlat$coords.x2 + 0.03

# Check the bounding box it assigned
print(summary(longlatcoor))

# And manually set my box to be just a smidge bigger than that
y1 <- 40.4
y2 <- 40.8
x1 <- -94.3
x2 <- -93.9

# Add a column for pasture name and another for management
longlat$Site <- utms.v2$Pasture
longlat$Mgmt <- c("GB", "PBG", "PBG", "BO", "GB", "PBG", "BO", "GB", "BO", "PBG")

# Now this next bit is super manual and gross, but it doesn't have to be pretty
## Assign shapes and colors to sites of different treatments
shapes <- c(23, 22, 22, 21, 23,
            22, 21, 23, 21, 22)
## BO = 21 | PBG = 22 | GB = 23 | H+ = 24

colors <- c("#abd9e9", "#fdae61", "#fdae61", "#d73027", "#abd9e9",
            "#fdae61", "#d73027", "#abd9e9", "#d73027", "#fdae61")
## BO = "#d73027", # red | PBG = "#fdae61", # yellow |
## GB = "#abd9e9", # light blue

# In case it's needed again, here is the treatment order
# c(GB, PBG, PBG, BO, GB, PBG, BO, GB, GB, BO, PBG)

##  ----------------------------------------------------------------------------------------------------------  ##
# Map Creation with "maps" library ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
# Step-wise map creation
##  ----------------------------------------------------------  ##
# Get a map of the counties in which we sample and the state boundaries
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)

# Add the site points
points(longlat, pch = shapes, bg = colors, cex = 0.75)
#text(longlat, as.vector(longlat$Site), cex = 1)

# Maybe add county names?
#text(c(-94, 40.5), expression(italic("Ringgold Co.")))
#text(c(-94, 40.5), expression(italic("Harrison Co.")))

# Draw a box around the whole map you just created
box()

# Set the position of the map inset
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)

# And set the extent the inset will cover
inset.x <- c(-97, -89)
inset.y <- c(36, 45)
plot.window(xlim = inset.x, ylim = inset.y)

# Actually put bigger area in here
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()

# Add a box into the inset showing where the more zoomed-in one is from
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")

# And re-add state boundaries so the iowa/missouri border is clear in the inset
map("state", regions = c('iowa', 'missouri'), xlim = inset.x, ylim = inset.y,
    col = NA, fill = T, res = 0, add = T)

##  ----------------------------------------------------------  ##
# Plotting
##  ----------------------------------------------------------  ##
# NOTE
## For the plotting, because we have to call all that stuff again
## Let's just agree to do it without any comments (to increase efficiency)

# Stuff a low-res, but checkable, jpeg into the Graphs folder
jpeg(filename = "./Site Map/GRG_Map.jpeg", width = 6, height = 6, units = "in", res = 720)
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)
points(longlat, pch = shapes, bg = colors, cex = 1)
box()
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)
plot.window(xlim = inset.x, ylim = inset.y)
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")
dev.off()

# Get an enhanced metafile (EMF) for actual inclusion in any publication
emf(file = "./Site Map/GRG_Map.emf", bg = "white", width = 7, height = 7, family = "Calibri", coordDPI = 350)
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)
points(longlat, pch = shapes, bg = colors, cex = 2)
box()
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)
plot.window(xlim = inset.x, ylim = inset.y)
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")
dev.off()

# END ####