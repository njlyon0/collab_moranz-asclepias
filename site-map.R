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
librarian::shelf(tidyverse, magrittr, sf, supportR, maps, cowplot)

# Clear environment
rm(list = ls())

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

# Read in the GRG outline and convert it's CRS as well
grg <- sf::st_read(dsn = file.path("other_shapefiles", "GRG_Boundary.shp")) %>%
  ## Add "Site" column
  dplyr::mutate(Site = "GRG") %>%
  # Pare down to just this + geometry
  dplyr::select(Site, geometry) %>%
  # Convert to lat/long
  sf::st_transform(x = ., crs = 4326)

# Read in state data
states <- sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)) %>%
  # Drop ID column
  dplyr::select(-ID)

## ------------------------------------------------ ##
# Map Creation ----
## ------------------------------------------------ ##

# Make the site-level map first
site_map <- sites_actual %>%
  ggplot(aes(fill = Site)) +
  geom_sf() +
  # Handle formatting bits of plot
  supportR::theme_lyon() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left")

# Examine
site_map

# Now make larger regional map
region_map <- states %>%
  ggplot() +
  geom_sf(fill = NA) +
  geom_sf(data = grg, aes(fill = Site)) +
  coord_sf(xlim = c(-89, -97),
           ylim = c(37, 43),
           expand = F)+
  # Map formatting
  supportR::theme_lyon() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Examine this too
region_map

# Draw together!
cowplot::ggdraw(plot = site_map) +
  cowplot::draw_plot({ region_map },
                     x = 0.58, y = 0,
                     width = 0.46, height = 0.46)

# End ----
