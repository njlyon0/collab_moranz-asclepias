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
  ggplot(aes(fill = Site, label = Site)) +
  geom_sf() +
  # Add site names
  geom_sf_label(fill = "white", position = "jitter") +
  # geom_text(label = "PYW", x = -94.185, y = 40.5725) +
  # geom_text(label = "PYN", x = -94.175, y = 40.585) +
  # geom_text(label = "PYS", x = -94.167, y = 40.569) +
  # Modify axis tick labels
  scale_x_continuous(limits = c(-94.20, -94.10),
                     breaks = seq(from = -94.20, to = -94.10, by = 0.05)) +
  scale_y_continuous(limits = c(40.50, 40.65),
                     breaks = seq(from = 40.50, to = 40.65, by = 0.05)) +
  # Handle formatting bits of plot
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.title = element_blank())

# Examine
site_map

# Now make larger regional map
region_map <- states %>%
  ggplot() +
  geom_sf(fill = NA) +
  # Add in GRG boundary shape
  geom_sf(data = grg, aes(fill = Site)) +
  # Set limits
  coord_sf(xlim = c(-90, -96), ylim = c(38.75, 42.25), expand = F) +
  # Modify axis tick labels
  scale_x_continuous(limits = c(-96, -90),
                     breaks = seq(from = -96, to = -90, by = 3)) +
  scale_y_continuous(limits = c(38.75, 42.25),
                     breaks = seq(from = 39, to = 42, by = 3)) +
  # scale_y_continuous(limits = c(40.50, 40.65),
  #                    breaks = seq(from = 40.50, to = 40.65, by = 0.05)) +
  # Map formatting
  supportR::theme_lyon() +
  theme(legend.position = "none")

# Examine this too
region_map

# Combine graphs beginning with site map
cowplot::ggdraw(plot = site_map) +
  # And draw in the regional map too
  cowplot::draw_plot({ region_map },
                     # Starting point on left edge (x) and bottom edge (y)
                     x = 0.58, y = 0.6,
                     # Dimensions of inset plot expressed as proportion of entrire area
                     width = 0.46, height = 0.46)


# End ----
