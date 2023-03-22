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

# Assemble a vector of colors for sites
site_colors <- c("GIL" = "#b35806", "LTR" = "#b35806", "PYN" = "#b35806", 
                 "PYS" = "#fdb863", "PYW" = "#fdb863", "RCH" = "#fdb863", "RIS" = "#fdb863",
                 "PAW" = "#8073ac", "RIN" = "#8073ac")

# Bind text nudge y / x to site names
site_nudge <- data.frame("Site" = unique(sites_actual$Site),
                          ## Site order: "GIL" "LTR" "PAW" "PYN" "PYS" 
                          ## "PYW" "RCH" "RIN" "RIS"
                         "x" = c(0, 0, 0, 0.002, 0.003,
                                 -0.008, 0, 0, -0.008),
                         "y" = c(0.006, -0.005, 0.005, 0.004, -0.004,
                                 0, 0.006, 0.005, 0))

# Make the site-level map first
site_map <- sites_actual %>%
  ggplot(aes(fill = Site, label = Site)) +
  geom_sf() +
  # Add site names (can ignore warning about possibly non-exact text placement)
  geom_sf_text(label = site_nudge$Site, nudge_x = site_nudge$x, nudge_y = site_nudge$y) +
  # Color site shapes by their stocking treatment
  scale_fill_manual(values = site_colors) +
  # Modify axis tick labels
  scale_x_continuous(limits = c(-94.2, -94.05),
                     breaks = seq(from = -94.2, to = -94.1, by = 0.1)) +
  scale_y_continuous(limits = c(40.5, 40.65),
                     breaks = seq(from = 40.5, to = 40.65, by = 0.05)) +
  # Handle formatting bits of plot
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.title = element_blank()); site_map

# Now make larger regional map
region_map <- states %>%
  ggplot() +
  geom_sf(fill = NA) +
  # Add in GRG boundary shape
  geom_sf(data = grg, aes(fill = Site)) +
  # Add text labels
  geom_text(label = "Grand River Grasslands", x = -93.75, y = 40.85, 
            size = 3, fontface = "italic") +
  geom_text(label = "Iowa", x = -93.5, y = 41.9, 
            size = 5, fontface = "bold") +
  geom_text(label = "Missouri", x = -93, y = 39.5, 
            size = 5, fontface = "bold") +
  # Define color of GRG boundary
  scale_fill_manual(values = "#41b6c4") +
  # Set limits
  coord_sf(xlim = c(-90, -96), ylim = c(38.8, 42.25), expand = F) +
  # Modify axis tick labels
  scale_x_continuous(limits = c(-96, -90),
                     breaks = seq(from = -95, to = -91, by = 3)) +
  scale_y_continuous(limits = c(38.75, 42.25),
                     breaks = seq(from = 39, to = 42, by = 3)) +
  # Map formatting (plus border outline for inset as a whole)
  supportR::theme_lyon() +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white")); region_map

# Combine graphs beginning with site map
cowplot::ggdraw(plot = site_map) +
  # And draw in the regional map too
  cowplot::draw_plot({ region_map },
                     # Starting point on left edge (x) and bottom edge (y)
                     x = 0.515, y = 0.025,
                     # Dimensions of inset plot expressed as proportion of entrire area
                     width = 0.46, height = 0.46)

# Create the export folder (if it doesn't already exist)
dir.create("figures", showWarnings = F)

# Export!
ggsave(filename = file.path("figures", "Asclepias_Fig1.png"),
       plot = last_plot(), width = 6, height = 6, unit = "in")

# End ----
