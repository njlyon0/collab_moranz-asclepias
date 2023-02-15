## ---------------------------------------------------------- ##
          # Moranz et al. Asclepias tuberosa Project
## ---------------------------------------------------------- ##
# Code written by Nick J Lyon

# PURPOSE ----
## Creates all needed publication-quality figures for this project

## ------------------------------------------------ ##
                  # Housekeeping ----
## ------------------------------------------------ ##

# Call any needed libraries here (good to centralize this step)
# install.packages("librarian")
librarian::shelf(tidyverse, supportR, psych, cowplot)

# Clear environment
rm(list = ls())

# Read in data
mkwd <- read.csv(file = file.path("tidy_data", "Asclepias-TIDY.csv")) %>%
  # Change some columns into characters to start
  dplyr::mutate(dplyr::across(.cols = c(Site:Management.Method,
                                        Stocking.Type:GrazingLawn),
                              .fns = as.factor)) %>%
  # Filter out TSF > 2
  dplyr::filter(TSF <= 2)

# Glimpse it
dplyr::glimpse(mkwd)

# Create a folder to export to
dir.create("figures", showWarnings = F)

## ------------------------------------------------ ##
          # Template ggplot Functions ----
## ------------------------------------------------ ##

# Vector of colors for stocking types
stock_colors <- c("None" = "#8073ac", "SLS" = "#fdb863", "IES" = "#b35806")

# Create the ggplot architecture for the TSF figure
tsf_fig_skeleton <- function(df, ylab){
  ggplot(data = df, aes(x = TSF, y = mean, fill = "a")) +
    geom_errorbar(aes(ymin = mean - std_error, ymax = mean + std_error),
                  width = 0.25) +
    geom_point(size = 3, pch = 23) + 
    labs(x = "Time Since Fire (years)", y = ylab) +
    scale_x_continuous(breaks = c(0, 1, 2)) +
    scale_fill_manual(values = "gray45") +
    scale_color_manual(values = 'black') +
    theme(legend.position = "none") +
    supportR::theme_lyon() }

# Create ggplot template for stocking type figure
grz_fig_skeleton <- function(df, ylab){
ggplot(data = df, aes(x = Stocking.Type, y = mean,
                     fill = Stocking.Type, color = "x")) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean - std_error, ymax = mean + std_error),
                width = 0.25) + 
  scale_fill_manual(values = stock_colors) +
  labs(x = "Stocking Type", y = ylab) +
  scale_color_manual(values = 'black') +
  theme(legend.position = "none") +
  supportR::theme_lyon() }

# Interaction figure skeleton
ixn_fig_skeleton <- function(df, ylab){
  ggplot(data = df, aes(x = TSF, y = mean, shape = Stocking.Type,
                       fill = Stocking.Type)) +
    geom_smooth(aes(color = Stocking.Type), method = "lm",
                formula = "y ~ x", se = F) +
    geom_errorbar(aes(ymin = mean - std_error, ymax = mean + std_error),
                  width = 0.25, position = position_dodge(width = 0.5)) +     
    geom_point(stat = 'identity', size = 3,
               position = position_dodge(width = 0.5)) +
    labs(x = "Time Since Fire (years)", y = ylab) +
    scale_x_continuous(breaks = c(0, 1, 2)) +
    scale_fill_manual(values = stock_colors) +
    scale_color_manual(values = stock_colors) +
    scale_shape_manual(values = 21:23) +
    supportR::theme_lyon() }

## ------------------------------------------------ ##
              # Fig2 - # Bitten Stems ----
## ------------------------------------------------ ##

# Results:
## Time Since Fire (TSF) doesn't affect number bitten stems
## Stocking type does

# Define y limits shared by both plots
tot_bitten_ylim <- ylim(0, 2.5)

# Create TSF plot
tot_bitten_tsf <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("TSF"),
                response = "Tot.Bitten.Stems", drop_na = T) %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Bitten Stems") +
  geom_text(label = "NS", x = 2, y = 2.5, size = 6) +
  tot_bitten_ylim

# Look at it
tot_bitten_tsf

# Now make the stocking rate one
tot_bitten_mgmt <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Tot.Bitten.Stems") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Bitten Stems") +
  geom_text(label = "a", x = 0.7, y = 0.6, size = 6) +
  geom_text(label = "b", x = 1.7, y = 1.3, size = 6) +
  geom_text(label = "c", x = 2.7, y = 2.2, size = 6) +
  geom_text(label = "p = 0.012", x = 3, y = 2.5, size = 6) +
  tot_bitten_ylim +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
  
# Check it
tot_bitten_mgmt

# Combine the plots!
cowplot::plot_grid(tot_bitten_tsf, tot_bitten_mgmt,
                   nrow = 1, ncol = 2)

# Export
ggsave(filename = file.path("figures", "Asclepias_Fig2.png"),
       plot = last_plot(), width = 6.5, height = 4, unit = "in")

## ------------------------------------------------ ##
          # Fig3 - # Flowering Stems ----
## ------------------------------------------------ ##

# Results
## TSF affects flowering stems while stocking doesn't

# Define share y limits
flr_stem_ylim <- ylim(0, 4.5)

# Create TSF plot
flr_stem_tsf <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("TSF"),
                       response = "Num.Stems.ALL.Flowering.Stages") %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Reproductive Stems") +
  geom_smooth(aes(color = "b"), formula = "y ~ x", 
              se = F, method = 'lm') +
  geom_text(label = "p = 0.033", x = 1.75, y = 4.5, size = 6) +
  flr_stem_ylim

# Look at it
flr_stem_tsf

# Now make the stocking rate one
flr_stem_mgmt <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Num.Stems.ALL.Flowering.Stages") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Reproductive Stems") +
  geom_text(label = "NS", x = 3.2, y = 4.5, size = 6) +
  flr_stem_ylim +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Check it
flr_stem_mgmt

# Combine the plots!
cowplot::plot_grid(flr_stem_tsf, flr_stem_mgmt,
                   nrow = 1, ncol = 2)

# Export
ggsave(filename = file.path("figures", "Asclepias_Fig3.png"),
       plot = last_plot(), width = 6.5, height = 4, unit = "in")

## ------------------------------------------------ ##
    # Fig4 - Ratio of Flowering : Total Stems ----
## ------------------------------------------------ ##

# Results
## TSF doesn't affect the ratio of flowering to total
## Stocking's effect is marginally significant (p = 0.078)

# Define limit for y axis for both plots
rat_flr_tot_ylim <- ylim(0, 1)

# Create TSF plot
rat_flr_tot_tsf <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("TSF"),
                       response = "Ratio.Flowering.vs.Total.Stems", 
                       drop_na = T) %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Flowering:Total Stems") +
  geom_text(label = "NS", x = 2, y = 1, size = 6) +
  rat_flr_tot_ylim

# Look at it
rat_flr_tot_tsf

# Now make the stocking rate one
rat_flr_tot_mgmt <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Ratio.Flowering.vs.Total.Stems") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Flowering:Total Stems") +
  geom_text(label = "a", x = 0.7, y = 0.9, size = 6) +
  geom_text(label = "ab", x = 1.7, y = 0.88, size = 6) +
  geom_text(label = "b", x = 2.7, y = 0.67, size = 6) +
  geom_text(label = "p = 0.078", x = 2.75, y = 1, size = 6) +
  rat_flr_tot_ylim +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Check it
rat_flr_tot_mgmt

# Combine the plots!
cowplot::plot_grid(rat_flr_tot_tsf, rat_flr_tot_mgmt,
                   nrow = 1, ncol = 2)

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig4.png"),
       plot = last_plot(), width = 6, height = 5, unit = "in")

 ## ------------------------------------------------ ##
            # Fig5 - # Buds & Flowers ----
## ------------------------------------------------ ##

# Results
## No significant relationships with fixed effects

# Set y limit
budflr_ylim <- ylim(0, 300)

# Create TSF plot
budflr_tsf <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("TSF"),
                       response = "Tot.Bud.n.Flr", drop_na = T) %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Buds & Flowers") +
  geom_text(label = "NS", x = 2, y = 300, size = 6) +
  budflr_ylim

# Look at it
budflr_tsf

# Now make the stocking rate one
budflr_mgmt <- mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Tot.Bud.n.Flr") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Buds & Flowers") +
  geom_text(label = "NS", x = 3.2, y = 300, size = 6) +
  budflr_ylim +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Check it
budflr_mgmt

# Combine the plots!
cowplot::plot_grid(budflr_tsf, budflr_mgmt, nrow = 1, ncol = 2)

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig5.png"),
       plot = last_plot(), width = 6, height = 5, unit = "in")

## ------------------------------------------------ ##
      # Fig6 - Ratio Bitten : Total Stems ----
## ------------------------------------------------ ##

# Results
## Significant effect of nearby shrubs

# Make figure
mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = "Shrub.Abun.1m",
                       response = "Ratio.Bitten.vs.Total.Stems",
                       drop_na = TRUE) %>%
  # Make graph
  ggplot(data = ., aes(x = Shrub.Abun.1m, y = mean, fill = "a")) +
  geom_smooth(aes(color = "b"), method = "lm", formula = "y ~ x", se = F) +
  geom_errorbar(aes(ymin = mean - std_error, ymax = mean + std_error),
                width = 0.25) +
  geom_point(size = 3, pch = 25) + 
  labs(x = "Shrubs within 1 meter", y = "Bitten:Total Stems") +
  scale_x_continuous() +
  scale_fill_manual(values = "#35978f") +
  scale_color_manual(values = 'black') +
  theme(legend.position = "none") +
  supportR::theme_lyon() + 
  ylim(0, 0.5) +
  geom_text(label = "p = 0.018", x = 23, y = 0.5, size = 6)

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig6.png"),
       plot = last_plot(), width = 5, height = 5, unit = "in")

## ------------------------------------------------ ##
            # Fig7 - Monarch Immatures ----
## ------------------------------------------------ ##

# Results
## Nonsignificant relationship between monarch immatures and stocking type

# Create TSF plot
mkwd %>%
  # Get summary table
  supportR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Tot.Monarch.Immatures") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Monarch Immatures") +
  geom_text(label = "NS", x = 3.2, y = 1.5, size = 6) +
  ylim(0, 1.5)

# Export figure
ggsave(filename = file.path("figures", "Asclepias_Fig7.png"),
       plot = last_plot(), width = 4, height = 4, unit = "in")

# End ----
