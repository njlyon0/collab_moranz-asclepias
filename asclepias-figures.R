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
librarian::shelf(tidyverse, njlyon0/helpR, psych, cowplot)

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
    helpR::theme_lyon() }

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
  helpR::theme_lyon() }

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
    helpR::theme_lyon() }

## ------------------------------------------------ ##
              # Fig2 - # Bitten Stems ----
## ------------------------------------------------ ##

# Results:
## Time Since Fire (TSF) and Stocking *separately* affect number bitten stems

# Create TSF plot
tot_bitten_tsf <- mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("TSF"),
                response = "Tot.Bitten.Stems", drop_na = T) %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Bitten Stems") + 
  geom_smooth(aes(color = "b"), formula = "y ~ x", 
              se = F, method = 'lm')

# Look at it
tot_bitten_tsf

# Now make the stocking rate one
tot_bitten_mgmt <- mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Tot.Bitten.Stems") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Bitten Stems") +
  geom_text(label = "a", x = 0.7, y = 0.6, size = 6) +
  geom_text(label = "b", x = 1.7, y = 1.3, size = 6) +
  geom_text(label = "c", x = 2.7, y = 2.2, size = 6)
  
# Check it
tot_bitten_mgmt

# Combine the plots!
cowplot::plot_grid(tot_bitten_tsf, tot_bitten_mgmt,
                   nrow = 1, ncol = 2)

# Export
ggsave(filename = file.path("figures", "Asclepias_Fig2.pdf"),
       plot = last_plot(), width = 6.5, height = 4, unit = "in")

## ------------------------------------------------ ##
          # Fig3 - # Flowering Stems ----
## ------------------------------------------------ ##

# Results
## No sig differences

# Create TSF plot
flr_stem_tsf <- mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("TSF"),
                       response = "Num.Stems.ALL.Flowering.Stages") %>%
  # Generate plot
  tsf_fig_skeleton(df = ., ylab = "Flowering Stems (all stages)") +
  geom_text(label = "NS", x = 2, y = 3.7, size = 6)

# Look at it
flr_stem_tsf

# Now make the stocking rate one
flr_stem_mgmt <- mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("Stocking.Type"),
                       response = "Num.Stems.ALL.Flowering.Stages") %>%
  # Relevel factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make graph
  grz_fig_skeleton(df = ., ylab = "Flowering Stems (all stages)") +
  geom_text(label = "NS", x = 3.2, y = 4, size = 6)

# Check it
flr_stem_mgmt

# Combine the plots!
cowplot::plot_grid(flr_stem_tsf, flr_stem_mgmt,
                   nrow = 1, ncol = 2)

# Export
ggsave(filename = file.path("figures", "Asclepias_Fig3.pdf"),
       plot = last_plot(), width = 6.5, height = 4, unit = "in")

## ------------------------------------------------ ##
    # Fig4 - Ratio of Flowering : Total Stems ----
## ------------------------------------------------ ##

# Results
## Significant TSF x Stocking interaction

# Generate plot
mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("TSF", "Stocking.Type"),
                       response = "Ratio.Flowering.vs.Total.Stems") %>%
  # Re-level factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make plot
  ixn_fig_skeleton(df = ., ylab = "Flowering:Total Stems") +
  theme(legend.position = c(0.8, 0.85))

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig4.pdf"),
       plot = last_plot(), width = 5, height = 5, unit = "in")

## ------------------------------------------------ ##
            # Fig5 - # Buds & Flowers ----
## ------------------------------------------------ ##

# Results
## Significant TSF * Grazing interaction

# Generate plot
mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = c("TSF", "Stocking.Type"),
                       response = "Tot.Bud.n.Flr") %>%
  # Re-level factor
  dplyr::mutate(Stocking.Type = factor(Stocking.Type,
                                       levels = c("None", "SLS", "IES"))) %>%
  # Make plot
  ixn_fig_skeleton(df = ., ylab = "Buds & Flowers") +
  theme(legend.position = c(0.8, 0.85))

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig5.pdf"),
       plot = last_plot(), width = 5, height = 5, unit = "in")

## ------------------------------------------------ ##
      # Fig6 - Ratio Bitten : Total Stems ----
## ------------------------------------------------ ##

# Results
## Significant effect of nearby shrubs

mkwd %>%
  # Get summary table
  helpR::summary_table(data = ., groups = "Shrub.Abun.1m",
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
  helpR::theme_lyon()

# Export this figure
ggsave(filename = file.path("figures", "Asclepias_Fig6.pdf"),
       plot = last_plot(), width = 5, height = 5, unit = "in")

## ------------------------------------------------ ##
# Q7 - Monarch Immatures ----
## ------------------------------------------------ ##

# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Tot.Monarch.Immatures))

# Distribution check
psych::multi.hist(mkwd_sub$Tot.Monarch.Immatures)

# Check whether interaction is significant
summary(lme4::glmer(Tot.Monarch.Immatures ~ Stocking.Type + 
                      (1|Year) + (1|Site),
                    data = mkwd_sub, family = "poisson"))

# Skip checking other factor level because there were no immatures in the "No" grazing so we can't put that factor in the intercept

mkwd_sub %>%
  dplyr::group_by(Stocking.Type) %>%
  dplyr::summarize(mean_monarch = mean(Tot.Monarch.Immatures, na.rm = T))








## ------------------------------------------------ ##
              # Stem Length Figure ####
## ------------------------------------------------ ##
# Stem length graph gets its own figure solo

# Need to summarize
mkwd.height <- summarySE(data = mkwd, measurevar = "Avg.Height",
                         groupvars = c("TSF"), na.rm = T)
mkwd.height$Dummy <- rep("x", nrow(mkwd.height))

# Create the plot
mkwd.height.plt1 <- ggplot(mkwd.height, aes(x = TSF, y = Avg.Height,
                                            fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Avg.Height + se, ymin = Avg.Height - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3.5) +
  labs(x = "Time Since Fire (Years)", y = "Average Stem Length (cm)") +
  ylim(0, 101) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; mkwd.height.plt1

# Plot with just grazing
mkwd.height.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Avg.Height,
                                     fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "a", x = 0.8, y = 70, size = 6) +
  geom_text(label = "a", x = 1.8, y = 70, size = 6) +
  geom_text(label = "b", x = 2.8, y = 50, size = 6) +
  labs(x = "Time Since Fire (Years)", y = "Average Stem Length (cm)") +
  scale_fill_manual(values = stk_colors) +
  ylim(0, 101) +
  pref_theme + axis_angle; mkwd.height.plt2

# Make the two panel graph including both
egg::ggarrange(mkwd.height.plt1, mkwd.height.plt2, nrow = 1)
mkwd.height.panels <- egg::ggarrange(mkwd.height.plt1, mkwd.height.plt2, nrow = 1)

# Save the plot
ggsave("./Figures/Stem-Length.pdf", plot = mkwd.height.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
              # Reproductive Figure ####
## ------------------------------------------------ ##
# 3-panel figure
  # 1) Buds & Flowers
  # 2) Flowering Stems
  # 3) Flowering:Total

## ------------------------------ ##
          # Buds + Flowers
## ------------------------------ ##
# Need to summarize
bud.n.flr.stm <- summarySE(data = mkwd,
                                measurevar = "Tot.Bud.n.Flr",
                                groupvars = c("TSF", "Grazing"), na.rm = T)

# Make the plot
bud.flr.plt <- ggplot(bud.n.flr.stm, aes(x = TSF, y = Tot.Bud.n.Flr,
                               fill = Grazing, shape = Grazing)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Grazing)) +
  geom_errorbar(aes(ymax = Tot.Bud.n.Flr + se,
                    ymin = Tot.Bud.n.Flr - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3.5, position = dodge) + 
  labs(x = "Time Since Fire (Years)", y = "Buds and Flowers") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  scale_shape_manual(values = stk_shps) +
  pref_theme + tsf.x.brks + theme(legend.position = c(0.85, 0.85))
bud.flr.plt

## ------------------------------ ##
        # Flowering Stems
## ------------------------------ ##
# Summarize the response of interest
mkwd.all.flr.stm <- summarySE(data = mkwd, measurevar = "Num.Stems.ALL.Flowering.Stages",
                              groupvars = c("TSF"), na.rm = T)
## And add a new dummy column to that
mkwd.all.flr.stm$Dummy <- rep("x", nrow(mkwd.all.flr.stm))

# Create the plot
flr.stm.plt1 <- ggplot(mkwd.all.flr.stm, aes(x = TSF, y = Num.Stems.ALL.Flowering.Stages,
                                             fill = Dummy, color = Dummy)) + 
  geom_text(label = "NS", x = 1.85, y = 25, size = 6) +
  geom_errorbar(aes(ymax = Num.Stems.ALL.Flowering.Stages + se,
                    ymin = Num.Stems.ALL.Flowering.Stages - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Flowering Stems") +
  ylim(0, 27) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; flr.stm.plt1

flr.stm.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Num.Stems.ALL.Flowering.Stages, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "NS", x = 0.75, y = 25, size = 6) +
  labs(x = "Grazing Treatment", y = "Flowering Stems") +
  ylim(0, 27) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle; flr.stm.plt2

# Make the two panel graph including both
egg::ggarrange(flr.stm.plt1, flr.stm.plt2, nrow = 1)
flr.stm.panels <- egg::ggarrange(flr.stm.plt1, flr.stm.plt2, nrow = 1)

## ------------------------------ ##
        # Flowering:Total
## ------------------------------ ##
# Need to summarize
flr.stm.rat <- summarySE(data = mkwd,
                            measurevar = "Ratio.Flowering.vs.Total.Stems",
                            groupvars = c("TSF", "Grazing", "Combo.Factor"), na.rm = T)
# Make the plot
flr.rat.plt <- ggplot(flr.stm.rat, aes(x = TSF, y = Ratio.Flowering.vs.Total.Stems,
                           fill = Grazing, shape = Grazing)) + 
  geom_smooth(aes(color = Grazing), method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Ratio.Flowering.vs.Total.Stems + se,
                    ymin = Ratio.Flowering.vs.Total.Stems - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3.5, position = dodge) + 
  labs(x = "Time Since Fire (Years)", y = "Flowering:Total Stems") +
  ylim(0.5, 1) +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  scale_shape_manual(values = stk_shps) +
  pref_theme + theme(legend.position = c(0.8, 0.85)) + tsf.x.brks
flr.rat.plt

## ------------------------------ ##
        # Figure Assembly
## ------------------------------ ##
# Make the compound figure
plot_grid(bud.flr.plt, flr.stm.panels, flr.rat.plt,
          ncol = 1, nrow = 3,
          labels = c("A", "B", "C"))

# Save it
ggplot2::ggsave("./Figures/Reproductive.pdf",
                width = 8.5, height = 11,
                plot = last_plot())

## ------------------------------------------------ ##
               # Herbivory Figure ####
## ------------------------------------------------ ##
# 4-panel figure
# 1) Axillary Shoots
# 2) Bitten Stems
# 3) Axillary vs. Bitten
# 4) Bitten:Total

## ------------------------------ ##
         # Axillary Shoots
## ------------------------------ ##
# Summarize the response of interest
mkwd.all.ax.sht <- summarySE(data = mkwd, measurevar = "Tot.Axillary.Shoots",
                             groupvars = c("TSF"), na.rm = T)
## And add a new dummy column to that
mkwd.all.ax.sht$Dummy <- rep("x", nrow(mkwd.all.ax.sht))

# Create the plot
ax.sht.plt1 <- ggplot(mkwd.all.ax.sht, aes(x = TSF, y = Tot.Axillary.Shoots,
                                           fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Axillary.Shoots + se,
                    ymin = Tot.Axillary.Shoots - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Axillary Shoots") +
  ylim(0, 35) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; ax.sht.plt1

ax.sht.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Tot.Axillary.Shoots, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "a", x = 0.8, y = 4.5, size = 6) +
  geom_text(label = "b", x = 1.8, y = 5.5, size = 6) +
  geom_text(label = "c", x = 2.8, y = 6, size = 6) +
  labs(x = "Grazing Treatment", y = "Axillary Shoots") +
  ylim(0, 35) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle
ax.sht.plt2

# Make the two panel graph including both
egg::ggarrange(ax.sht.plt1, ax.sht.plt2, nrow = 1)
ax.sht.panels <- egg::ggarrange(ax.sht.plt1, ax.sht.plt2, nrow = 1)

## ------------------------------ ##
          # Bitten Stems
## ------------------------------ ##
# Summarize
mkwd.tot.btn.stm <- summarySE(data = mkwd, measurevar = "Tot.Bitten.Stems",
                              groupvars = c("TSF"), na.rm = T)

## make dummy column
mkwd.tot.btn.stm$For.Plotting <- rep("x", nrow(mkwd.tot.btn.stm))

# Make the first plot
btn.plt1 <- ggplot(mkwd.tot.btn.stm, aes(x = TSF, y = Tot.Bitten.Stems, fill = For.Plotting)) + 
  geom_smooth(method = 'lm', se = F, aes(color = For.Plotting)) +
  geom_errorbar(aes(ymax = Tot.Bitten.Stems + se, ymin = Tot.Bitten.Stems - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Bitten Stems") +
  ylim(0, 15) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; btn.plt1

# Now make the second
btn.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Tot.Bitten.Stems, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "a", x = 0.8, y = 1.5, size = 6) +
  geom_text(label = "b", x = 1.8, y = 2, size = 6) +
  geom_text(label = "c", x = 2.8, y = 4, size = 6) +
  labs(x = "Grazing Treatment", y = "Bitten Stems") +
  ylim(0, 15) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle; btn.plt2

# Make the two panel graph including both
egg::ggarrange(btn.plt1, btn.plt2, nrow = 1)
btn.panels <- egg::ggarrange(btn.plt1, btn.plt2, nrow = 1)

## ------------------------------ ##
        # Axillary ~ Bitten
## ------------------------------ ##
# Summarize
ax.vs.btn.btn <- summarySE(data = mkwd, measurevar = "Tot.Axillary.Shoots",
                           groupvars = c("Tot.Bitten.Stems"), na.rm = T)
## Add a dummy column
ax.vs.btn.btn$Dummy <- rep("x", nrow(ax.vs.btn.btn))

# Make plot
ax.vs.btn.plt <- ggplot(ax.vs.btn.btn, aes(x = Tot.Bitten.Stems,
                          y = Tot.Axillary.Shoots,
                          fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Axillary.Shoots + se,
                    ymin = Tot.Axillary.Shoots - se),
                width = 0.5) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Bitten Stems", y = "Axillary Shoots") +
  scale_fill_manual(values = btn_color) +
  scale_color_manual(values = 'black') +
  pref_theme

ax.vs.btn.plt

## ------------------------------ ##
         # Bitten:Total
## ------------------------------ ##
# Summarize
mkwd.rat.btn <- summarySE(data = mkwd,
                          measurevar = "Ratio.Bitten.vs.Total.Stems",
                          groupvars = c("TSF"), na.rm = T)

## make dummy column
mkwd.rat.btn$For.Plotting <- rep("x", nrow(mkwd.rat.btn))

# Make the TSF panel
btn.rat.plt1 <- ggplot(mkwd.rat.btn, aes(x = TSF, y = Ratio.Bitten.vs.Total.Stems,
                                         fill = For.Plotting)) + 
  geom_errorbar(aes(ymax = Ratio.Bitten.vs.Total.Stems + se,
                    ymin = Ratio.Bitten.vs.Total.Stems - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  geom_text(label = "NS", x = 0.05, y = 0.9, size = 6) +
  labs(x = "Time Since Fire (years)", y = "Bitten:Total Stems") +
  ylim(0, 1) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; btn.rat.plt1

# Plot
btn.rat.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Ratio.Bitten.vs.Total.Stems, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "a", x = 0.8, y = 0.24, size = 6) +
  geom_text(label = "b", x = 1.8, y = 0.58, size = 6) +
  geom_text(label = "c", x = 2.5, y = 0.95, size = 6) +
  labs(x = "Grazing Treatment", y = "Bitten:Total Stems") +
  ylim(0, 1) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle; btn.rat.plt2

# Make the two panel graph including both
egg::ggarrange(btn.rat.plt1, btn.rat.plt2, nrow = 1)
btn.rat.panels <- egg::ggarrange(btn.rat.plt1, btn.rat.plt2, nrow = 1)

## ------------------------------ ##
        # Figure Assembly
## ------------------------------ ##
# Make the compound figure
plot_grid(ax.sht.panels, btn.panels, ax.vs.btn.plt, btn.rat.panels,
          ncol = 1, nrow = 4,
          labels = c("A", "B", "C", "D"))

# Save it
ggplot2::ggsave("./Figures/Herbivory.pdf",
                width = 8.5, height = 11,
                plot = last_plot())

## ------------------------------------------------ ##
                # Shrub Figure ####
## ------------------------------------------------ ##
# 2-panel figure
# 1) Total Bitten ~ Shrub
# 2) Bitten:Total ~ Shrub

## ------------------------------ ##
        # Bitten ~ Shrub
## ------------------------------ ##
# Summarize each response of interest
shrub.btn <- summarySE(data = mkwd, measurevar = "Tot.Bitten.Stems",
                       groupvars = c("Shrub.Abun.1m"), na.rm = T)
## And add a new dummy column to that
shrub.btn$Dummy <- rep("x", nrow(shrub.btn))

# Create the plot
shrub.btn.plt <- ggplot(shrub.btn, aes(x = Shrub.Abun.1m, y = Tot.Bitten.Stems,
                      fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Bitten.Stems + se,
                    ymin = Tot.Bitten.Stems - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Shrubs within 1 meter", y = "Bitten Stems") +
  scale_fill_manual(values = shrub_color) +
  scale_color_manual(values = 'black') +
  ylim(-0.15, 3.5) +
  pref_theme

shrub.btn.plt

## ------------------------------ ##
      # Bitten:Total ~ Shrub
## ------------------------------ ##
# Summarize
shrub.ratio <- summarySE(data = mkwd, measurevar = "Ratio.Bitten.vs.Total.Stems",
                         groupvars = c("Shrub.Abun.1m"), na.rm = T)
## Add a dummy column
shrub.ratio$Dummy <- rep("x", nrow(shrub.ratio))

# Make plot
shrub.rat.plt <- ggplot(shrub.ratio, aes(x = Shrub.Abun.1m,
                                         y = Ratio.Bitten.vs.Total.Stems,
                        fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Ratio.Bitten.vs.Total.Stems + se,
                    ymin = Ratio.Bitten.vs.Total.Stems - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Shrubs within 1 meter", y = "Bitten:Total Stems") +
  scale_fill_manual(values = shrub_color) +
  scale_color_manual(values = 'black') +
  ylim(-0.1, 0.7) +
  pref_theme
shrub.rat.plt

## ------------------------------ ##
        # Figure Assembly
## ------------------------------ ##
# Make the compound figure
plot_grid(shrub.btn.plt, shrub.rat.plt,
          ncol = 1, nrow = 2,
          labels = c("A", "B"))

# Save it
ggplot2::ggsave("./Figures/Shrubs.pdf",
                width = 8, height = 8,
                plot = last_plot())

# END ####
