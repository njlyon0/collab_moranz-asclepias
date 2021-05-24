## ---------------------------------------------------------------------------------------- ##
              # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code creates starter plots based on preliminary statistical results
  ## Eventually this will be superceded by a publication-quality figures script

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/_Publications/2021_Moranz_Asclepias/Moranz-AsclepiasCollab")
## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse); library(Rmisc)

## ------------------------------------------------ ##
                  # Housekeeping ####
## ------------------------------------------------ ##
# Read in data
mkwd <- read_excel("./Data/Asclepias-TIDY.xlsx", sheet = "Data")

# Check the structure
str(mkwd)

# Some of our columns are not the right format so let's fix 'em all here
mkwd$Site <- as.factor(mkwd$Site)
mkwd$Patch <- as.factor(mkwd$Patch)
mkwd$Plant.ID <- as.factor(mkwd$Plant.ID)
mkwd$Management <- as.factor(mkwd$Management)
mkwd$Stocking <- factor(mkwd$Stocking, levels = c("None", "SLS", "IES"))
mkwd$GrazingLawn <- as.factor(mkwd$GrazingLawn)
str(mkwd)

# Give the summary of the df a quick once-over too
summary(mkwd)

# Make any needed plotting aesthetics shared among graphs here
stk_colors <- c("None" = "#b2abd2", "SLS" = "#fdb863", "IES" = "#b35806")
mgmt_colors <- c("BO" = "#d73027", "GB" = "#e0f3f8", "PBG" = "#74add1")
std_color <- "#9ebcda"
dodge <- position_dodge(width = 0.5)
pref_theme <- theme_classic() + theme(axis.text = element_text(size = 13),
                                      axis.title = element_text(size = 15),
                                      legend.position = "none",
                                      legend.title = element_blank())

## ------------------------------------------------ ##
        # Plant Characteristics Tests ####
## ------------------------------------------------ ##
# Average height plot
## Create the plot
ggplot(mkwd, aes(x = Stocking, y = Avg.Height, fill = Stocking)) + 
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.8, y = 71, size = 6) +
  geom_text(label = "a", x = 1.8, y = 67, size = 6) +
  geom_text(label = "b", x = 2.8, y = 59, size = 6) +
  labs(x = "Stocking Treatment", y = "Average Height (cm)") +
  scale_fill_manual(values = stk_colors) +
  pref_theme
## Save the plot!
ggsave("./Graphs/Avg-Height.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

# Average number of buds plot
## Need to summarize before plotting
mkwd.avg.bud <- summarySE(data = mkwd,
                      measurevar = "Avg.Bud",
                      groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.avg.bud, aes(x = TSF, y = Avg.Bud, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Avg.Bud + se, ymin = Avg.Bud - se), width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Average Bud Abundance") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Avg-Bud-Num.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

# Average number of flowers plot
## Summarize
mkwd.avg.flr <- summarySE(data = mkwd,
                      measurevar = "Avg.Flr",
                      groupvars = c("Year"), na.rm = T)
## Make the plot
ggplot(mkwd.avg.flr, aes(x = Year, y = Avg.Flr, fill = rep('z', nrow(mkwd.avg.flr)))) + 
  geom_smooth(method = 'lm', se = F, color = 'black') +
  geom_errorbar(aes(ymax = Avg.Flr + se, ymin = Avg.Flr - se), width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Average Flower Abundance") +
  scale_fill_manual(values = std_color) +
  #scale_color_manual(values = stk_colors) +
  pref_theme
## Save the plot
ggsave("./Graphs/Avg-Flr-Num.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

# Total bud plot
## year = sig & TSF*SLS = sig

# Total flower plot
## year = sig

# Total number of stems of any flowering stage plot
## Create the plot
ggplot(mkwd, aes(x = Stocking, y = Num.Stems.ALL.Flowering.Stages, fill = Stocking)) + 
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.8, y = 6, size = 6) +
  geom_text(label = "a", x = 1.8, y = 5, size = 6) +
  geom_text(label = "b", x = 2.8, y = 5, size = 6) +
  labs(x = "Stocking Treatment", y = "Number Flowering Stems (ANY STAGE)") +
  scale_fill_manual(values = stk_colors) +
  pref_theme
## Save the plot!
ggsave("./Graphs/Num-Flowering-Stems_ANY-STAGE.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

# Total axillary shoots plot
  ## Need to summarize
mkwd.tot.ax <- summarySE(data = mkwd,
                          measurevar = "Tot.Axillary.Shoots",
                          groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.tot.ax, aes(x = TSF, y = Tot.Axillary.Shoots, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Tot.Axillary.Shoots + se, ymin = Tot.Axillary.Shoots - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Total Axillary Shoots") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Tot-Axillary-Shoots.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
           # Neighboring Plant Tests ####
## ------------------------------------------------ ##
# Asclepias tuberosa (butterfly milkweed) within 1 meter of the focal plant
## Need to summarize
mkwd.consp.1m <- summarySE(data = mkwd,
                         measurevar = "ASCTUB.Abun.1m",
                         groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.consp.1m, aes(x = TSF, y = ASCTUB.Abun.1m, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = ASCTUB.Abun.1m + se, ymin = ASCTUB.Abun.1m - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Conspecifics within 1 meter") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Neighbor-ASCTUB-1m.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

# Conspecifics within 2 meters plot
## Need to summarize
mkwd.consp.2m <- summarySE(data = mkwd,
                           measurevar = "ASCTUB.Abun.2m",
                           groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.consp.2m, aes(x = TSF, y = ASCTUB.Abun.2m, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = ASCTUB.Abun.2m + se, ymin = ASCTUB.Abun.2m - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Conspecifics within 2 meters") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Neighbor-ASCTUB-2m.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

# Shrubs within 1 meter plot
## Create the plot
ggplot(mkwd, aes(x = Stocking, y = Shrub.Abun.1m, fill = Stocking)) + 
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "ab", x = 0.75, y = 2, size = 6) +
  geom_text(label = "a", x = 1.8, y = 2, size = 6) +
  geom_text(label = "b", x = 2.8, y = 3, size = 6) +
  labs(x = "Stocking Treatment", y = "Shrubs within 1 meter") +
  scale_fill_manual(values = stk_colors) +
  pref_theme
## Save the plot!
ggsave("./Graphs/Neighbor-Shrub-1m.pdf", plot = last_plot(), dpi = 600, width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
               # Herbivory Tests ####
## ------------------------------------------------ ##
# Total bitten stems
## Need to summarize
mkwd.tot.btn.stm <- summarySE(data = mkwd,
                              measurevar = "Tot.Bitten.Stems",
                              groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.tot.btn.stm, aes(x = TSF, y = Tot.Bitten.Stems, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Tot.Bitten.Stems + se, ymin = Tot.Bitten.Stems - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Total Bitten Stems") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Tot-Bitten-Stems.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

# Number of axillary shoots bitten
## Need to summarize
mkwd.ax.sht.btn <- summarySE(data = mkwd,
                              measurevar = "Num.Axillary.Shoots.Bitten",
                              groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.ax.sht.btn, aes(x = TSF, y = Num.Axillary.Shoots.Bitten, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Num.Axillary.Shoots.Bitten + se, ymin = Num.Axillary.Shoots.Bitten - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Total Bitten Axillary Shoots") +
  geom_text(label = "NS", x = 3.5, y = 0.25, size = 6) +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Bitten-Axillary-Shoots.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

# Number of bitten stems with axillary shoots
## Need to summarize
mkwd.btn.stm.ax.sht <- summarySE(data = mkwd,
                             measurevar = "Num.Bitten.Stems.w.Axillary.Shoots",
                             groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.btn.stm.ax.sht, aes(x = TSF, y = Num.Bitten.Stems.w.Axillary.Shoots, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Num.Bitten.Stems.w.Axillary.Shoots + se,
                    ymin = Num.Bitten.Stems.w.Axillary.Shoots - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Bitten Stems w/ Axillary Shoots") +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Bitten-Stems-w-Axillary-Shoots.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
             # Invertebrate Tests ####
## ------------------------------------------------ ##
# Mostly not run at this point due to convergence issues
## (Many NAs and the data that are there are largely 0s)

# Total monarch immatures (eggs + larvae) plot
## Need to summarize
mkwd.mnrch.imtr <- summarySE(data = mkwd,
                             measurevar = "Tot.Monarch.Immatures",
                             groupvars = c("TSF", "Stocking"), na.rm = T)
## Make the plot
ggplot(mkwd.mnrch.imtr, aes(x = TSF, y = Tot.Monarch.Immatures, fill = Stocking)) + 
  geom_smooth(method = 'lm', se = F, aes(color = Stocking)) +
  geom_errorbar(aes(ymax = Tot.Monarch.Immatures + se, ymin = Tot.Monarch.Immatures - se),
                width = 0.3, position = dodge) +
  geom_point(size = 3, pch = 23, position = dodge) + 
  labs(x = "Time Since Fire (years)", y = "Monarch Eggs & Larvae") +
  geom_text(label = "NS", x = 3.5, y = 0.45, size = 6) +
  scale_fill_manual(values = stk_colors) +
  scale_color_manual(values = stk_colors) +
  pref_theme + theme(legend.position = c(0.8, 0.8))
## Save the plot
ggsave("./Graphs/Monarch-Immatures.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")


# END ####
