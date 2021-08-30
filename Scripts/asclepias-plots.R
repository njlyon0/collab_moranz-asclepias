## ----------------------------------------------------------------------- ##
      # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ----------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code creates starter plots based on preliminary statistical results
  ## Eventually this will be superceded by a publication-quality figures script

# Clear the environment
rm(list = ls())

# Set the working directory
  ## Session -> Set Working Directory -> Choose Directory...
myWD <- getwd()
setwd(myWD)

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse)
library(Rmisc); library(egg)

## ------------------------------------------------ ##
                  # Housekeeping ####
## ------------------------------------------------ ##
# Read in data
all.mkwd <- read_excel("./Data/Asclepias-TIDY.xlsx",
                       sheet = "Data", guess_max = 10000)
all.bfly <- read_excel("./Data/Monarch-Adult-TIDY.xlsx",
                       sheet = "Data", guess_max = 10000)

# Check the structure
str(all.mkwd)
str(all.bfly)

# Some of our columns are not the right format so let's fix 'em all here
all.mkwd$Site <- as.factor(all.mkwd$Site)
all.mkwd$Management <- as.factor(all.mkwd$Management)
all.mkwd$Stocking <- factor(all.mkwd$Stocking, 
                            levels = c("None", "SLS", "IES"))
all.mkwd$GrazingLawn <- as.factor(all.mkwd$GrazingLawn)
str(all.mkwd)

all.bfly$Site <- as.factor(all.bfly$Site)
str(all.bfly)

# For ease of plotting, let's give our plotting the right aesthetics
all.mkwd$Grazing <- all.mkwd$Stocking
all.mkwd$Grazing <- gsub("IES", "Intensive Early", all.mkwd$Grazing)
all.mkwd$Grazing <- gsub("SLS", "Season Long", all.mkwd$Grazing)
all.mkwd$Grazing <- factor(all.mkwd$Grazing,
                           levels = c("None", "Season Long", "Intensive Early"))
sort(unique(all.mkwd$Grazing))

all.bfly$Grazing <- all.bfly$Stocking.Type
all.bfly$Grazing <- gsub("IES", "Intensive Early", all.bfly$Grazing)
all.bfly$Grazing <- gsub("SLS", "Season Long", all.bfly$Grazing)
all.bfly$Grazing <- factor(all.bfly$Grazing,
                           levels = c("None", "Season Long", "Intensive Early"))
sort(unique(all.bfly$Grazing))

# If we treat TSF as a factor and have significant TSF*Grazing interactions
  ## We'll need a combo column for plotting that relationship
all.mkwd$Combo.Factor <- with(all.mkwd, paste0(TSF, "-", Grazing))
sort(unique(all.mkwd$Combo.Factor))

# Factor this column and manually set the level order
all.mkwd$Combo.Factor <- factor(all.mkwd$Combo.Factor,
                                levels = c("0-None", "0-Season Long", "0-Intensive Early",
                                           "1-None", "1-Season Long", "1-Intensive Early",
                                           "2-None", "2-Season Long", "2-Intensive Early"))
sort(unique(all.mkwd$Combo.Factor))

# Give the summary of the df a quick once-over too
summary(all.mkwd)

# Subset the data to remove the TSF 3 & 4 data
  ## Two sites that were ungrazed failed to be burned on schedule resulting in overly high TSF
  ## These lack direct comparability with the other grazing types
  ## Thus they are removed from consideration here
mkwd <- filter(all.mkwd, TSF <= 2)
sort(unique(mkwd$TSF))
bfly <- filter(all.bfly, TSF <= 2)
sort(unique(bfly$TSF))

# Make any needed plotting aesthetics shared among graphs here
stk_colors <- c("None" = "#b2abd2", #"SLS" = "#fdb863", "IES" = "#b35806",
                "Season Long" = "#fdb863", "Intensive Early" = "#b35806")
stk_shps <- c("None" = 21, "Season Long" = 25, "Intensive Early" = 24)
mgmt_colors <- c("BO" = "#d73027", "GB" = "#e0f3f8", "PBG" = "#74add1")
tsf_color <- "#FFFFFF"
shrub_color <- "#7fbc41"
btn_color <- "#35978f"
dodge <- position_dodge(width = 0.5)
pref_theme <- theme_classic() + theme(axis.text = element_text(size = 13),
                                      axis.title = element_text(size = 15),
                                      legend.position = "none",
                                      legend.title.align = 0.5,
                                      legend.background = element_blank(),
                                      legend.text = element_text(size = 12),
                                      legend.title = element_text(size = 13))
axis_angle <- theme(axis.text.x = element_text(angle = 25, hjust = 1))
tsf.x.brks <- scale_x_continuous(breaks = c(0, 1, 2))

# Sometimes we will want all groups to have the same color/shape/fill (i.e., aesthetic)
  ## It is easiest to do that by adding a dummy column here
  ## Then calling that column to assign those aesthetics
mkwd$For.Plotting <- rep("x", nrow(mkwd))

## ------------------------------------------------ ##
      # Q1 - # stems of any flowering stage ####
## ------------------------------------------------ ##
# Summarize the response of interest
mkwd.all.flr.stm <- summarySE(data = mkwd, measurevar = "Num.Stems.ALL.Flowering.Stages",
                              groupvars = c("TSF"), na.rm = T)
  ## And add a new dummy column to that
mkwd.all.flr.stm$Dummy <- rep("x", nrow(mkwd.all.flr.stm))

# Create the plot
flr.stm.plt1 <- ggplot(mkwd.all.flr.stm, aes(x = TSF, y = Num.Stems.ALL.Flowering.Stages,
                                             fill = Dummy, color = Dummy)) + 
  geom_text(label = "NS", x = 1.85, y = 25, size = 6) +
  #geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Num.Stems.ALL.Flowering.Stages + se,
                    ymin = Num.Stems.ALL.Flowering.Stages - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Flowering Stems (all stages)") +
  ylim(0, 27) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; flr.stm.plt1

flr.stm.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Num.Stems.ALL.Flowering.Stages, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "NS", x = 0.75, y = 25, size = 6) +
  #geom_text(label = "a", x = 0.8, y = 4.5, size = 6) +
  #geom_text(label = "ab", x = 1.8, y = 4.5, size = 6) +
  #geom_text(label = "b", x = 2.8, y = 5.5, size = 6) +
  labs(x = "Grazing Treatment", y = "Flowering Stems (all stages)") +
  ylim(0, 27) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle; flr.stm.plt2

# Make the two panel graph including both
egg::ggarrange(flr.stm.plt1, flr.stm.plt2, nrow = 1)
flr.stm.panels <- egg::ggarrange(flr.stm.plt1, flr.stm.plt2, nrow = 1)

# Save it
ggsave("./Graphs/Flowering-Stems_ANY-STAGE.pdf", plot = flr.stm.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
        # Q2 - # stems that won't flower ####
## ------------------------------------------------ ##
# Need to summarize
mkwd.noflr.stm <- summarySE(data = mkwd,
                              measurevar = "Ratio.Flowering.vs.Total.Stems",
                              groupvars = c("TSF", "Grazing", "Combo.Factor"), na.rm = T)
# Make the plot
ggplot(mkwd.noflr.stm, aes(x = TSF, y = Ratio.Flowering.vs.Total.Stems,
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

# Save the plot
ggsave("./Graphs/Ratio-Flowering-Total-Stems.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
          # Q3 - total buds + flowers ####
## ------------------------------------------------ ##
# Need to summarize
mkwd.bud.n.flr.stm <- summarySE(data = mkwd,
                            measurevar = "Tot.Bud.n.Flr",
                            groupvars = c("TSF", "Grazing"), na.rm = T)

# Make the plot
ggplot(mkwd.bud.n.flr.stm, aes(x = TSF, y = Tot.Bud.n.Flr,
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

# Save the plot
ggsave("./Graphs/Buds-and-Flowers.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
    # Q4 - average height of 3 longest stems ####
## ------------------------------------------------ ##
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
ggsave("./Graphs/Avg-Length.pdf", plot = mkwd.height.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
          # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
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

# Save it
ggsave("./Graphs/Bitten-Stems.pdf", plot = btn.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
   # Q6 - ratio of bitten stems : total stems ####
## ------------------------------------------------ ##
# NOTE:
  ## The "total stems" included in this ratio was not directly measured as such
  ## HOWEVER, the total number of:
    ### (1) flowering stems and
    ### (2) nonflowering stems *was* counted
  ## So I added those two together to get the total stems
  ## There were ~20 cases (of ~600) where the resulting ratio was greater than 1
    ### (i.e., more bitten stems were counted than flowering/nonflowering)
  ## And these cases were coerced into NAs to avoid messing with the data

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

# Save it
ggsave("./Graphs/Ratio-Bitten-Total-Stems.pdf", plot = btn.rat.panels,
       dpi = 600, width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
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
  labs(x = "Time Since Fire (Years)", y = "# Axillary Shoots") +
  ylim(0, 35) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; ax.sht.plt1

ax.sht.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Tot.Axillary.Shoots, fill = Grazing)) + 
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  geom_text(label = "a", x = 0.8, y = 4.5, size = 6) +
  geom_text(label = "b", x = 1.8, y = 5.5, size = 6) +
  geom_text(label = "c", x = 2.8, y = 6, size = 6) +
  labs(x = "Grazing Treatment", y = "# Axillary Shoots") +
  ylim(0, 35) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle
ax.sht.plt2

# Make the two panel graph including both
egg::ggarrange(ax.sht.plt1, ax.sht.plt2, nrow = 1)
ax.sht.panels <- egg::ggarrange(ax.sht.plt1, ax.sht.plt2, nrow = 1)

# Save it
ggsave("./Graphs/Axillary-Shoots.pdf", plot = ax.sht.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
            # Q8 - monarch immatures ####
## ------------------------------------------------ ##
# "Immatures" = larvae (caterpillars) + eggs

# Summarize the response of interest
mkwd.all.monarchs <- summarySE(data = mkwd, measurevar = "Tot.Monarch.Immatures",
                             groupvars = c("TSF"), na.rm = T)
## And add a new dummy column to that
mkwd.all.monarchs$Dummy <- rep("x", nrow(mkwd.all.monarchs))

# Create the plot
monarchs.plt1 <- ggplot(mkwd.all.monarchs, aes(x = TSF, y = Tot.Monarch.Immatures,
                                           fill = Dummy, color = Dummy)) + 
  geom_text(label = "NS", x = 1.85, y = 10, size = 6) +
  #geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Monarch.Immatures + se,
                    ymin = Tot.Monarch.Immatures - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Monarch Immatures") +
  ylim(0, 10) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; monarchs.plt1

monarchs.plt2 <- ggplot(mkwd, aes(x = Grazing, y = Tot.Monarch.Immatures, fill = Grazing)) + 
  geom_text(label = "NS", x = 0.75, y = 9, size = 6) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  #geom_text(label = "a", x = 0.8, y = 4.5, size = 6) +
  #geom_text(label = "b", x = 1.8, y = 5.5, size = 6) +
  #geom_text(label = "c", x = 2.8, y = 6, size = 6) +
  labs(x = "Grazing Treatment", y = "Monarch Immatures") +
  ylim(0, 10) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle
monarchs.plt2

# Make the two panel graph including both
egg::ggarrange(monarchs.plt1, monarchs.plt2, nrow = 1)
monarchs.panels <- egg::ggarrange(monarchs.plt1, monarchs.plt2, nrow = 1)

# Save it
ggsave("./Graphs/Monarch-Immatures.pdf", plot = monarchs.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
              # Q9 - monarch adults ####
## ------------------------------------------------ ##
# Summarize the response of interest
bfly.adults <- summarySE(data = bfly, measurevar = "Monarch.Adults",
                               groupvars = c("TSF"), na.rm = T)
## And add a new dummy column to that
bfly.adults$Dummy <- rep("x", nrow(bfly.adults))

# Create the plot
adults.plt1 <- ggplot(bfly.adults, aes(x = TSF, y = Monarch.Adults,
                                               fill = Dummy, color = Dummy)) + 
  geom_text(label = "NS", x = 1.85, y = 3.5, size = 6) +
  #geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Monarch.Adults + se,
                    ymin = Monarch.Adults - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Monarch Adults") +
  ylim(1, 4) +
  scale_fill_manual(values = tsf_color) +
  scale_color_manual(values = 'black') +
  pref_theme + tsf.x.brks; adults.plt1

adults.plt2 <- ggplot(bfly, aes(x = Grazing, y = Monarch.Adults, fill = Grazing)) + 
  geom_text(label = "NS", x = 0.75, y = 4, size = 6) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.125) +
  #geom_text(label = "a", x = 0.8, y = 4.5, size = 6) +
  #geom_text(label = "b", x = 1.8, y = 5.5, size = 6) +
  #geom_text(label = "c", x = 2.8, y = 6, size = 6) +
  labs(x = "Grazing Treatment", y = "Monarch Adults") +
  ylim(1, 4) +
  scale_fill_manual(values = stk_colors) +
  pref_theme + axis_angle
adults.plt2

# Make the two panel graph including both
egg::ggarrange(adults.plt1, adults.plt2, nrow = 1)
adults.panels <- egg::ggarrange(adults.plt1, adults.plt2, nrow = 1)

# Save it
ggsave("./Graphs/Monarch-Adults.pdf", plot = adults.panels, dpi = 600,
       width = 8, height = 5, units = "in")

## ------------------------------------------------ ##
 # Q10 - plant quality ~ nearby shrub abundance ####
## ------------------------------------------------ ##

## ------------------------ ##
      ### Total Bitten
## ------------------------ ##
# Summarize each response of interest
shrub.btn <- summarySE(data = mkwd, measurevar = "Tot.Bitten.Stems",
                              groupvars = c("Shrub.Abun.1m"), na.rm = T)
  ## And add a new dummy column to that
shrub.btn$Dummy <- rep("x", nrow(shrub.btn))

# Create the plot
ggplot(shrub.btn, aes(x = Shrub.Abun.1m, y = Tot.Bitten.Stems,
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

# Save the plot!
ggsave("./Graphs/Shrubs-Total-Bitten-Stems.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

## ------------------------ ##
    ### Bitten:Total Stems
## ------------------------ ##
# Summarize
shrub.ratio <- summarySE(data = mkwd, measurevar = "Ratio.Bitten.vs.Total.Stems",
                       groupvars = c("Shrub.Abun.1m"), na.rm = T)
  ## Add a dummy column
shrub.ratio$Dummy <- rep("x", nrow(shrub.ratio))

# Make plot
ggplot(shrub.ratio, aes(x = Shrub.Abun.1m, y = Ratio.Bitten.vs.Total.Stems,
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

# Save
ggsave("./Graphs/Shrubs-Ratio-Bitten-Total-Stems.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

## ------------------------ ##
    ### Bitten:Total Stems
## ------------------------ ##
# Summarize
shrub.axl <- summarySE(data = mkwd, measurevar = "Tot.Axillary.Shoots",
                         groupvars = c("Shrub.Abun.1m"), na.rm = T)
## Add a dummy column
shrub.axl$Dummy <- rep("x", nrow(shrub.axl))

# Make plot
ggplot(shrub.axl, aes(x = Shrub.Abun.1m, y = Tot.Axillary.Shoots,
                        fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Axillary.Shoots + se,
                    ymin = Tot.Axillary.Shoots - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Shrubs within 1 meter", y = "Axillary Shoots") +
  scale_fill_manual(values = shrub_color) +
  scale_color_manual(values = 'black') +
  ylim(-0.05, 5.5) +
  pref_theme

# Save
ggsave("./Graphs/Shrubs-Axillary-Shoots.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
     # Q11 - Axillary Shoots ~ Bitten Stems ####
## ------------------------------------------------ ##
# Summarize
ax.vs.rat.btn <- summarySE(data = mkwd, measurevar = "Tot.Axillary.Shoots",
                       groupvars = c("Tot.Bitten.Stems"), na.rm = T)
## Add a dummy column
ax.vs.rat.btn$Dummy <- rep("x", nrow(ax.vs.rat.btn))

# Make plot
ggplot(ax.vs.rat.btn, aes(x = Tot.Bitten.Stems,
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

# Save
ggsave("./Graphs/Axillary-Shoots-versus-Bitten-Stems.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
   # Supplemental - Neighboring Conspecifics ####
## ------------------------------------------------ ##
# Make the first conspecific plot
ggplot(mkwd, aes(x = Grazing, y = ASCTUB.Abun.1m, fill = Grazing)) +
  geom_violin() +
  geom_point(position = position_jitter(w = 0.3, h = 0), alpha = 0.1) + 
  scale_fill_manual(values = stk_colors) +
  labs(x = "Grazing Treatment", y = "Conspecifics within 1 meter") +
  pref_theme + axis_angle

# Save
ggsave("./Graphs/Conspecifics-1m.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

# Make the second
ggplot(mkwd, aes(x = Grazing, y = ASCTUB.Abun.2m, fill = Grazing)) +
  geom_violin() +
  geom_point(position = position_jitter(w = 0.3, h = 0), alpha = 0.1) + 
  scale_fill_manual(values = stk_colors) +
  labs(x = "Grazing Treatment", y = "Conspecifics within 2 meters") +
  pref_theme + axis_angle

# Save
ggsave("./Graphs/Conspecifics-2m.pdf", plot = last_plot(),
       dpi = 600, width = 5, height = 5, units = "in")

# END ####
