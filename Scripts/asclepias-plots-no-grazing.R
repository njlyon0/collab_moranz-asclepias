## ---------------------------------------------------------------------------------------- ##
              # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## Creates plots of just the no grazing treatments across all five levels of TSF
    ### TSF = Time Since Fire (in years)

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/_Publications/2022_Moranz_Asclepias/Moranz-AsclepiasCollab")
  ## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse); library(Rmisc); library(egg)

## ------------------------------------------------ ##
                  # Housekeeping ####
## ------------------------------------------------ ##
# Read in data
all.mkwd <- read_excel("./Data/Asclepias-TIDY.xlsx", sheet = "Data", guess_max = 10000)

# Check the structure
str(all.mkwd)

# Some of our columns are not the right format so let's fix 'em all here
all.mkwd$Site <- as.factor(all.mkwd$Site)
all.mkwd$Management <- as.factor(all.mkwd$Management)
all.mkwd$Stocking <- factor(all.mkwd$Stocking, levels = c("None", "SLS", "IES"))
all.mkwd$GrazingLawn <- as.factor(all.mkwd$GrazingLawn)
str(all.mkwd)

# For ease of plotting, let's give our plotting the right aesthetics
all.mkwd$Grazing <- all.mkwd$Stocking
all.mkwd$Grazing <- gsub("IES", "Intensive Early", all.mkwd$Grazing)
all.mkwd$Grazing <- gsub("SLS", "Season Long", all.mkwd$Grazing)
all.mkwd$Grazing <- factor(all.mkwd$Grazing, levels = c("None", "Season Long", "Intensive Early"))
sort(unique(all.mkwd$Grazing))

# Give the summary of the df a quick once-over too
summary(all.mkwd)

# Subset the data to just no grazing sites
mkwd <- filter(all.mkwd, Grazing == "None")
sort(unique(mkwd$Grazing))

# Make any needed plotting aesthetics shared among graphs here
stk_colors <- c("None" = "#b2abd2", #"SLS" = "#fdb863", "IES" = "#b35806",
                "Season Long" = "#fdb863", "Intensive Early" = "#b35806")
mgmt_colors <- c("BO" = "#d73027", "GB" = "#e0f3f8", "PBG" = "#74add1")
std_color <- "#FFFFFF"
dodge <- position_dodge(width = 0.5)
pref_theme <- theme_classic() + theme(axis.text = element_text(size = 13),
                                      axis.title = element_text(size = 15),
                                      legend.position = "none",
                                      legend.title.align = 0.5,
                                      legend.background = element_blank())
axis_angle <- theme(axis.text.x = element_text(angle = 35, hjust = 1))

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
ggplot(mkwd.all.flr.stm, aes(x = TSF, y = Num.Stems.ALL.Flowering.Stages,
                                             fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Num.Stems.ALL.Flowering.Stages + se,
                    ymin = Num.Stems.ALL.Flowering.Stages - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Flowering Stems (all stages)") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Flowering-Stems_ANY-STAGE.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
        # Q2 - # stems that won't flower ####
## ------------------------------------------------ ##
# Summarize the response of interest
mkwd.noflr.stm <- summarySE(data = mkwd, measurevar = "Ratio.Flowering.vs.Total.Stems",
                              groupvars = c("TSF"), na.rm = T)
  ## And add a new dummy column to that
mkwd.noflr.stm$Dummy <- rep("x", nrow(mkwd.noflr.stm))

# Create the plot
ggplot(mkwd.noflr.stm, aes(x = TSF, y = Ratio.Flowering.vs.Total.Stems,
                             fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Ratio.Flowering.vs.Total.Stems + se,
                    ymin = Ratio.Flowering.vs.Total.Stems - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Flowering:Total Stems") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Ratio-Flowering-Total-Stems.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
          # Q3 - total buds + flowers ####
## ------------------------------------------------ ##
# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.bud.n.flr.stm <- summarySE(data = mkwd, measurevar = "Tot.Bud.n.Flr",
                            groupvars = c("TSF"), na.rm = T)
mkwd.bud.n.flr.stm$Dummy <- rep("x", nrow(mkwd.bud.n.flr.stm))

# Create the plot
ggplot(mkwd.bud.n.flr.stm, aes(x = TSF, y = Tot.Bud.n.Flr,
                           fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Bud.n.Flr + se,
                    ymin = Tot.Bud.n.Flr - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Buds & Flowers") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Buds-and-Flowers.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
    # Q4 - average height of 3 longest stems ####
## ------------------------------------------------ ##
# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.height <- summarySE(data = mkwd, measurevar = "Avg.Height",
                                groupvars = c("TSF"), na.rm = T)
mkwd.height$Dummy <- rep("x", nrow(mkwd.height))

# Create the plot
ggplot(mkwd.height, aes(x = TSF, y = Avg.Height,
                               fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Avg.Height + se,
                    ymin = Avg.Height - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Average Stem Length (cm)") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Avg-Length.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
          # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.tot.btn.stm <- summarySE(data = mkwd, measurevar = "Tot.Bitten.Stems",
                         groupvars = c("TSF"), na.rm = T)
mkwd.tot.btn.stm$Dummy <- rep("x", nrow(mkwd.tot.btn.stm))

# Create the plot
ggplot(mkwd.tot.btn.stm, aes(x = TSF, y = Tot.Bitten.Stems,
                        fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Bitten.Stems + se,
                    ymin = Tot.Bitten.Stems - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Bitten Stems") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Bitten-Stems.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
   # Q6 - ratio of bitten stems : total stems ####
## ------------------------------------------------ ##
# NOTE:
  ## The "total stems" included in this ratio was not directly measured as such
  ## HOWEVER, the total number of (1) flowering stems and (2) nonflowering stems *was* counted
  ## So I added those two together to get the total stems
  ## There were ~20 cases (of ~600) where the resulting ratio was greater than 1
    ### (i.e., more bitten stems were counted than flowering/nonflowering)
  ## And these cases were coerced into NAs to avoid messing with the data

# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.rat.btn.stm <- summarySE(data = mkwd, measurevar = "Ratio.Bitten.vs.Total.Stems",
                              groupvars = c("TSF"), na.rm = T)
mkwd.rat.btn.stm$Dummy <- rep("x", nrow(mkwd.rat.btn.stm))

# Create the plot
ggplot(mkwd.rat.btn.stm, aes(x = TSF, y = Ratio.Bitten.vs.Total.Stems,
                             fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Ratio.Bitten.vs.Total.Stems + se,
                    ymin = Ratio.Bitten.vs.Total.Stems - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Bitten:Total Stems") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Ratio-Bitten-Total-Stems.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.ax.sht <- summarySE(data = mkwd, measurevar = "Tot.Axillary.Shoots",
                              groupvars = c("TSF"), na.rm = T)
mkwd.ax.sht$Dummy <- rep("x", nrow(mkwd.ax.sht))

# Create the plot
ggplot(mkwd.ax.sht, aes(x = TSF, y = Tot.Axillary.Shoots,
                             fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Axillary.Shoots + se,
                    ymin = Tot.Axillary.Shoots - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "# Axillary Shoots") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Axillary-Shoots.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
            # Q8 - monarch immatures ####
## ------------------------------------------------ ##
# "Immatures" = larvae (caterpillars) + eggs
# Summarize the response of interest & make a dummy variable in the resulting dataframe
mkwd.mnrch <- summarySE(data = mkwd, measurevar = "Tot.Monarch.Immatures",
                         groupvars = c("TSF"), na.rm = T)
mkwd.mnrch$Dummy <- rep("x", nrow(mkwd.mnrch))

# Create the plot
ggplot(mkwd.mnrch, aes(x = TSF, y = Tot.Monarch.Immatures,
                        fill = Dummy, color = Dummy)) + 
  geom_smooth(method = 'lm', se = F) +
  geom_errorbar(aes(ymax = Tot.Monarch.Immatures + se,
                    ymin = Tot.Monarch.Immatures - se),
                width = 0.2) +
  geom_point(pch = 23, size = 3) +
  labs(x = "Time Since Fire (Years)", y = "Monarch Immatures") +
  scale_fill_manual(values = "#b2abd2") +
  scale_color_manual(values = 'black') +
  pref_theme

# Save it
ggsave("./Graphs/Ungrazed_Monarch-Immatures.pdf", plot = last_plot(), dpi = 600,
       width = 5, height = 5, units = "in")

## ------------------------------------------------ ##
 # Q9 - plant quality ~ nearby shrub abundance ####
## ------------------------------------------------ ##

# Only interested in this when also including the other management types
  ## Creating these plots (even for exploratory purposes) would incentivize eyeball tests of spurious relationships
  ## So the plots will remain un-done for now.

## ------------------------------------------------ ##
   # Supplemental - Neighboring Conspecifics ####
## ------------------------------------------------ ##

# Same as the shrub plots:
  ## We only care about this when all of the grazing types are included

# END ####
