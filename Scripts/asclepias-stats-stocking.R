## ---------------------------------------------------------------------------------------- ##
              # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code analyzes essentially all of the variables against 'stocking rate' x 'time since fire'

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/_Publications/2021_Moranz_Asclepias/Moranz-AsclepiasCollab")
  ## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse); library(glmmTMB)

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
mkwd$Stocking <- as.factor(mkwd$Stocking)
mkwd$GrazingLawn <- as.factor(mkwd$GrazingLawn)
str(mkwd)

# Give the summary of the df a quick once-over too
summary(mkwd)

# Okay, if we want to get all of our pairwise comparisons we need to change the factor levels
  ## The first level gets 'sucked into' the intercept so we need a second version of the data
  ## Where a different factor level is "first"
mkwd.lvl2 <- mkwd
levels(mkwd.lvl2$Stocking)
levels(mkwd.lvl2$Management)
mkwd.lvl2$Stocking <- factor(mkwd.lvl2$Stocking, levels = c("None", "SLS", "IES"))
mkwd.lvl2$Management <- factor(mkwd.lvl2$Management, levels = c("GB", "PBG", "BO"))
levels(mkwd.lvl2$Stocking)
levels(mkwd.lvl2$Management)
  ## So, to get all three comparisons you just run the same test once with each version of the data

## ------------------------------------------------ ##
         # Plant Characteristics Tests ####
## ------------------------------------------------ ##
# Let's see whether Time Since Fire (TSF) and Stocking treatment (and their interaction)
  ## Affect any part of the focal plant's characteristics

# For each test, check the distribution of the data, transform if necessary, then run the test

# Average height affected?
  ## Check distribution
multi.hist(mkwd$Avg.Height)
  ## Run test with both level orderings
summary(glmmTMB(Avg.Height ~ TSF * Stocking + Year + (1|Site), data = mkwd, family = gaussian()))
summary(glmmTMB(Avg.Height ~ TSF * Stocking + Year + (1|Site), data = mkwd.lvl2, family = gaussian()))
  ## stocking = sig
  ## IES ≠ SLS/None

# Average number of buds? (on the three longest stems)
  ## Data are heavily skewed, try log transformation
multi.hist(mkwd$Avg.Bud)
multi.hist(log(mkwd$Avg.Bud))
mkwd$Log.Avg.Bud <- log(mkwd$Avg.Bud + 1)
mkwd.lvl2$Log.Avg.Bud <- log(mkwd.lvl2$Avg.Bud + 1)
  ## Using log transformed average bud number
summary(glmmTMB(Log.Avg.Bud ~ TSF * Stocking + Year + (1|Site), data = mkwd, family = gaussian()))
summary(glmmTMB(Log.Avg.Bud ~ TSF * Stocking + Year + (1|Site), data = mkwd.lvl2, family = gaussian()))
  ## year = sig & TSF*SLS = sig

# Average number of flowers? (on the three longest stems)
  ## Data are heavily skewed, try log transformation
multi.hist(mkwd$Avg.Flr)
multi.hist(log(mkwd$Avg.Flr))
mkwd$Log.Avg.Flr <- log(mkwd$Avg.Flr + 1)
mkwd.lvl2$Log.Avg.Flr <- log(mkwd.lvl2$Avg.Flr + 1)
  ## Using log transformed average bud number
summary(glmmTMB(Log.Avg.Flr ~ TSF * Stocking + Year + (1|Site), data = mkwd, family = gaussian()))
summary(glmmTMB(Log.Avg.Flr ~ TSF * Stocking + Year + (1|Site), data = mkwd.lvl2, family = gaussian()))
  ## year = sig

# Total number of buds
  ## Distribution is too skewed for possion dist. to work (I tried)
multi.hist(mkwd$Tot.Bud)
multi.hist(log(mkwd$Tot.Bud))
mkwd$Log.Tot.Bud <- log(mkwd$Tot.Bud + 1)
mkwd.lvl2$Log.Tot.Bud <- log(mkwd.lvl2$Tot.Bud + 1)
  ## Using log transformed average bud number
summary(glmmTMB(Log.Tot.Bud ~ TSF * Stocking + Year + (1|Site), data = mkwd, family = gaussian()))
summary(glmmTMB(Log.Tot.Bud ~ TSF * Stocking + Year + (1|Site), data = mkwd.lvl2, family = gaussian()))
  ## year = sig & TSF*SLS = sig

# Total flower distribution
  ## Distribution is too skewed for possion dist. to work (I tried)
multi.hist(mkwd$Tot.Flr)
multi.hist(log(mkwd$Tot.Flr))
mkwd$Log.Tot.Flr <- log(mkwd$Tot.Flr + 1)
mkwd.lvl2$Log.Tot.Flr <- log(mkwd.lvl2$Tot.Flr + 1)
  ## Using log transformed average bud number
summary(glmmTMB(Log.Tot.Flr ~ TSF * Stocking + Year + (1|Site),
                data = mkwd, family = gaussian()))
summary(glmmTMB(Log.Tot.Flr ~ TSF * Stocking + Year + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## year = sig

# Number of stems of any flowering stage (bud, flower, postflower)
multi.hist(mkwd$Num.Stems.ALL.Flowering.Stages)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Site),
                data = mkwd, family = genpois()))
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = sig
  ## IES ≠ SLS/None

# Total axillary shoots
multi.hist(mkwd$Tot.Axillary.Shoots)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
  ## Significant interaction term!
  ## TSF*None ≠ TSF*IES/TSF*SLS

# Left for later (i.e., unanalyzed as of May 2021 but intending to analyze down the line)
multi.hist(mkwd$Avg.Bloom.Status)
multi.hist(mkwd$Num.Stems.Budding)
multi.hist(mkwd$Num.Stems.Flowering)
multi.hist(mkwd$Num.Stems.PostFlower)
multi.hist(mkwd$Num.Stems.Nonflowering)

## ------------------------------------------------ ##
            # Neighboring Plant Tests ####
## ------------------------------------------------ ##
# Number of conspecifics within 1 meter?
multi.hist(mkwd$ASCTUB.Abun.1m)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
 ## significant interaction terms across the board!

# Number of conspecifics within 2 meters?
multi.hist(mkwd$ASCTUB.Abun.2m)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
  ## significant interaction terms again!

# Number of shrubs within 1 meter?
multi.hist(mkwd$Shrub.Abun.1m)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
  ## Stocking = sig  
  ## SLS ≠ IES

## ------------------------------------------------ ##
               # Herbivory Tests ####
## ------------------------------------------------ ##
# Total bitten stems
  ## Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
  ## TSF*Stocking interaction (all of them)

# Number of axillary shoots bitten
  ## Distribution check
multi.hist(mkwd$Num.Axillary.Shoots.Bitten)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Num.Axillary.Shoots.Bitten ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Num.Axillary.Shoots.Bitten ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
  ## NS

# Number of bitten stems with axillary shoots
  ## Distribution check
multi.hist(mkwd$Num.Bitten.Stems.w.Axillary.Shoots)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Num.Bitten.Stems.w.Axillary.Shoots ~ TSF * Stocking + (1|Site),
                data = mkwd, family = genpois()))
summary(glmmTMB(Num.Bitten.Stems.w.Axillary.Shoots ~ TSF * Stocking + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF*Stocking
  ## TSF*None ≠ TSF*SLS/IES

# Not yet analyzed
multi.hist(mkwd$Num.Flowering.Stems.Bitten)
multi.hist(mkwd$Num.Unbit.Stems.w.Axillary.Shoots)

## ------------------------------------------------ ##
              # Invertebrate Tests ####
## ------------------------------------------------ ##
# Number of monarch immatures (eggs + larvae)
  ## Distribution check
multi.hist(mkwd$Tot.Monarch.Immatures)
  ## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Site),
                data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## NS

# Unanalyzed (so far)
multi.hist(mkwd$Num.Monarch.Eggs)
multi.hist(mkwd$Num.Monarch.Larvae)

# And model convergence issues for the following
  ## May be related to distribution choice
multi.hist(mkwd$Crab.Spider.Abun)
multi.hist(mkwd$Nectaring.Bfly.Abun)
multi.hist(mkwd$Nectaring.Bfly.Rich)
multi.hist(mkwd$Monarch.Immature.Evidence)


# END ####

