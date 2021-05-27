## ---------------------------------------------------------------------------------------- ##
              # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code tests the eleven variables that Ray requested

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/_Publications/2022_Moranz_Asclepias/Moranz-AsclepiasCollab")
  ## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse); library(glmmTMB)

## ------------------------------------------------ ##
                # Housekeeping ####
## ------------------------------------------------ ##
# Read in data
mkwd <- read_excel("./Data/Asclepias-TIDY.xlsx", sheet = "Data", guess_max = 10000)

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

# We are now ready to do the analyses!

# For each test,
  ## 1 - check the distribution of the data
  ## 2 - transform if necessary
  ## 3 - then run the test

## ------------------------------------------------ ##
      # Q1 - # stems of any flowering stage ####
## ------------------------------------------------ ##
### Number of stems with buds + flowers + dead flowers
# Distribution check
multi.hist(mkwd$Num.Stems.ALL.Flowering.Stages)

# Tests
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None/SLS)
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig
  ## Stocking = sig (None ≠ IES)
  ## Ixn = marginal (TSF*SLS p = 0.093)

## ------------------------------------------------ ##
      # Q2 - # stems that won't flower ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(mkwd$Num.Stems.Nonflowering)


# ADD TEST


## ------------------------------------------------ ##
          # Q3 - total buds + flowers ####
## ------------------------------------------------ ##
### Total flower & bud distribution
# Distribution?
multi.hist(mkwd$Tot.Bud.n.Flr)
multi.hist(log(mkwd$Tot.Bud.n.Flr))
mkwd$Log.Tot.Bud.n.Flr <- log(mkwd$Tot.Bud.n.Flr + 1)
mkwd.lvl2$Log.Tot.Bud.n.Flr <- log(mkwd.lvl2$Tot.Bud.n.Flr + 1)

## Using log transformed total buds/flowers number as response
summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
  ## Stocking = sig (SLS ≠ IES sig | SLS - None marginal)
summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## TSF = sig
  ## Stocking = marginal (None vs. IES p = 0.056)
  ## Ixn = marginal (TSF*SLS p = 0.081)

## ------------------------------------------------ ##
   # Q4 - average height of 3 longest stems ####
## ------------------------------------------------ ##
### Average height
# Check distribution
multi.hist(mkwd$Avg.Height)
  ## Surprisingly good fit for the normal distribution (at least relative to other raw data)

# Run tests
summary(glmmTMB(Avg.Height ~ TSF * Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd, family = gaussian()))
  ## Stocking = sig (IES ≠ None/SLS)
summary(glmmTMB(Avg.Height ~ TSF * Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## Stocking = sig (None ≠ IES)

## ------------------------------------------------ ##
        # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
### Total bitten stems
# Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)

# Tests
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ SLS)
  ## Ixn = sig (TSF*None)
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig
  ## Ixn = all of them

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



# DOUBLE CHECK THIS VARIABLE (esp. "Tot.Bitten.Stems")
  ## Ray says there should be far fewer NAs


### Bitten stems:Total stems
# Distribution
multi.hist(mkwd$Ratio.Bitten.vs.Total.Stems)
  ## ranges from 0 to 1

# Test
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## Stocking = marginal (IES vs. SLS p = 0.051)

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))
  ## Stocking = marginal (TSF*SLS p = 0.0998)

## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
### Total axillary shoots
# Distribution
multi.hist(mkwd$Tot.Axillary.Shoots)

# Tests
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Ixn = sig (TSF*None)

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig
  ## Ixn = sig (all!)

## ------------------------------------------------ ##
           # Q8 - monarch immatures ####
## ------------------------------------------------ ##
### Number of monarch immatures (eggs + larvae)

# Distribution check
multi.hist(mkwd$Tot.Monarch.Immatures)

## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## NS

summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## NS

### Monarch evidence (0 or 1)
# Distribution check
multi.hist(mkwd$Monarch.Immature.Evidence)

# Test
summary(glmmTMB(Monarch.Immature.Evidence ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))

summary(glmmTMB(Monarch.Immature.Evidence ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))

## ------------------------------------------------ ##
 # Q9 - plant quality ~ nearby shrub abundance ####
## ------------------------------------------------ ##
# NOTE: This is (as of 5/24/21) the only variable with a different explanatory variable used
  ## Also, this is the only variable containing two (related) tests of the same column

### Is the number of shrubs within 1 meter *affected by* TSF/Stocking?
# Distribution
multi.hist(mkwd$Shrub.Abun.1m)

# Test
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ SLS)

summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## NS

### Now, *do shrubs affect milkweeds?*
# Distribution
multi.hist(mkwd$Tot.Bitten.Stems)
multi.hist(mkwd$Ratio.Bitten.vs.Total.Stems)
multi.hist(mkwd$Avg.Height)
multi.hist(mkwd$Tot.Axillary.Shoots)

# Tests
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## NS

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## NS

summary(glmmTMB(Avg.Height ~  TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
  ## TSF = marginal (p = 0.079)

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## NS


# Do shrubs alone affect those characteristics?

# ADD ME


## ------------------------------------------------ ##
           # Neighboring Conspecifics ####
## ------------------------------------------------ ##
# NOTE: This is a 'just in case' set of tests
  ## Fewer nearby conspecifics means greater odds we actually have data from the same plants

### Number of conspecifics within 1 meter?
# Distribution
multi.hist(mkwd$ASCTUB.Abun.1m)

# Tests
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None)
  ## Ixn = sig (TSF*None)
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## everything sig!

### Number of conspecifics within 2 meters?
# Distribution
multi.hist(mkwd$ASCTUB.Abun.2m)

# Tests
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None)
  ## Ixn = sig (TSF*None)
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Everything sig


# END ####

