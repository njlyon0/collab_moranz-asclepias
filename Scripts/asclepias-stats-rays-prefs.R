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
  ## Stocking = marginal (SLS vs. IES p = 0.054)

summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig

## ------------------------------------------------ ##
      # Q2 - # stems that won't flower ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(mkwd$Num.Stems.Nonflowering)

# Tests
summary(glmmTMB(Num.Stems.Nonflowering ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ SLS/None)
  ## Ixn = sig (TSF*None)

summary(glmmTMB(Num.Stems.Nonflowering ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig
  ## Stocking = sig (None ≠ SLS/IES)
  ## Ixn = sig (TSF*IES)

## ------------------------------------------------ ##
          # Q3 - total buds + flowers ####
## ------------------------------------------------ ##
# Distribution?
multi.hist(mkwd$Tot.Bud.n.Flr)
multi.hist(log(mkwd$Tot.Bud.n.Flr))
mkwd$Log.Tot.Bud.n.Flr <- log(mkwd$Tot.Bud.n.Flr + 1)
mkwd.lvl2$Log.Tot.Bud.n.Flr <- log(mkwd.lvl2$Tot.Bud.n.Flr + 1)

## Using log transformed total buds/flowers number as response
summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
  ## All sig! (TSF, Stocking, and Ixn!)

summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## TSF = sig
  ## Stocking = sig (None ≠ IES)
  ## Ixn = sig (TSF*IES)

## ------------------------------------------------ ##
   # Q4 - average height of 3 longest stems ####
## ------------------------------------------------ ##
# Check distribution
multi.hist(mkwd$Avg.Height)
  ## Surprisingly good fit for the normal distribution (at least relative to other raw data)
  ## I guess it makes sense given that the variable is height

# Run tests
summary(glmmTMB(Avg.Height ~ TSF * Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd, family = gaussian()))
  ## Stocking = sig (IES ≠ None/SLS)

summary(glmmTMB(Avg.Height ~ TSF * Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## TSF = sig
  ## Stocking = sig (None ≠ IES)
  ## Ixn = TSF*SLS

## ------------------------------------------------ ##
        # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)

# Tests
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## TSF = sig
  ## Stocking = sig (IES vs. None = marginal | IES vs. SLS = sig)

summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = marginal (None vs. IES p = 0.056)

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

# Distribution
multi.hist(mkwd$Ratio.Bitten.vs.Total.Stems)
  ## ranges from 0 to 1

# Test
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## Stocking = sig (IES ≠ SLS/None)

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))
  ## Stocking = sig (None ≠ IES)

## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
# Distribution
multi.hist(mkwd$Tot.Axillary.Shoots)

# Tests
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = marginal (IES vs. None p = 0.056 | IES vs. SLS p = 0.051)

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = marginal (None vs. IES p = 0.056)

## ------------------------------------------------ ##
           # Q8 - monarch immatures ####
## ------------------------------------------------ ##
# "Immatures" = larvae (caterpillars) + eggs

# Distribution check
multi.hist(mkwd$Tot.Monarch.Immatures)

## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = marginal (IES vs. None p = 0.056)
  ## Ixn = marginal (TSF*SLS p = 0.091)

summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = marginal (None vs. IES p = 0.056 | None vs. SLS p = 0.058)
  ## Ixn = marginal (TSF*SLS p = 0.096)

### Monarch evidence (0 or 1)
# Distribution check
multi.hist(mkwd$Monarch.Immature.Evidence)

# Test
summary(glmmTMB(Monarch.Immature.Evidence ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## Stocking = marginal (IES vs. None p = 0.058)

summary(glmmTMB(Monarch.Immature.Evidence ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))
  ## Stocking = marginal (None vs. SLS p = 0.056 | None vs. IES p = 0.058)

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
  ## Stocking = marginal (IES ≠ SLS p = 0.0501) <- basically sig

summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## NS

### Now, *do shrubs affect milkweeds?*
# Distribution
multi.hist(mkwd$Tot.Bitten.Stems)
multi.hist(mkwd$Ratio.Bitten.vs.Total.Stems)
multi.hist(mkwd$Avg.Height)
multi.hist(mkwd$Tot.Axillary.Shoots)

### Total bitten stems
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ SLS)

summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## NS

### Ratio bitten vs. total stems
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## Stocking = sig (IES ≠ None/SLS)

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))
  ## Stocking = sig (None ≠ IES)

### Average height of three longest stems
summary(glmmTMB(Avg.Height ~  TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
  ## Stocking = sig (IES ≠ None)

summary(glmmTMB(Avg.Height ~  TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
  ## Stocking = sig & marginal (None vs. IES p = 0.016 | None vs. SLS p = 0.0504) <- basically sig
  ## Ixn = marginal (TSF*SLS p = 0.057)

### Total axillary shoots
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None/SLS)

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking * Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = sig & marginal (None vs. SLS p = 0.077 | None vs. IES p = 0.010)
  ## Stocking * Shrubs ixn = marginal! (SLS*Shrubs p = 0.072)

### Finally, do **shrubs alone** affect those characteristics?

### Total bitten stems
summary(glmmTMB(Tot.Bitten.Stems ~ Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Shrubs = sig

### Ratio bitten vs. total stems
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))
  ## Shrubs = sig

### Average height of three longest stems
summary(glmmTMB(Avg.Height ~  Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = gaussian()))
  ## NS

### Total axillary shoots
summary(glmmTMB(Tot.Axillary.Shoots ~ Shrub.Abun.1m + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Shrubs = sig

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
  ## Ixn = marginal (TSF*None p = 0.061)
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = sig
  ## Stocking = sig (None ≠ IES/SLS)
  ## Ixn = sig & marginal (TSF*IES p = 0.061 | TSF*SLS p = 0.048)

### Number of conspecifics within 2 meters?
# Distribution
multi.hist(mkwd$ASCTUB.Abun.2m)

# Tests
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None)
  ## Ixn = marginal (TSF*None p = 0.089)
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## TSF = marginal (p = 0.056)
  ## Stocking = sig (None ≠ IES)
  ## Ixn = marginal (TSF*SLS p = 0.095 | TSF*IES p = 0.089)

# END ####

