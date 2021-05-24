## ---------------------------------------------------------------------------------------- ##
              # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code tests the eleven variables that Ray requested

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

# We are now ready to do the analyses!

# For each test,
  ## 1 - check the distribution of the data
  ## 2 - transform if necessary
  ## 3 - then run the test

## ------------------------------------------------ ##
   # Q1 & 2 - # stems of each flowering stage ####
## ------------------------------------------------ ##

multi.hist(mkwd$Num.Stems.Budding)
multi.hist(mkwd$Num.Stems.Flowering)
multi.hist(mkwd$Num.Stems.PostFlower)



# Number of stems of any flowering stage (bud, flower, postflower)
multi.hist(mkwd$Num.Stems.ALL.Flowering.Stages)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Site),
                data = mkwd, family = genpois()))
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Site),
                data = mkwd.lvl2, family = genpois()))
## Stocking = sig

## ------------------------------------------------ ##
          # Q3 - total buds/flowers ####
## ------------------------------------------------ ##
# Total flower & bud distribution

# Distribution?
multi.hist(mkwd$Tot.Bud.n.Flr)
multi.hist(log(mkwd$Tot.Bud.n.Flr))
mkwd$Log.Tot.Bud.n.Flr <- log(mkwd$Tot.Bud.n.Flr + 1)
mkwd.lvl2$Log.Tot.Bud.n.Flr <- log(mkwd.lvl2$Tot.Bud.n.Flr + 1)
## Using log transformed average bud number
summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + Year + (1|Site),
                data = mkwd, family = gaussian()))
summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + Year + (1|Site),
                data = mkwd.lvl2, family = gaussian()))
## year = sig


## ------------------------------------------------ ##
   # Q4 - average height of 3 longest stems ####
## ------------------------------------------------ ##
# Check distribution
multi.hist(mkwd$Avg.Height)

# Run test with both level orderings
summary(glmmTMB(Avg.Height ~ TSF * Stocking + Year + (1|Site), data = mkwd, family = gaussian()))
  ## stocking = sig
  ## IES ≠ SLS/None

summary(glmmTMB(Avg.Height ~ TSF * Stocking + Year + (1|Site), data = mkwd.lvl2, family = gaussian()))
  ## stocking = sig
  ## None ≠ IES (SLS = not diff)

## ------------------------------------------------ ##
        # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
# Total bitten stems
## Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
## TSF*Stocking interaction (all of them)


## ------------------------------------------------ ##
  # Q6 - ratio of bitten stems : total stems ####
## ------------------------------------------------ ##
sort(unique(mkwd$Ratio.Bitten.vs.Total.Stems))




## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
# Total axillary shoots
multi.hist(mkwd$Tot.Axillary.Shoots)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
## Significant interaction term!
## TSF*None ≠ TSF*IES/TSF*SLS

## ------------------------------------------------ ##
           # Q8 - monarch immatures ####
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

## ------------------------------------------------ ##
 # Q9 - plant quality ~ nearby shrub abundance ####
## ------------------------------------------------ ##
# NOTE: This is (as of 5/24/21) the only variable with a different explanatory variable set

# Number of shrubs within 1 meter?
multi.hist(mkwd$Shrub.Abun.1m)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Shrub.Abun.1m ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
## Stocking = sig  
## SLS ≠ IES



# NOTE: This (vvv) will be the response var and this (^^^) will be the explanatory



# Total bitten stems
## Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)
## Took year out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd, family = genpois()))
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Site), data = mkwd.lvl2, family = genpois()))
## TSF*Stocking interaction (all of them)





## ------------------------------------------------ ##
           # Neighboring Conspecifics ####
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

# END ####

