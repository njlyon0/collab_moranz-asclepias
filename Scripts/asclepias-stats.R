## ----------------------------------------------------------------------- ##
      # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ----------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code performs all of the statistical tests in the paper
  ## and (presumably) some tests that didn't make it to the paper

# Clear the environment
rm(list = ls())

# Set the working directory
  ## Session -> Set Working Directory -> Choose Directory...
myWD <- getwd()
setwd(myWD)

# Call any needed libraries here (good to centralize this step)
library(readxl); library(psych); library(tidyverse); library(glmmTMB)

## ------------------------------------------------ ##
                # Housekeeping ####
## ------------------------------------------------ ##
# Read in data
mkwd.v0 <- as.data.frame(read_excel("./Data/Asclepias-TIDY.xlsx",
                      sheet = "Data", guess_max = 10000))
bfly.v0 <- as.data.frame(read_excel("./Data/Monarch-Adult-TIDY.xlsx",
                   sheet = "Data", guess_max = 10000))

# Check the structure
str(mkwd.v0)
str(bfly.v0)

# Some of our columns are not the right format so let's fix 'em all here
mkwd.v0$Site <- as.factor(mkwd.v0$Site)
mkwd.v0$Management <- as.factor(mkwd.v0$Management)
mkwd.v0$Stocking <- as.factor(mkwd.v0$Stocking)
mkwd.v0$GrazingLawn <- as.factor(mkwd.v0$GrazingLawn)
str(mkwd.v0)
bfly.v0$Site <- as.factor(bfly.v0$Site)
bfly.v0$Stocking.Type <- as.factor(bfly.v0$Stocking.Type)
str(bfly.v0)

# Give the summary of the df a quick once-over too
summary(mkwd.v0)
summary(bfly.v0)

# Now remove the TSF > 2
mkwd <- filter(mkwd.v0, TSF <= 2)
sort(unique(mkwd$TSF))
bfly <- filter(bfly.v0, TSF <= 2)
sort(unique(bfly$TSF))

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

# Do the same for the butterfly stocking column
bfly.lvl2 <- bfly
levels(bfly.lvl2$Stocking.Type)
bfly.lvl2$Stocking.Type <- factor(bfly.lvl2$Stocking.Type,
                            levels = c("None", "SLS", "IES"))
levels(bfly.lvl2$Stocking.Type)

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

# Run again with the other factor order
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

# If neither of those tests has a sig interaction term, re-run w/o
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF + Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

# Run again with the other factor order
summary(glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF + Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

## ------------------------------------------------ ##
     # Q2 - ratio of flowering : total stems ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(mkwd$Ratio.Flowering.vs.Total.Stems)

# Tests
summary(glmmTMB(Ratio.Flowering.vs.Total.Stems ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))

summary(glmmTMB(Ratio.Flowering.vs.Total.Stems ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))

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

summary(glmmTMB(Log.Tot.Bud.n.Flr ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))

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

summary(glmmTMB(Avg.Height ~ TSF * Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))

# Re-run without interaction term
summary(glmmTMB(Avg.Height ~ TSF + Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd, family = gaussian()))

summary(glmmTMB(Avg.Height ~ TSF + Stocking + (1|Year) + (1|Julian) + (1|Site),
                data = mkwd.lvl2, family = gaussian()))

## ------------------------------------------------ ##
        # Q5 - total # bitten stems ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)

# Tests
summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

summary(glmmTMB(Tot.Bitten.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

# Re-run without interaction
summary(glmmTMB(Tot.Bitten.Stems ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

summary(glmmTMB(Tot.Bitten.Stems ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

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

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))

# Re-run w/o interaction term
summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = binomial()))

summary(glmmTMB(Ratio.Bitten.vs.Total.Stems ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = binomial()))

## ------------------------------------------------ ##
        # Q7 - total # axillary shoots ####
## ------------------------------------------------ ##
# Distribution
multi.hist(mkwd$Tot.Axillary.Shoots)

# Tests
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF * Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

# Without interaction term
summary(glmmTMB(Tot.Axillary.Shoots ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

summary(glmmTMB(Tot.Axillary.Shoots ~ TSF + Stocking + (1|Julian) + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

## ------------------------------------------------ ##
           # Q8 - monarch immatures ####
## ------------------------------------------------ ##
# "Immatures" = larvae (caterpillars) + eggs

# Distribution check
multi.hist(mkwd$Tot.Monarch.Immatures)

## Took Julian out to let the model run with a poisson distribution
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

summary(glmmTMB(Tot.Monarch.Immatures ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))

# Run without interaction term
summary(glmmTMB(Tot.Monarch.Immatures ~ TSF + Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))

## ------------------------------------------------ ##
              # Q9 - monarch adults ####
## ------------------------------------------------ ##
# Distribution check
multi.hist(bfly$Monarch.Adults)

## Run the same model as monarch immatures
summary(glmmTMB(Monarch.Adults ~ TSF * Stocking.Type + (1|Year) + (1|Site),
                data = bfly, family = genpois()))

summary(glmmTMB(Monarch.Adults ~ TSF * Stocking.Type + (1|Year) + (1|Site),
                data = bfly.lvl2, family = genpois()))

# And again without interaction term
summary(glmmTMB(Monarch.Adults ~ TSF + Stocking.Type + (1|Year) + (1|Site),
                data = bfly, family = genpois()))

summary(glmmTMB(Monarch.Adults ~ TSF + Stocking.Type + (1|Year) + (1|Site),
                data = bfly.lvl2, family = genpois()))

## ------------------------------------------------ ##
 # Q10 - plant quality ~ nearby shrub abundance ####
## ------------------------------------------------ ##
# NOTE: This is (as of 5/24/21) the only variable with a different explanatory variable used

### Now, *do shrubs affect milkweeds?*
# Distribution
multi.hist(mkwd$Tot.Bitten.Stems)
multi.hist(mkwd$Ratio.Bitten.vs.Total.Stems)
multi.hist(mkwd$Avg.Height)
multi.hist(mkwd$Tot.Axillary.Shoots)

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
summary(glmmTMB(ASCTUB.Abun.1m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = sig (None ≠ IES, None vs. SLS = marginal)

### Number of conspecifics within 2 meters?
# Distribution
multi.hist(mkwd$ASCTUB.Abun.2m)

# Tests
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd, family = genpois()))
  ## Stocking = sig (IES ≠ None)
summary(glmmTMB(ASCTUB.Abun.2m ~ TSF * Stocking + (1|Year) + (1|Site),
                data = mkwd.lvl2, family = genpois()))
  ## Stocking = sig (IES ≠ None)

## ------------------------------------------------ ##
    # Q11 - Axillary Shoots ~ Bitten Stems ####
## ------------------------------------------------ ##
### Does the # of axillary shoots depend on the # of bitten stems?
# Distribution check
multi.hist(mkwd$Tot.Bitten.Stems)
multi.hist(mkwd$Tot.Axillary.Shoots)

# Run the test
summary(glmmTMB(Tot.Axillary.Shoots ~ Tot.Bitten.Stems,
                data = mkwd, family = genpois()))

# END ####

