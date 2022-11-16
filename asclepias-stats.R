## ---------------------------------------------------------- ##
         # Moranz et al. Asclepias tuberosa Project
## ---------------------------------------------------------- ##
# Code written by Nick J Lyon

# PURPOSE ----
## This code performs all of the statistical tests in the paper

## ------------------------------------------------ ##
                # Housekeeping ----
## ------------------------------------------------ ##

# Call any needed libraries here (good to centralize this step)
# install.packages("librarian")
librarian::shelf(tidyverse, njlyon0/helpR, psych, lme4)

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

# Okay, if we want to get all of our pairwise comparisons we need to change the factor levels
## The first level gets 'sucked into' the intercept so we need a second version of the data where a different factor level is "first"
mkwd_lvl2 <- mkwd %>%
  dplyr::mutate(
    Stocking.Type = factor(Stocking.Type, levels = c("None", "SLS", "IES")),
    Management.Method = factor(Management.Method,
                               levels = c("GB", "PBG", "BO")))

# Glimpse this
dplyr::glimpse(mkwd_lvl2)

# Check factor levels
levels(mkwd$Management.Method); levels(mkwd_lvl2$Management.Method)

## ------------------------------------------------ ##
              # Q2 - # Bitten Stems ----
## ------------------------------------------------ ##
# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Tot.Bitten.Stems))
mkwd_lvl2_sub <- mkwd_lvl2 %>%
  dplyr::filter(!is.na(Tot.Bitten.Stems))

# Distribution check
psych::multi.hist(mkwd_sub$Tot.Bitten.Stems)

# Check whether interaction is significant
summary(lme4::glmer(Tot.Bitten.Stems ~ TSF * Stocking.Type + 
              (1|Julian) + (1|Year) + (1|Site),
            data = mkwd_sub, family = "poisson"))

## For both factor levels
summary(lme4::glmer(Tot.Bitten.Stems ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "poisson"))

# If not, re-run without interaction term
summary(lme4::glmer(Tot.Bitten.Stems ~ TSF + Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "poisson"))

## For both factor levels
summary(lme4::glmer(Tot.Bitten.Stems ~ TSF + Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "poisson"))

## ------------------------------------------------ ##
            # Q3 - # Flowering Stems ----
## ------------------------------------------------ ##
# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Num.Stems.ALL.Flowering.Stages))
mkwd_lvl2_sub <- mkwd_lvl2 %>%
  dplyr::filter(!is.na(Num.Stems.ALL.Flowering.Stages))

# Distribution check
psych::multi.hist(mkwd_sub$Num.Stems.ALL.Flowering.Stages)

# Check whether interaction is significant
summary(lme4::glmer(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "poisson"))

## For both factor levels
summary(lme4::glmer(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "poisson"))

# If not, re-run without interaction term
summary(lme4::glmer(Num.Stems.ALL.Flowering.Stages ~ TSF + Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "poisson"))

## For both factor levels
summary(lme4::glmer(Num.Stems.ALL.Flowering.Stages ~ TSF + Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "poisson"))

## ------------------------------------------------ ##
    # Q4 - Ratio of Flowering : Total Stems ----
## ------------------------------------------------ ##
# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Ratio.Flowering.vs.Total.Stems))
mkwd_lvl2_sub <- mkwd_lvl2 %>%
  dplyr::filter(!is.na(Ratio.Flowering.vs.Total.Stems))

# Distribution check
psych::multi.hist(mkwd_sub$Ratio.Flowering.vs.Total.Stems)

# Check whether interaction is significant
summary(lme4::glmer(Ratio.Flowering.vs.Total.Stems ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "binomial"))

## For both factor levels
summary(lme4::glmer(Ratio.Flowering.vs.Total.Stems ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "binomial"))

## ------------------------------------------------ ##
            # Q5 - # Buds & Flowers ----
## ------------------------------------------------ ##
# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Tot.Bud.n.Flr))
mkwd_lvl2_sub <- mkwd_lvl2 %>%
  dplyr::filter(!is.na(Tot.Bud.n.Flr))

# Distribution check
psych::multi.hist(mkwd_sub$Tot.Bud.n.Flr)

# Check whether interaction is significant
summary(lme4::glmer(Tot.Bud.n.Flr ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "poisson"))

## For both factor levels
summary(lme4::glmer(Tot.Bud.n.Flr ~ TSF * Stocking.Type + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_lvl2_sub, family = "poisson"))

## ------------------------------------------------ ##
      # Q6 - Ratio Bitten : Total Stems ----
## ------------------------------------------------ ##
# Drop missing values
mkwd_sub <- mkwd %>%
  dplyr::filter(!is.na(Ratio.Bitten.vs.Total.Stems) &
                  !is.na(Shrub.Abun.1m))

# Distribution check
psych::multi.hist(mkwd_sub$Ratio.Bitten.vs.Total.Stems)

# Check result of single explanatory var
summary(lme4::glmer(Ratio.Bitten.vs.Total.Stems ~ Shrub.Abun.1m + 
                      (1|Julian) + (1|Year) + (1|Site),
                    data = mkwd_sub, family = "binomial"))

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

# End ----


## ------------------------------------------------ ##
      # Q1 - # stems of any flowering stage ####
## ------------------------------------------------ ##
### Number of stems with buds + flowers + dead flowers
# Distribution check
psych::multi.hist(mkwd$Num.Stems.ALL.Flowering.Stages)

# Tests
summary(glmmTMB::glmmTMB(Num.Stems.ALL.Flowering.Stages ~ TSF * Stocking.Type + (1|Year) + (1|Site),
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

