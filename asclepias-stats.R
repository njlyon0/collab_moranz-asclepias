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
              # Sample Size Check ----
## ------------------------------------------------ ##
# Want to know number of plants per year / stocking type
mkwd %>%
  dplyr::group_by(TSF, Stocking.Type) %>%
  dplyr::summarize(plant_num = dplyr::n()) %>%
  dplyr::ungroup()

## ------------------------------------------------ ##
              # Q1 - # Bitten Stems ----
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
            # Q2 - # Flowering Stems ----
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
    # Q3 - Ratio of Flowering : Total Stems ----
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
            # Q4 - # Buds & Flowers ----
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
      # Q5 - Ratio Bitten : Total Stems ----
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
            # Q6 - Monarch Immatures ----
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
