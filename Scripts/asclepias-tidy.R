## ---------------------------------------------------------------------------------------- ##
                        # Moranz et al. Butterfly Milkweed Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code tidys the raw data into a format that is ready for analysis and figure construction

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/_Publications/2021_Moranz_Asclepias/Moranz-AsclepiasCollab")
  ## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(tidyverse); library(stringr)

## ------------------------------------------------ ##
                # 2012 Tidying ####
## ------------------------------------------------ ##
# Before we can get to tidying in earnest, it will be best to join 2012 to the other years

# Read in the data
mkwd.12.v0 <- read_excel("./Data/Asclepias-2012-RAW.xlsx", sheet = "MAIN Trimbled minus not 3yrs")

# Take a look at the data
str(mkwd.12.v0)
summary(mkwd.12.v0)
names(mkwd.12.v0)
  ## Oof that is rough

# Let's make this more manageable by slicing down now to only the columns that we want
  ## Identify what's in there now
names(mkwd.12.v0)
  ## Prune and re-order based on what we want
mkwd.12.v1 <- mkwd.12.v0 %>%
  select(Year, Date, Pasture, PatchWhit, `2012PlantIDCode`, PlantNumAuto,
         `Length zAVERAGE`, `# buds zAVERAGE`, `# flowers zAVERAGE`,
         `# buds TOTAL`, `# flowers TOTAL`, `bloom status zAVERAGE`) %>%
  rename(Year = Year, Date = Date, Site = Pasture, Patch = PatchWhit,
         Plant.ID = `2012PlantIDCode`, Plant.Num = PlantNumAuto,
         Avg.Height = `Length zAVERAGE`, Avg.Bud = `# buds zAVERAGE`, Avg.Flr = `# flowers zAVERAGE`,
         Tot.Bud = `# buds TOTAL`, Tot.Flr = `# flowers TOTAL`, Avg.Bloom.Status = `bloom status zAVERAGE`)

# How's that look?
names(mkwd.12.v1)
str(mkwd.12.v1)
  ## Much more manageable

## ------------------------------------------------ ##
            # 2013-16 Data Tidying ####
## ------------------------------------------------ ##
# Read in the data
  ## The below data is a .csv file directly (unmodified) from the Excel sheet "Plants"
  ## Much of that data is numbers stored as text so the simpler csv file actually preserves more data
  ## Something about "read_excel()" deletes those numbers stored as text
mkwd.13.16.v0 <- read.csv("./Data/Asclepias-2016-RAW-plants.csv")
mkwd.13.16.meta <- read_excel("./Data/Asclepias-2016-RAW.xlsx", sheet = "Metadata")

# Maybe 2013-16 will be cleaner?
str(mkwd.13.16.v0)
summary(mkwd.13.16.v0)
names(mkwd.13.16.v0)
  ## Not really... time for a pivot in approach

# To combine 2012 with the other years we first need to get 2013-16 into a single datafile
  ## The legacy of MS Access lives on in it's current form
mkwd.13.16.meta$AsclepTransID <- mkwd.13.16.meta$AsclepTransIDauto
mkwd.13.16.v1 <- left_join(mkwd.13.16.v0, mkwd.13.16.meta, by = "AsclepTransID")
names(mkwd.13.16.v1)

# Now prune back to just what we need from this dataset
names(mkwd.13.16.v1)
mkwd.13.16.v2 <- mkwd.13.16.v1 %>%
    ## Needed columns
  select(YearVis, Date, Pasture, Patch, Whittaker,
         Length.St1:LengthSt3, Buds.St.1:Buds.St.3, Flow.St.1:Flow.St.3,
         BlooStatus.St.1:BlooStatus.St.3, Monarch.immatures2., total...bitten.stems)
names(mkwd.13.16.v2)

# Have some tidying to do before we can combine 2012 and 2013-16
  ## We need averages of all of the 'stems 1-3' variables
  ## To get that we need to ensure that those columns are all numbers so that they can BE averaged

# The year column is missing 2013 (but the date column isn't!)
mkwd.13.16.v2$Year <- as.factor(str_sub(mkwd.13.16.v2$Date, 1, 4))
unique(mkwd.13.16.v2$Year)

# Stem length checks
  ## Stem 1
sort(unique(mkwd.13.16.v2$Length.St1))
mkwd.13.16.v2$Length.St1 <- gsub("no data|not meas", "", mkwd.13.16.v2$Length.St1)
mkwd.13.16.v2$Length.St1 <- as.numeric(gsub("\\(dead\\)", "", mkwd.13.16.v2$Length.St1))
sort(unique(mkwd.13.16.v2$Length.St1))

  ## Stem 2
sort(unique(mkwd.13.16.v2$LengthSt2))
mkwd.13.16.v2$LengthSt2 <- as.numeric(gsub("not meas", "", mkwd.13.16.v2$LengthSt2))
sort(unique(mkwd.13.16.v2$LengthSt2))

  ## Stem 3
sort(unique(mkwd.13.16.v2$LengthSt3))
mkwd.13.16.v2$LengthSt3 <- as.numeric(gsub("not meas", "", mkwd.13.16.v2$LengthSt3))
sort(unique(mkwd.13.16.v2$LengthSt3))

# Get the average length of the stems
mkwd.13.16.v2$Avg.Height <- rowMeans(select(mkwd.13.16.v2, Length.St1:LengthSt3))
sort(unique(mkwd.13.16.v2$Avg.Height))

# Bud number checks
  ## Stem 1
sort(unique(mkwd.13.16.v2$Buds.St.1))
mkwd.13.16.v2$Buds.St.1 <- gsub("no data|not counted|not recorded|too early|tiny",
                                "", mkwd.13.16.v2$Buds.St.1)
  ### Note the judgement call here
mkwd.13.16.v2$Buds.St.1 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Buds.St.1))
sort(unique(mkwd.13.16.v2$Buds.St.1))

  ## Stem 2
sort(unique(mkwd.13.16.v2$Buds.St.2))
mkwd.13.16.v2$Buds.St.2 <- gsub("not counted|to early|too early|tiny", "", mkwd.13.16.v2$Buds.St.2)
mkwd.13.16.v2$Buds.St.2 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Buds.St.2))
sort(unique(mkwd.13.16.v2$Buds.St.2))

  ## Stem 3
sort(unique(mkwd.13.16.v2$Buds.St.3))
mkwd.13.16.v2$Buds.St.3 <- gsub("not counted|too early|\\?", "", mkwd.13.16.v2$Buds.St.3)
mkwd.13.16.v2$Buds.St.3 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Buds.St.3))
sort(unique(mkwd.13.16.v2$Buds.St.3))

# Bud summarization
mkwd.13.16.v2$Tot.Bud <- rowSums(select(mkwd.13.16.v2, Buds.St.1:Buds.St.3))
sort(unique(mkwd.13.16.v2$Tot.Bud))
mkwd.13.16.v2$Avg.Bud <- rowMeans(select(mkwd.13.16.v2, Buds.St.1:Buds.St.3))
sort(unique(mkwd.13.16.v2$Avg.Bud))

# Flower number checks
  ## Stem 1
sort(unique(mkwd.13.16.v2$Flow.St.1))
mkwd.13.16.v2$Flow.St.1 <- gsub("no data", "", mkwd.13.16.v2$Flow.St.1)
    ### Note the judgement call
mkwd.13.16.v2$Flow.St.1 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Flow.St.1))
sort(unique(mkwd.13.16.v2$Flow.St.1))

  ## Stem 2
sort(unique(mkwd.13.16.v2$Flow.St.2))
mkwd.13.16.v2$Flow.St.2 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Flow.St.2))
sort(unique(mkwd.13.16.v2$Flow.St.2))

  ## Stem 3
sort(unique(mkwd.13.16.v2$Flow.St.3))
mkwd.13.16.v2$Flow.St.3 <- as.numeric(gsub("dozens", 24, mkwd.13.16.v2$Flow.St.3))
sort(unique(mkwd.13.16.v2$Flow.St.3))

# Flower summarization
mkwd.13.16.v2$Tot.Flr <- rowSums(select(mkwd.13.16.v2, Flow.St.1:Flow.St.3))
sort(unique(mkwd.13.16.v2$Tot.Flr))
mkwd.13.16.v2$Avg.Flr <- rowMeans(select(mkwd.13.16.v2, Flow.St.1:Flow.St.3))
sort(unique(mkwd.13.16.v2$Avg.Flr))

# Bloom status checks
  ## Stem 1
sort(unique(mkwd.13.16.v2$BlooStatus.St.1))
mkwd.13.16.v2$BlooStatus.St.1 <- as.numeric(gsub("\\?", "", mkwd.13.16.v2$BlooStatus.St.1))
sort(unique(mkwd.13.16.v2$BlooStatus.St.1))

  ## Stem 2
sort(unique(mkwd.13.16.v2$BlooStatus.St.2))

  ## Stem 3
sort(unique(mkwd.13.16.v2$BlooStatus.St.3))

# Bloom status summarization
mkwd.13.16.v2$Avg.Bloom.Status <- rowMeans(select(mkwd.13.16.v2, BlooStatus.St.1:BlooStatus.St.3))
sort(unique(mkwd.13.16.v2$Avg.Bloom.Status))

# We've done a lot of fixing so far so let's preserve that by making a new data file
mkwd.13.16.v3 <- mkwd.13.16.v2

# Monarch immatures checks
sort(unique(mkwd.13.16.v3$Monarch.immatures2.))
## This combines eggs and larvae (caterpillars) but we want them separated

# NOTE:
  ## Because this column is loaded with unique, qualitative comments, it cannot be easily fixed
  ## What follows are line-by-line corrections of each of these comments to be only a number
  ## This is burly enough that three subsections will follow

# These subsections will edit the same column into three new columns
  ## (1) number of monarch eggs
  ## (2) number of monarch larvae (i.e., caterpillars)
  ## (3) presence/absence of monarch immatures (incl. frass/plant damage)

## ------------------------------------------------ ##
              # Monarch Egg Tidying ####
## ------------------------------------------------ ##
# Tidy Monarch eggs first
mkwd.13.16.v3$Num.Monarch.Eggs <- tolower(mkwd.13.16.v3$Monarch.immatures2.)
sort(unique(mkwd.13.16.v3$Num.Monarch.Eggs))
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; three 4th or 5th instar$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
  ### Note judgement call here (most conservative number that would still be plural)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; eggs on leaf of c stem$", "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "5", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "4", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "4", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
  ## Note imprecision of "per stem" (assuming 3 stems sampled)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg on bud$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 fourth instar$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 5th instar$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes, 2 monarch eggs$", "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
  ### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^no data$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^na$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^n\\/a; no$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^maybe frass|^maybe$|^frass$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^5 monarch eggs$", "5", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^3 monarch eggs, all on stem a.$", "3", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 monarch eggs$|^2 eggs$", "2", mkwd.13.16.v3$Num.Monarch.Eggs)
  ### Note that I'm not counting hatched eggs towards the total monarchs
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 hatched monarch eggs on bud$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 eggs, 2 2nd instar larvae$", "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 5th instar larvae$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 3rd instar larvae on flower buds$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(3 4th or 5th instar\\)$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "10", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 egg, 1 larva$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaves near stem$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 egg$", "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 3rd instar larvae$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Eggs)
  ### Note that mkwd.13.16.v1$Comments.4 was removed in transition to v2 of the same
  ### That comment says there was an adult seen, not relevant to immatures
mkwd.13.16.v3$Num.Monarch.Eggs <- gsub("^0-\\? see comment 4$", "0", mkwd.13.16.v3$Num.Monarch.Eggs)
sort(unique(mkwd.13.16.v3$Num.Monarch.Eggs))

# Make R count it as a number
mkwd.13.16.v3$Num.Monarch.Eggs <- as.numeric(mkwd.13.16.v3$Num.Monarch.Eggs)
sort(unique(mkwd.13.16.v3$Num.Monarch.Eggs))

## ------------------------------------------------ ##
            # Monarch Larvae Tidying ####
## ------------------------------------------------ ##
# Tidy Monarch larvae next

# TIDYING HERE NOW ####
  ## Below here is actually tidying for eggs (just copy/pasted it, didn't have time to change it)

mkwd.13.16.v3$Num.Monarch.Larvae <- tolower(mkwd.13.16.v3$Monarch.immatures2.)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; three 4th or 5th instar$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note judgement call here (most conservative number that would still be plural)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; eggs on leaf of c stem$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "5", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "4", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "4", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
## Note imprecision of "per stem" (assuming 3 stems sampled)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on bud$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fourth instar$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 5th instar$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 monarch eggs$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^no data$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^na$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^n\\/a; no$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^maybe frass|^maybe$|^frass$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^5 monarch eggs$", "5", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^3 monarch eggs, all on stem a.$", "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 monarch eggs$|^2 eggs$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note that I'm not counting hatched eggs towards the total monarchs
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 hatched monarch eggs on bud$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 eggs, 2 2nd instar larvae$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 5th instar larvae$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 3rd instar larvae on flower buds$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(3 4th or 5th instar\\)$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "10", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg, 1 larva$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaves near stem$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar larvae$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note that mkwd.13.16.v1$Comments.4 was removed in transition to v2 of the same
### That comment says there was an adult seen, not relevant to immatures
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^0-\\? see comment 4$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))

# Make R count it as a number
mkwd.13.16.v3$Num.Monarch.Larvae <- as.numeric(mkwd.13.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))




mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("", "", mkwd.13.16.v3$Num.Monarch.Larvae)



## ------------------------------------------------ ##
        # 2013-16 Data Tidying Continued ####
## ------------------------------------------------ ##




# Total bitten stems
total...bitten.stems








names(mkwd.12.v1)




# 1) Number of stems
# 2) Number of stems that have flowered, are flowering or will flower.
# 3) Number of flower buds on three longest stems.
# 4) Number of flowers on three longest stems.
# 5) Length of each of the three longest stems.
# 6) Number of stems bitten by vertebrates.
# 7) Number of monarch larvae
# 8) Number of monarch eggs
  ## various other data columns




# 2012 doesn't have a site code in the same way that the other years do so it needs one
mkwd.12.v1$Site.Code <- with(mkwd.12.v1, paste0(Site, "-", Patch, "-", "12", "-", "R1") )
sort(unique(mkwd.12.v1$Site.Code))






## ------------------------------------------------ ##
                  # Data Tidying ####
## ------------------------------------------------ ##






## ------------------------------------------------ ##
               # Acknolwedgements ####
## ------------------------------------------------ ##
# Important to remember who helped!
sort(unique(tolower(mkwd.12.v0$Observer)))
  ## Ben Nagel
  ## Courtney Grula
  ## Ray Moranz
  ## Shannon Rusk
  ## Toni Proescholdt

sort(unique(tolower(mkwd.13.16.meta$Observer)))
  ## Anna Holtermann
  ## Jake Mortensen
  ## Audrey McCombs
  ## Gatha [last name?]
  ## John Delaney
  ## Karin Grimlund
  ## Marina Osier
  ## Jessica Williams
  ## Chaz Abarr
  ## Shannon Rusk
  ## Veronica Mecko [Meeko?]

# END ####

