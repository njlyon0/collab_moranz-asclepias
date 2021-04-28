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
library(readxl); library(tidyverse); library(stringr); library(writexl)

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

# Make date a character so it doesn't get messed up when binding to the other dataframe
mkwd.12.v1$Date <- as.character(mkwd.12.v1$Date)
sort(unique(mkwd.12.v1$Date))

# Make a new dataframe to preserve our work
mkwd.12.v2 <- mkwd.12.v1

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
  select(YearVis, Date, Pasture, Patch, Whittaker, PlantNumAuto, PlantID.Code.from.2012,
         Length.St1:LengthSt3, Buds.St.1:Buds.St.3, Flow.St.1:Flow.St.3,
         BlooStatus.St.1:BlooStatus.St.3, Monarch.immatures2., total...bitten.stems)
names(mkwd.13.16.v2)

# Have some tidying to do before we can combine 2012 and 2013-16
  ## We need averages of all of the 'stems 1-3' variables
  ## To get that we need to ensure that those columns are all numbers so that they can BE averaged

# The year column is missing 2013 (but the date column isn't!)
mkwd.13.16.v2$Year <- as.factor(str_sub(mkwd.13.16.v2$Date, 1, 4))
unique(mkwd.13.16.v2$Year)

# Make date a character so it doesn't get messed up when binding to the other dataframe
mkwd.13.16.v2$Date <- as.character(mkwd.13.16.v2$Date)
sort(unique(mkwd.13.16.v2$Date))

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
mkwd.13.16.v3$Num.Monarch.Larvae <- tolower(mkwd.13.16.v3$Monarch.immatures2.)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                         "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; three 4th or 5th instar$", "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; eggs on leaf of c stem$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
## Note imprecision of "per stem" (assuming 3 stems sampled)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on bud$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fourth instar$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 5th instar$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 monarch eggs$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
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
                                       "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^5 monarch eggs$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^3 monarch eggs, all on stem a.$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 monarch eggs$|^2 eggs$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note that I'm not counting hatched eggs towards the total monarchs
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 hatched monarch eggs on bud$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 eggs, 2 2nd instar larvae$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 5th instar larvae$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 3rd instar larvae on flower buds$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(3 4th or 5th instar\\)$", "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "3", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg, 1 larva$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaves near stem$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 egg$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "0", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "2", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar larvae$", "1", mkwd.13.16.v3$Num.Monarch.Larvae)
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.13.16.v3$Num.Monarch.Larvae)
### Note that mkwd.13.16.v1$Comments.4 was removed in transition to v2 of the same
### That comment says there was an adult seen, not relevant to immatures
mkwd.13.16.v3$Num.Monarch.Larvae <- gsub("^0-\\? see comment 4$", "0", mkwd.13.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))

# Make R count it as a number
mkwd.13.16.v3$Num.Monarch.Larvae <- as.numeric(mkwd.13.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.13.16.v3$Num.Monarch.Larvae))

## ------------------------------------------------ ##
      # Monarch Presence/Absence Tidying ####
## ------------------------------------------------ ##
# Just in case, let's get a presence/absence metric too
  ## In many cases, evidence was recorded but no eggs or larvae were observed
  ## I don't want to lose that data so we're going to collect that here

# Last round of tidying the same qualitative observations
mkwd.13.16.v3$Monarch.Immature.Evidence <- tolower(mkwd.13.16.v3$Monarch.immatures2.)
sort(unique(mkwd.13.16.v3$Monarch.Immature.Evidence))
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; three 4th or 5th instar$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
### Note judgement call here (most conservative number that would still be plural)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; eggs on leaf of c stem$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg on bud$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 fourth instar$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 5th instar$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 2 monarch eggs$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^no data$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^na$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^n\\/a; no$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^maybe frass|^maybe$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^frass$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^5 monarch eggs$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^3 monarch eggs, all on stem a.$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 monarch eggs$|^2 eggs$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 hatched monarch eggs on bud$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 eggs, 2 2nd instar larvae$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 5th instar larvae$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 3rd instar larvae on flower buds$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(3 4th or 5th instar\\)$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg, 1 larva$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaves near stem$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 3rd instar larvae$", "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.13.16.v3$Monarch.Immature.Evidence)
  ### Note that mkwd.13.16.v1$Comments.4 was removed in transition to v2 of the same
  ### That comment says there was an adult seen, not relevant to immatures
mkwd.13.16.v3$Monarch.Immature.Evidence <- gsub("^0-\\? see comment 4$", "0", mkwd.13.16.v3$Monarch.Immature.Evidence)
sort(unique(mkwd.13.16.v3$Monarch.Immature.Evidence))

# Make R count it as a number
mkwd.13.16.v3$Monarch.Immature.Evidence <- as.numeric(mkwd.13.16.v3$Monarch.Immature.Evidence)
sort(unique(mkwd.13.16.v3$Monarch.Immature.Evidence))

## ------------------------------------------------ ##
        # 2013-16 Data Tidying Continued ####
## ------------------------------------------------ ##
# Okay, back to the main tidying pipeline

# Let's get a total Monarch immature column
mkwd.13.16.v3$Tot.Monarch.Immatures <- (mkwd.13.16.v3$Num.Monarch.Eggs + mkwd.13.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.13.16.v3$Tot.Monarch.Immatures))

# Total bitten stems
sort(unique(mkwd.13.16.v3$total...bitten.stems))
mkwd.13.16.v3$Num.Bitten.Stems <- mkwd.13.16.v3$total...bitten.stems
  ## Note judgement call (at least 4 becomes 4)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("at least 4", "4", mkwd.13.16.v3$Num.Bitten.Stems)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("^na$", "", mkwd.13.16.v3$Num.Bitten.Stems)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("^unk$|^unk.$|unknown", "", mkwd.13.16.v3$Num.Bitten.Stems)
  ## Note judgement call (to me, these all imply that there were no stems)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("^unk \\(likely they've atrophied\\)$", "0", mkwd.13.16.v3$Num.Bitten.Stems)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("^unk \\(plant gone$", "0", mkwd.13.16.v3$Num.Bitten.Stems)
mkwd.13.16.v3$Num.Bitten.Stems <- gsub("^unk \\(probably had stems that were eaten and atrophied\\)$", "0", mkwd.13.16.v3$Num.Bitten.Stems)
sort(unique(mkwd.13.16.v3$Num.Bitten.Stems))

# Make R count it as a number
mkwd.13.16.v3$Num.Bitten.Stems <- as.numeric(mkwd.13.16.v3$Num.Bitten.Stems)
sort(unique(mkwd.13.16.v3$Num.Bitten.Stems))

# "Patch" includes whittaker number in 2012 so let's make it include that here as well
sort(unique(mkwd.13.16.v3$Patch))
mkwd.13.16.v3$Patch <- paste0(mkwd.13.16.v3$Patch, "-", mkwd.13.16.v3$Whittaker)
sort(unique(mkwd.13.16.v3$Patch))

# Prune off the now-unnecessary columns and keep only the ones that we need
names(mkwd.13.16.v3)
mkwd.13.16.v4 <- mkwd.13.16.v3 %>%
  select(Year, Date, Pasture, Patch, PlantID.Code.from.2012, PlantNumAuto,
         Avg.Height, Avg.Bud, Avg.Flr, Tot.Bud, Tot.Flr, Avg.Bloom.Status, Num.Bitten.Stems,
         Num.Monarch.Eggs, Num.Monarch.Larvae, Tot.Monarch.Immatures, Monarch.Immature.Evidence) %>%
  rename(Year = Year, Date = Date, Site = Pasture, Patch = Patch, Plant.ID = PlantID.Code.from.2012,
         Plant.Num = PlantNumAuto,Avg.Height = Avg.Height, Avg.Bud = Avg.Bud, Avg.Flr = Avg.Flr,
         Tot.Bud = Tot.Bud, Tot.Flr = Tot.Flr, Avg.Bloom.Status = Avg.Bloom.Status,
         Num.Bitten.Stems = Num.Bitten.Stems, Num.Monarch.Eggs = Num.Monarch.Eggs,
         Num.Monarch.Larvae = Num.Monarch.Larvae, Tot.Monarch.Immatures = Tot.Monarch.Immatures,
         Monarch.Immature.Evidence = Monarch.Immature.Evidence)

## ------------------------------------------------ ##
        # Data Joining (2012 + 2013-16) ####
## ------------------------------------------------ ##
# Before we can join we need to add dummy columns to the 2012 data
mkwd.12.v2$Num.Bitten.Stems <- NA
mkwd.12.v2$Num.Monarch.Eggs <- NA
mkwd.12.v2$Num.Monarch.Larvae <- NA
mkwd.12.v2$Tot.Monarch.Immatures <- NA
mkwd.12.v2$Monarch.Immature.Evidence <- NA

# Do both dataframes have the same columns in the same order?
names(mkwd.12.v2)
names(mkwd.13.16.v4)

# Double check that in a more precise way
setdiff(names(mkwd.12.v2), names(mkwd.13.16.v4))
setdiff(names(mkwd.13.16.v4), names(mkwd.12.v2))
  ## Looks good!

# Not the slickest way of doing this, but rbind the two dataframes together!
milkweed.v1 <- rbind(mkwd.12.v2, mkwd.13.16.v4)
str(milkweed.v1)
  ## Looks good!

## ------------------------------------------------ ##
              # Full Data Tidying ####
## ------------------------------------------------ ##
# So this job is half done as prerequisite to joining the two dataframes but let's finish it here
  ## Check each column individually and fix any errors that occur
  ## Also get each column in the format we want it (i.e., factor, numeric, etc.)
names(milkweed.v1)

# Year
sort(unique(milkweed.v1$Year))
milkweed.v1$Year <- as.factor(milkweed.v1$Year)
sort(unique(milkweed.v1$Year))

# Date
sort(unique(milkweed.v1$Date))
  ## Year is in another column already so let's make this just month and day
milkweed.v1$Date <- str_sub(milkweed.v1$Date, 7, 10)
milkweed.v1$Date <- as.numeric(gsub("-", ".", milkweed.v1$Date))
sort(unique(milkweed.v1$Date))

# Make a new dataset to avoid contaminating the progress thus far
milkweed.v2 <- milkweed.v1

# Site names
sort(unique(milkweed.v2$Site))
milkweed.v2$Site <- gsub("Gilleland", "GIL", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Lee Trail Rd", "LTR", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Pawnee Prairie", "PAW", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Pyland North", "PYN", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Pyland South", "PYS", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Pyland West", "PYW", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Richardson", "RCH", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Ringgold North", "RIN", milkweed.v2$Site)
milkweed.v2$Site <- gsub("Ringgold South", "RIS", milkweed.v2$Site)
milkweed.v2$Site <- as.factor(milkweed.v2$Site)
sort(unique(milkweed.v2$Site))

# Patch names
  ## Note that we don't care about Whittaker (i.e., the numbers) transects
sort(unique(milkweed.v2$Patch))
milkweed.v2$Patch <- gsub("^C1$|^C2$|^C3$|^Center-1$|^Center-1 and 2$|^Center-1,2,3$|^Center-2$|^Center-3$",
                          "C", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^E1$|^E2$|^East-1$|^East-2$|^East-1 \\& 2$",
                          "E", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^N1$|^N2$|^North-1$|^North-1 and 2$|^North-1and2$|^North-2$|^North-2 and 3$|^North-N1W1$|^North-N2W2$",
                          "N", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^S1$|^S2$|^S3$|^South-1$|^South-1 \\& 2$|^South-1 and 2$|^South-1 and 3$|^South-1,2$|^South-1and2$|^South-1and3$|^South-2$|^South-3$",
                          "S", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^W1$|^W2$|^W3$|^West-1$|^West-1 and 2$|^West-2$|^West-NA$",
                          "W", milkweed.v2$Patch)
## Fixes requiring judgement calls
  ### Richardson briefly had it's West patch called the "Y" patch
milkweed.v2$Patch <- gsub("^Y-1$", "W", milkweed.v2$Patch)
  ### The remaining issues have the correct information in the Plant.ID column (thankfully)
milkweed.v2$Patch <- ifelse(test = (milkweed.v2$Patch == "East and Center-1 &2" |
                                      milkweed.v2$Patch == "East and Center-NA" |
                                      milkweed.v2$Patch == "multiple-2" |
                                      milkweed.v2$Patch == "North & Center-1 & 2"), 
                            yes = milkweed.v2$Plant.ID, no = milkweed.v2$Patch)
  ### Now just revise the Plant.IDs to have only the patch letter
milkweed.v2$Patch <- gsub("^GIL-C-2013-901$|^GIL-C-2013-902$|^GIL-C1-13-R1-001$|^GILC1001$|^GILC1002$|^GILC1003$",
                          "C", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^LTRC1001$|^LTRC2001$|^LTRC2003$|^LTRC2007$|^LTRC2008$|^LTRC2009$|^LTRC2015$",
                          "C", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^GILN1001$", "N", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^LTRE1001$|^LTRE2001$|^LTRE2002$|^LTRE2003$|^LTRE2004$",
                          "E", milkweed.v2$Patch)
milkweed.v2$Patch <- gsub("^LTRW2001$",
                          "W", milkweed.v2$Patch)
  ### This is a blank row we'll remove later
milkweed.v2$Patch <- gsub("NA-NA", "", milkweed.v2$Patch)
sort(unique(milkweed.v2$Patch))

# Plant.ID code and plant number
sort(unique(milkweed.v2$Plant.ID))
sort(unique(milkweed.v2$Plant.Num))
  ## Need Ray's help to figure this one out

# Average height
sort(unique(milkweed.v2$Avg.Height))
milkweed.v2$Avg.Height <- gsub("no data", "", milkweed.v2$Avg.Height)
milkweed.v2$Avg.Height <- as.numeric(milkweed.v2$Avg.Height)
sort(unique(milkweed.v2$Avg.Height))

# Average number of buds or flowers
sort(unique(milkweed.v2$Avg.Bud))
sort(unique(milkweed.v2$Avg.Flr))

# Total number of buds or flowers
sort(unique(milkweed.v2$Tot.Bud))
sort(unique(milkweed.v2$Tot.Flr))

# Average bloom status
sort(unique(milkweed.v2$Avg.Bloom.Status))
milkweed.v2$Avg.Bloom.Status <- gsub("unknown", "", milkweed.v2$Avg.Bloom.Status)
milkweed.v2$Avg.Bloom.Status <- as.numeric(milkweed.v2$Avg.Bloom.Status)
sort(unique(milkweed.v2$Avg.Bloom.Status))

# I just made and cleaned the remaining columns so these are good to go now
sort(unique(milkweed.v2$Num.Bitten.Stems))
sort(unique(milkweed.v2$Num.Monarch.Eggs))
sort(unique(milkweed.v2$Num.Monarch.Larvae))
sort(unique(milkweed.v2$Tot.Monarch.Immatures))
sort(unique(milkweed.v2$Monarch.Immature.Evidence))

# There are some rows that were entirely empty that we should ditch now
milkweed.v3 <- milkweed.v2[complete.cases(milkweed.v2[, "Site"]), ]

## ------------------------------------------------ ##
        # Explanatory Variable Retrieval ####
## ------------------------------------------------ ##
# None of our explanatory variables are in here yet
# We want the following variables:
  ## (1) Time since fire (TSF)
  ## (2) Management method (e.g., GB, PBG, BO)
  ## (3) Stocking rate (e.g., none, standard, intensive)
  ## (4) Julian day

# Bring in the index file that connects sites/patches with treatments
julian.index <- read_excel("./Data/-Asclepias-Indices.xlsx", sheet = "Julian")
mgmt.index <- read_excel("./Data/-Asclepias-Indices.xlsx", sheet = "Site Management")
str(mgmt.index)

# The following lines will do these three things:
  ## (1) Create the necessary concatenated column in the data file
  ## (2) Pull in the variable of interest from the index file
  ## (3) Check for (and resolve) any errors

# Time since fire (hereafter 'TSF') needs a column including 'Site-Patch-Year'
  ## Make concatenated index code
milkweed.v3$TSF.Index.Code <- with(milkweed.v3, paste0(Site, "-", Patch, "-", Year))
sort(unique(milkweed.v3$TSF.Index.Code))
  ## Bring in desired variable
milkweed.v3$TSF <- mgmt.index$TSF[match(milkweed.v3$TSF.Index.Code, mgmt.index$Pasture.Patch.Year)]
  ## Check it
sort(unique(milkweed.v3$TSF))
summary(milkweed.v3$TSF)
  ## Looks good! On to the next one!

# Management method
  ### Index code not needed because it correlates with site
  ### Desired variable
milkweed.v3$Management <- as.factor(mgmt.index$Management.Method[match(milkweed.v3$Site, mgmt.index$Pasture)])
  ### Check it
sort(unique(milkweed.v3$Management))
summary(milkweed.v3$Management)

# Stocking rate needs 'Year-Site'
  ### Stocking can use the TSF index code
  ### Desired variable
milkweed.v3$Stocking <- as.factor(mgmt.index$Stocking.Type[match(milkweed.v3$TSF.Index.Code, mgmt.index$Pasture.Patch.Year)])
  ### Check
sort(unique(milkweed.v3$Stocking))
summary(milkweed.v3$Stocking)

# Julian day just needs 'Date'!
str(julian.index)
sort(unique(milkweed.v3$Date))
milkweed.v3$Julian <- julian.index$Julian[match(milkweed.v3$Date, julian.index$Date)]
sort(unique(milkweed.v3$Julian))
summary(milkweed.v3$Julian)

# Once we have all of these columns, pare down to just the columns we need
  ## I.e., ditch the various concatenated index columns we needed to bring in the new variables
milkweed.v4 <- milkweed.v3 %>%
  select(Year:Date, Julian, Site:Plant.Num, Management,
         TSF, Stocking, Avg.Height:Monarch.Immature.Evidence)

# Check what we ditched (should be just the unneeded index code)
setdiff(names(milkweed.v3), names(milkweed.v4))
  ## Looks good!

# Remove any accidental rows
milkweed.v5 <- milkweed.v4 %>%
  filter(Plant.ID != "(accidental row)" & Plant.ID != "accidental row" &
           Plant.ID != "Accidental row" & Plant.ID != "ACCIDENTAL ROW")

# Save the tidy data for ease of analysis later on
write_xlsx(list(Data = milkweed.v5),
           path = "./Data/Asclepias-TIDY.xlsx",
           col_names = T, format_headers = T)

## ------------------------------------------------ ##
               # Acknowledgements ####
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

