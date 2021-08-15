## ---------------------------------------------------------------------------------------- ##
               # Moranz et al. Butterfly Milkweed (Asclepias tuberosa) Project
## ---------------------------------------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## This code tidys the raw data into a format that is ready for analysis and figure construction

# Clear the environment
rm(list = ls())

# Set the working directory
setwd("~/Documents/Publications/2022_Moranz_Asclepias/Moranz-AsclepiasCollab")
  ## Session -> Set Working Directory -> Choose Directory...

# Call any needed libraries here (good to centralize this step)
library(readxl); library(tidyverse); library(stringr); library(vegan); library(writexl)

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
  select(Year, Date, Pasture, PatchWhit, `2012PlantIDCode`, `2012PlantIDCode_2nd Rnd`,
         `Length zAVERAGE`, `# buds zAVERAGE`, `# flowers zAVERAGE`,
         `# buds TOTAL`, `# flowers TOTAL`, `bloom status zAVERAGE`,
         TrimbStemsAbud, TrimbStemsBflower, TrimbStemsCdone, TrimbStemsDnoflowers, TrimbStemsFbudflowerdone,
         `# of UNbit stems w axillary`, AsclWithin1m, AsclWithin2m, BfliesNectaring, `Crab spiders?`,
           GrazingLawn, `height on 1st longest`, `height on 2nd longest`, `height on 3rd longest`,
         Comments, `major issues`, MonarchImmatures1, MonarchImmatures2,
         ShrubsWithin1m, TotalNumBittenStems) %>%
  dplyr::rename(Year = Year, Date = Date, Site = Pasture, Patch = PatchWhit,
         Plant.ID.R1 = `2012PlantIDCode`, Plant.ID.R2 = `2012PlantIDCode_2nd Rnd`,
         Avg.Height = `Length zAVERAGE`, Avg.Bud = `# buds zAVERAGE`, Avg.Flr = `# flowers zAVERAGE`,
         Tot.Bud = `# buds TOTAL`, Tot.Flr = `# flowers TOTAL`, Avg.Bloom.Status = `bloom status zAVERAGE`,
         Num.Stems.Budding = TrimbStemsAbud, Num.Stems.Flowering = TrimbStemsBflower,
         Num.Stems.PostFlower = TrimbStemsCdone, Num.Stems.Nonflowering = TrimbStemsDnoflowers, 
         Num.Stems.ALL.Flowering.Stages = TrimbStemsFbudflowerdone,
         Num.Unbit.Stems.w.Axillary.Shoots = `# of UNbit stems w axillary`, 
         ASCTUB.Abun.1m = AsclWithin1m, ASCTUB.Abun.2m = AsclWithin2m, 
         BfliesNectaring = BfliesNectaring, Crab.Spider.Abun = `Crab spiders?`,
         GrazingLawn = GrazingLawn, 
         Height.1st.Longest = `height on 1st longest`, 
         Height.2nd.Longest = `height on 2nd longest`, 
         Height.3rd.Longest = `height on 3rd longest`,
         Comments = Comments, Major.Issues = `major issues`, 
         MonarchImmatures1 = MonarchImmatures1, MonarchImmatures2 = MonarchImmatures2,
         Shrub.Abun.1m = ShrubsWithin1m, Tot.Bitten.Stems = TotalNumBittenStems)

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
# Two notes before we get down to business (sing Mulan to yourself while reading that last bit):
  ## 1) Despite the 2016 data in theory including the 2013-16 data (see file name) it **does not**
    ### For some reason, each year's data file *may* include data that can only be found in that file
    ### This is a nightmare obviously
    ### Doubly so because each year has inconsistent column names
    ### This precludes the possibility of uploading each year and rbind()-ing them together
    ### I think the best path forward is as follows:
      #### Tidy using the pipeline I have already established
      #### At the very end, give the opportunity for NAs to be filled with their respective data

## 2) Though the data are in Excel files (see the "Data" folder), all data uploads are .csv files
    ### The below data is a .csv file directly (unmodified) from the Excel sheet "Plants"
    ### Much of these files are numbers stored as text so the csv file preserves more data than the xlsx
    ### Something about "read_excel()" deletes those numbers stored as text

# Read in the data
mkwd.16.v0 <- read.csv("./Data/Asclepias-2016-RAW-plants.csv")
mkwd.13.16.meta <- read_excel("./Data/Asclepias-2016-RAW.xlsx", sheet = "Metadata")

# Get some diagnostics to understand the scope of tidying ahead of us
str(mkwd.16.v0)
summary(mkwd.16.v0)
names(mkwd.16.v0)
  ## Time for a pivot in approach

# To combine 2012 with the other years we first need to get 2013-16 into a single datafile
  ## The legacy of MS Access lives on in it's current form
mkwd.13.16.meta$AsclepTransID <- mkwd.13.16.meta$AsclepTransIDauto
mkwd.16.v1 <- left_join(mkwd.16.v0, mkwd.13.16.meta, by = "AsclepTransID")
names(mkwd.16.v1)

# Now prune back to just what we need from this dataset
names(mkwd.16.v1)
mkwd.16.v2 <- mkwd.16.v1 %>%
    ## Needed columns
  select(YearVis, Date, Pasture, Patch, Whittaker, PlantID.Code.from.2012,
         Length.St1:LengthSt3, Buds.St.1:Buds.St.3, Flow.St.1:Flow.St.3,
         BlooStatus.St.1:BlooStatus.St.3, Monarch.immatures2., total...bitten.stems,
         TRIMBSBUD:TRIMBS.NOflow, Ascl.within.1.m., Ascl.within.2.m,
         Grazing.lawn., Bflies.nectaring., Crab.spiders., Shrubs.within.1.m.,
         X..of.bitten.stems.flower.this.year, X..bitten.stems.w.axillary, X..of.UNbit.stems.w.axillary,
         X..of.axillary.shoots, X..of.BITTEN.Axillary.shoots) %>%
  dplyr::rename(YearVis = YearVis, Date = Date, Pasture = Pasture, Patch = Patch, Whittaker = Whittaker,
         Plant.ID.R1 = PlantID.Code.from.2012, 
         Length.St1 = Length.St1, Length.St2 = LengthSt2, Length.St3 = LengthSt3,
         Buds.St.1 = Buds.St.1, Buds.St.2 = Buds.St.2, Buds.St.3 = Buds.St.3,
         Flow.St.1 = Flow.St.1, Flow.St.2 = Flow.St.2, Flow.St.3 = Flow.St.3,
         BlooStatus.St.1 = BlooStatus.St.1, BlooStatus.St.2 = BlooStatus.St.2, BlooStatus.St.3 = BlooStatus.St.3,
         MonarchImmatures2 = Monarch.immatures2., Tot.Bitten.Stems = total...bitten.stems,
         Num.Stems.Budding = TRIMBSBUD,
         Num.Stems.Flowering = TRIMBSFLOW,
         Num.Stems.PostFlower = TRIMBSDONE,
         Num.Stems.Nonflowering = TRIMBS.NOflow,
         ASCTUB.Abun.1m = Ascl.within.1.m.,
         ASCTUB.Abun.2m = Ascl.within.2.m,
         GrazingLawn = Grazing.lawn., 
         BfliesNectaring = Bflies.nectaring.,
         Crab.Spider.Abun = Crab.spiders.,
         Shrub.Abun.1m = Shrubs.within.1.m.,
         Num.Flowering.Stems.Bitten = X..of.bitten.stems.flower.this.year,
         Num.Bitten.Stems.w.Axillary.Shoots = X..bitten.stems.w.axillary,
         Num.Unbit.Stems.w.Axillary.Shoots = X..of.UNbit.stems.w.axillary,
         Tot.Axillary.Shoots = X..of.axillary.shoots,
         Num.Axillary.Shoots.Bitten = X..of.BITTEN.Axillary.shoots)

#Num.Stems.ALL.Flowering.Stages = TrimbStemsFbudflowerdone,

names(mkwd.16.v2)

# Have some tidying to do before we can combine 2012 and 2013-16
  ## We need averages of all of the 'stems 1-3' variables
  ## To get that we need to ensure that those columns are all numbers so that they can BE averaged

# The year column is missing 2013 (but the date column isn't!)
mkwd.16.v2$Year <- as.factor(str_sub(mkwd.16.v2$Date, 1, 4))
unique(mkwd.16.v2$Year)

# Make date a character so it doesn't get messed up when binding to the other dataframe
mkwd.16.v2$Date <- as.character(mkwd.16.v2$Date)
sort(unique(mkwd.16.v2$Date))

# Stem length checks
  ## Stem 1
sort(unique(mkwd.16.v2$Length.St1))
mkwd.16.v2$Length.St1 <- gsub("no data|not meas", "", mkwd.16.v2$Length.St1)
mkwd.16.v2$Length.St1 <- as.numeric(gsub("\\(dead\\)", "", mkwd.16.v2$Length.St1))
sort(unique(mkwd.16.v2$Length.St1))

  ## Stem 2
sort(unique(mkwd.16.v2$Length.St2))
mkwd.16.v2$Length.St2 <- as.numeric(gsub("not meas", "", mkwd.16.v2$Length.St2))
sort(unique(mkwd.16.v2$Length.St2))

  ## Stem 3
sort(unique(mkwd.16.v2$Length.St3))
mkwd.16.v2$Length.St3 <- as.numeric(gsub("not meas", "", mkwd.16.v2$Length.St3))
sort(unique(mkwd.16.v2$Length.St3))

# Get the average length of the stems
mkwd.16.v2$Avg.Height <- rowMeans(select(mkwd.16.v2, Length.St1:Length.St3))
sort(unique(mkwd.16.v2$Avg.Height))

# Bud number checks
  ## Stem 1
sort(unique(mkwd.16.v2$Buds.St.1))
mkwd.16.v2$Buds.St.1 <- gsub("no data|not counted|not recorded|too early|tiny",
                                "", mkwd.16.v2$Buds.St.1)
  ### Note the judgement call here
mkwd.16.v2$Buds.St.1 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Buds.St.1))
sort(unique(mkwd.16.v2$Buds.St.1))

  ## Stem 2
sort(unique(mkwd.16.v2$Buds.St.2))
mkwd.16.v2$Buds.St.2 <- gsub("not counted|to early|too early|tiny", "", mkwd.16.v2$Buds.St.2)
mkwd.16.v2$Buds.St.2 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Buds.St.2))
sort(unique(mkwd.16.v2$Buds.St.2))

  ## Stem 3
sort(unique(mkwd.16.v2$Buds.St.3))
mkwd.16.v2$Buds.St.3 <- gsub("not counted|too early|\\?", "", mkwd.16.v2$Buds.St.3)
mkwd.16.v2$Buds.St.3 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Buds.St.3))
sort(unique(mkwd.16.v2$Buds.St.3))

# Bud summarization
mkwd.16.v2$Tot.Bud <- rowSums(select(mkwd.16.v2, Buds.St.1:Buds.St.3))
sort(unique(mkwd.16.v2$Tot.Bud))
mkwd.16.v2$Avg.Bud <- rowMeans(select(mkwd.16.v2, Buds.St.1:Buds.St.3))
sort(unique(mkwd.16.v2$Avg.Bud))

# Flower number checks
  ## Stem 1
sort(unique(mkwd.16.v2$Flow.St.1))
mkwd.16.v2$Flow.St.1 <- gsub("no data", "", mkwd.16.v2$Flow.St.1)
    ### Note the judgement call (this is the same judgement call made everywhere "dozens" was entered)
mkwd.16.v2$Flow.St.1 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Flow.St.1))
sort(unique(mkwd.16.v2$Flow.St.1))

  ## Stem 2
sort(unique(mkwd.16.v2$Flow.St.2))
mkwd.16.v2$Flow.St.2 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Flow.St.2))
sort(unique(mkwd.16.v2$Flow.St.2))

  ## Stem 3
sort(unique(mkwd.16.v2$Flow.St.3))
mkwd.16.v2$Flow.St.3 <- as.numeric(gsub("dozens", 24, mkwd.16.v2$Flow.St.3))
sort(unique(mkwd.16.v2$Flow.St.3))

# Flower summarization
mkwd.16.v2$Tot.Flr <- rowSums(select(mkwd.16.v2, Flow.St.1:Flow.St.3))
sort(unique(mkwd.16.v2$Tot.Flr))
mkwd.16.v2$Avg.Flr <- rowMeans(select(mkwd.16.v2, Flow.St.1:Flow.St.3))
sort(unique(mkwd.16.v2$Avg.Flr))

# Bloom status checks
  ## Stem 1
sort(unique(mkwd.16.v2$BlooStatus.St.1))
mkwd.16.v2$BlooStatus.St.1 <- as.numeric(gsub("\\?", "", mkwd.16.v2$BlooStatus.St.1))
sort(unique(mkwd.16.v2$BlooStatus.St.1))

  ## Stem 2
sort(unique(mkwd.16.v2$BlooStatus.St.2))

  ## Stem 3
sort(unique(mkwd.16.v2$BlooStatus.St.3))

# Bloom status summarization
mkwd.16.v2$Avg.Bloom.Status <- rowMeans(select(mkwd.16.v2, BlooStatus.St.1:BlooStatus.St.3))
sort(unique(mkwd.16.v2$Avg.Bloom.Status))

# We've done a lot of fixing so far so let's preserve that by making a new data file
mkwd.16.v3 <- mkwd.16.v2

# Monarch immatures checks
sort(unique(mkwd.16.v3$MonarchImmatures2))
## This combines eggs and larvae (caterpillars) but we want them separated

# NOTE:
  ## Because this column is loaded with unique, qualitative comments, it cannot be easily fixed
  ## What follows are line-by-line corrections of each of these comments to be only a number
  ## This is burly enough that three subsections will follow

# These subsections will edit the same column into three new columns
  ## (1) number of monarch eggs
  ## (2) number of monarch larvae (i.e., caterpillars)
  ## (3) presence/absence of *evidence of* monarch immatures (incl. frass/plant damage)

## ------------------------------------------------ ##
              # Monarch Egg Tidying ####
## ------------------------------------------------ ##
# Tidy Monarch eggs first
mkwd.16.v3$Num.Monarch.Eggs <- tolower(mkwd.16.v3$MonarchImmatures2)
sort(unique(mkwd.16.v3$Num.Monarch.Eggs))
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; three 4th or 5th instar$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
  ### Note judgement call here (most conservative number that would still be plural)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; eggs on leaf of c stem$", "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "5", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "4", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "4", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "3", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
  ## Note imprecision of "per stem" (assuming 3 stems sampled)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "3", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "3", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 monarch egg on bud$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 fourth instar$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes; 1 5th instar$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes, 2 monarch eggs$", "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "0", mkwd.16.v3$Num.Monarch.Eggs)
  ### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^no data$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^na$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^n\\/a; no$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^maybe frass|^maybe$|^frass$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^5 monarch eggs$", "5", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^3 monarch eggs, all on stem a.$", "3", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 monarch eggs$|^2 eggs$", "2", mkwd.16.v3$Num.Monarch.Eggs)
  ### Note that I'm not counting hatched eggs towards the total monarchs
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 hatched monarch eggs on bud$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 eggs, 2 2nd instar larvae$", "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 5th instar larvae$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 3rd instar larvae on flower buds$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^yes \\(3 4th or 5th instar\\)$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "10", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 egg, 1 larva$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaves near stem$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "2", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 egg$", "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 3rd instar larvae$", "0", mkwd.16.v3$Num.Monarch.Eggs)
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.16.v3$Num.Monarch.Eggs)
  ### Note that mkwd.16.v1$Comments.4 was removed in transition to v2 of the same
  ### That comment says there was an adult seen, not relevant to immatures
mkwd.16.v3$Num.Monarch.Eggs <- gsub("^0-\\? see comment 4$", "0", mkwd.16.v3$Num.Monarch.Eggs)
sort(unique(mkwd.16.v3$Num.Monarch.Eggs))

# Make R count it as a number
mkwd.16.v3$Num.Monarch.Eggs <- as.numeric(mkwd.16.v3$Num.Monarch.Eggs)
sort(unique(mkwd.16.v3$Num.Monarch.Eggs))

## ------------------------------------------------ ##
            # Monarch Larvae Tidying ####
## ------------------------------------------------ ##
# Tidy Monarch larvae next
mkwd.16.v3$Num.Monarch.Larvae <- tolower(mkwd.16.v3$MonarchImmatures2)
sort(unique(mkwd.16.v3$Num.Monarch.Larvae))
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                         "3", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; three 4th or 5th instar$", "3", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; eggs on leaf of c stem$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
## Note imprecision of "per stem" (assuming 3 stems sampled)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 monarch egg on bud$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fourth instar$", "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes; 1 5th instar$", "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 monarch eggs$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "1", mkwd.16.v3$Num.Monarch.Larvae)
### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes$", "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^no data$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^na$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^n\\/a; no$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^maybe frass|^maybe$|^frass$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "8", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^5 monarch eggs$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^3 monarch eggs, all on stem a.$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 monarch eggs$|^2 eggs$", "0", mkwd.16.v3$Num.Monarch.Larvae)
### Note that I'm not counting hatched eggs towards the total monarchs
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 hatched monarch eggs on bud$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 eggs, 2 2nd instar larvae$", "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 5th instar larvae$", "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 3rd instar larvae on flower buds$", "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^yes \\(3 4th or 5th instar\\)$", "3", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "3", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 egg, 1 larva$", "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaves near stem$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 egg$", "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "0", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "2", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar larvae$", "1", mkwd.16.v3$Num.Monarch.Larvae)
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.16.v3$Num.Monarch.Larvae)
### Note that mkwd.16.v1$Comments.4 was removed in transition to v2 of the same
### That comment says there was an adult seen, not relevant to immatures
mkwd.16.v3$Num.Monarch.Larvae <- gsub("^0-\\? see comment 4$", "0", mkwd.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.16.v3$Num.Monarch.Larvae))

# Make R count it as a number
mkwd.16.v3$Num.Monarch.Larvae <- as.numeric(mkwd.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.16.v3$Num.Monarch.Larvae))

## ------------------------------------------------ ##
      # Monarch Presence/Absence Tidying ####
## ------------------------------------------------ ##
# Just in case, let's get a presence/absence metric too
  ## In many cases, evidence was recorded but no eggs or larvae were observed
  ## I don't want to lose that data so we're going to collect that here

# Last round of tidying the same qualitative observations
mkwd.16.v3$Monarch.Immature.Evidence <- tolower(mkwd.16.v3$MonarchImmatures2)
sort(unique(mkwd.16.v3$Monarch.Immature.Evidence))
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; two monarch eggs on stem c \\(specifically on underside of leaves, 10cm and 22cm from top of plant\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds \\(and were somewhat hidden in the cluster of buds\\); all three stems are the most advanced phenologically for this plant \\(all have a few red tipped buds\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; three 4th or 5th instar$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; strangely, one egg on upper surface of leaf \\(10\" below flowers\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; small monarch frass on each stem; some buds eaten away$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem a \\(and 10\" below flower\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; monarch egg on stem a; frass on stem b and c$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; lots of leaves missing\\/chewed up by insects, and some large monarch frass; did not see larvae$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
### Note judgement call here (most conservative number that would still be plural)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; eggs on leaf of c stem$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem a, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 5 monarch eggs plus small monarch frass\\)$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1\\/2\" and 2\" below bud cluster$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 4 monarch eggs total \\(2 on taller stems - both on young leaves\\), \\(2 on shorter stems  - 1 on stem, 1 on young leaves\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 2 monarch eggs on buds. 1 monarch larva \\(2nd instar\\) in buds. frass from large 5th instar larva.$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem c$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem b \\(another resprout\\) and large frass \\(potentially monarch frass\\); third monarch egg also on red tipped buds of an axillary shoot on stem c$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 monarch egg on bud$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 fourth instar$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 fifth instar monarch larvae feeding on stem a when we arrived$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 egg on 1cm long leaf next to buds on stem a$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes; 1 5th instar$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 2 monarch eggs$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 2 dark monarch eggs \\(both on flower buds\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar \\(\\?\\) caterpillar hiding in grass next to tallest stem; large amounts of poop$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(frass\\)|yes \\(frass on stem, some buds eaten away\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(1 monarch egg on bud of stem c\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(1 5th instar\\)$|^yes \\(1 4th instar\\)$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
### Note judgement call (assuming at least 1 egg and larva in these cases)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^unk \\(not recorded\\)$|^trans$|^presum no$",
                                       "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^no; frass found on leaves but not of monarch$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^no data$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^no$|^no \\(could not find plant\\!\\)$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^na$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^n\\/a; no; caterpillar poop on stem, some missing buds \\(eaten\\?\\)$",
                                       "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^n\\/a; no$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^monarch larva\\(e\\) had chewed leaves, pods$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; some insect has been eating some buds \\(in fashion of monarch larva\\)$",
                                       "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; some buds have been chewed on$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^maybe; 5 buds chewed by small invert$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^maybe frass|^maybe$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^frass$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^found 3  2nd instar monarch larva on stem b and 5 on stem c. they were feeding on buds. both have mixture of small green buds and red buds.$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^did not measure$|^did not assess$",
                                       "", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^5 monarch eggs$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^3 monarch eggs, all on stem a.$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 monarch eggs$|^2 eggs$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 hatched monarch eggs on bud$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 eggs, 2 2nd instar larvae$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 5th instar larvae$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 3rd instar larvae on flower buds$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^yes \\(3 4th or 5th instar\\)$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^2 2nd instars on stem a \\(eating buds\\), 1 egg on bud of stem b, 1 2nd instar and 1 egg on stem c \\(on buds\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^10 monarch eggs \\(7 eggs on buds, 3 on leaves\\)$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 monarch egg, plus evidence of other larvae on the stem$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 monarch egg on a bud$|^1 monarch egg$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 larva \\(1st instar resting on bud stalk on stem a. one egg on big leaf on stem b.$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg, 1 larva$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaves near stem$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaf, plus evidence of herbivory by monarch larvae$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg on leaf next to buds on stem c; 1 egg on leaf near top of stem a \\(no buds\\)$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 egg$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. \\! live 5th instar ot base of the plant$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 4th instar near base, 1 2nd instar eating flowers$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 3rd instar larvae$", "1", mkwd.16.v3$Monarch.Immature.Evidence)
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^1 3rd instar iarva, 1 monarch egg, evidence of other larvae.$",
                                       "1", mkwd.16.v3$Monarch.Immature.Evidence)
  ### Note that mkwd.16.v1$Comments.4 was removed in transition to v2 of the same
  ### That comment says there was an adult seen, not relevant to immatures
mkwd.16.v3$Monarch.Immature.Evidence <- gsub("^0-\\? see comment 4$", "0", mkwd.16.v3$Monarch.Immature.Evidence)
sort(unique(mkwd.16.v3$Monarch.Immature.Evidence))

# Make R count it as a number
mkwd.16.v3$Monarch.Immature.Evidence <- as.numeric(mkwd.16.v3$Monarch.Immature.Evidence)
sort(unique(mkwd.16.v3$Monarch.Immature.Evidence))

## ------------------------------------------------ ##
        # 2013-16 Data Tidying Continued ####
## ------------------------------------------------ ##
# Okay, back to the main tidying pipeline

# Let's get a total Monarch immature column
mkwd.16.v3$Tot.Monarch.Immatures <- (mkwd.16.v3$Num.Monarch.Eggs + mkwd.16.v3$Num.Monarch.Larvae)
sort(unique(mkwd.16.v3$Tot.Monarch.Immatures))

# Total bitten stems
sort(unique(mkwd.16.v3$Tot.Bitten.Stems))
  ## Note judgement call ("at least 4" becomes 4)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("at least 4", "4", mkwd.16.v3$Tot.Bitten.Stems)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("^na$", NA, mkwd.16.v3$Tot.Bitten.Stems)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("^unk$|^unk.$|unknown", NA, mkwd.16.v3$Tot.Bitten.Stems)
  ## Note judgement call (to me, these all imply that there were no stems)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("^unk \\(likely they've atrophied\\)$", "0", mkwd.16.v3$Tot.Bitten.Stems)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("^unk \\(plant gone$", "0", mkwd.16.v3$Tot.Bitten.Stems)
mkwd.16.v3$Tot.Bitten.Stems <- gsub("^unk \\(probably had stems that were eaten and atrophied\\)$", "0", mkwd.16.v3$Tot.Bitten.Stems)
sort(unique(mkwd.16.v3$Tot.Bitten.Stems))

# Make R count it as a number
mkwd.16.v3$Tot.Bitten.Stems <- as.numeric(mkwd.16.v3$Tot.Bitten.Stems)
sort(unique(mkwd.16.v3$Tot.Bitten.Stems))

# "Patch" includes whittaker number in 2012 so let's make it include that here as well
sort(unique(mkwd.16.v3$Patch))
mkwd.16.v3$Patch <- paste0(mkwd.16.v3$Patch, "-", mkwd.16.v3$Whittaker)
sort(unique(mkwd.16.v3$Patch))

# Rather than use the "rename()" function, duplicate Pasture and Plant.ID with the right name
mkwd.16.v3$Site <- mkwd.16.v3$Pasture
mkwd.16.v3$Plant.ID <- mkwd.16.v3$Plant.ID.R1

# Prune off the now-unnecessary columns and keep only the ones that we need
names(mkwd.16.v3)
mkwd.16.v4 <- mkwd.16.v3 %>%
  select(Year, Date, Site, Patch, Plant.ID,
         Avg.Height, Avg.Bud, Avg.Flr, Tot.Bud, Tot.Flr, Avg.Bloom.Status, 
         Num.Stems.Budding, Num.Stems.Flowering, Num.Stems.PostFlower, Num.Stems.Nonflowering,
         Num.Unbit.Stems.w.Axillary.Shoots, ASCTUB.Abun.1m, ASCTUB.Abun.2m, BfliesNectaring,
         Crab.Spider.Abun, GrazingLawn, Shrub.Abun.1m, Tot.Bitten.Stems, Num.Flowering.Stems.Bitten,
         Num.Bitten.Stems.w.Axillary.Shoots, Tot.Axillary.Shoots, Num.Axillary.Shoots.Bitten,
         Num.Monarch.Eggs, Num.Monarch.Larvae, Monarch.Immature.Evidence, Tot.Monarch.Immatures)

## ------------------------------------------------ ##
        # Data Joining (2012 + 2013-16) ####
## ------------------------------------------------ ##
# Before we can join we need to do two things:
  ## 1) for any column in one dataframe but not the other, add a dummy column with the same name
  ## 2) Ensure that all columns are in the same order

# What is in the 2013-16 data that isn't in the 2012 data?
setdiff(names(mkwd.16.v4), names(mkwd.12.v2))

# Add them in as dummy columns
  ## Rather than use the "rename()" function, duplicate Plant.ID with the right name
mkwd.12.v2$Plant.ID <- mkwd.12.v2$Plant.ID.R1
mkwd.12.v2$Num.Flowering.Stems.Bitten <- NA
mkwd.12.v2$Num.Bitten.Stems.w.Axillary.Shoots <- NA
mkwd.12.v2$Tot.Axillary.Shoots <- NA
mkwd.12.v2$Num.Axillary.Shoots.Bitten <- NA
mkwd.12.v2$Num.Monarch.Eggs <- NA
mkwd.12.v2$Num.Monarch.Larvae <- NA
mkwd.12.v2$Monarch.Immature.Evidence <- NA
mkwd.12.v2$Tot.Monarch.Immatures <- NA

# Check to make sure they transferred right
setdiff(names(mkwd.16.v4), names(mkwd.12.v2))

# Vice versa? (in 2012 but missing from 13-16)
  ## Check
setdiff(names(mkwd.12.v2), names(mkwd.16.v4))

  ## Create
mkwd.16.v4$Plant.ID.R2 <- NA
mkwd.16.v4$Num.Stems.ALL.Flowering.Stages <- NA
mkwd.16.v4$Height.1st.Longest <- NA
mkwd.16.v4$Height.2nd.Longest <- NA
mkwd.16.v4$Height.3rd.Longest <- NA
mkwd.16.v4$Comments <- NA
mkwd.16.v4$Major.Issues <- NA

  ## Check again
setdiff(names(mkwd.12.v2), names(mkwd.16.v4))
      ### We're ditching the monarch immatures columns so this is fine

# Reorder the 2012 column names
mkwd.12.v3 <- mkwd.12.v2 %>%
  select(Year, Date, Site, Patch, Plant.ID, Avg.Height, Avg.Bud, Avg.Flr,
         Tot.Bud, Tot.Flr, Avg.Bloom.Status, Num.Stems.Budding, Num.Stems.Flowering,
         Num.Stems.PostFlower, Num.Stems.Nonflowering, Num.Stems.ALL.Flowering.Stages,
         Num.Unbit.Stems.w.Axillary.Shoots, ASCTUB.Abun.1m, ASCTUB.Abun.2m,
         BfliesNectaring, Crab.Spider.Abun, GrazingLawn,
         Comments, Major.Issues, Shrub.Abun.1m, Tot.Bitten.Stems, Num.Flowering.Stems.Bitten,
         Num.Bitten.Stems.w.Axillary.Shoots, Tot.Axillary.Shoots, Num.Axillary.Shoots.Bitten,
         Num.Monarch.Eggs, Num.Monarch.Larvae, Monarch.Immature.Evidence, Tot.Monarch.Immatures)

# Reorder the 2013-16 columns too
mkwd.16.v5 <- mkwd.16.v4 %>%
  select(Year, Date, Site, Patch, Plant.ID, Avg.Height, Avg.Bud, Avg.Flr,
         Tot.Bud, Tot.Flr, Avg.Bloom.Status, Num.Stems.Budding, Num.Stems.Flowering,
         Num.Stems.PostFlower, Num.Stems.Nonflowering, Num.Stems.ALL.Flowering.Stages,
         Num.Unbit.Stems.w.Axillary.Shoots, ASCTUB.Abun.1m, ASCTUB.Abun.2m,
         BfliesNectaring, Crab.Spider.Abun, GrazingLawn,
         Comments, Major.Issues, Shrub.Abun.1m, Tot.Bitten.Stems, Num.Flowering.Stems.Bitten,
         Num.Bitten.Stems.w.Axillary.Shoots, Tot.Axillary.Shoots, Num.Axillary.Shoots.Bitten,
         Num.Monarch.Eggs, Num.Monarch.Larvae, Monarch.Immature.Evidence, Tot.Monarch.Immatures)

# Do both dataframes have the same columns in the same order?
names(mkwd.12.v3)
names(mkwd.16.v5)

# Triple check that in a more precise way
setdiff(names(mkwd.12.v3), names(mkwd.16.v5))
setdiff(names(mkwd.16.v5), names(mkwd.12.v3))
  ## Looks good!

# Not the slickest way of doing this, but rbind the two dataframes together!
milkweed.v1 <- rbind(mkwd.12.v3, mkwd.16.v5)
str(milkweed.v1)
  ## Looks good!

## ------------------------------------------------ ##
         # Full Data Tidying (Part 1) ####
## ------------------------------------------------ ##
# So this job is partially done as prerequisite to joining the two dataframes but let's finish it here
  ## Check each column individually and fix any errors that occur
  ## Also get each column in the format we want it (i.e., factor, numeric, etc.)
names(milkweed.v1)

# Year
sort(unique(milkweed.v1$Year))
milkweed.v1$Year <- as.numeric(as.character(milkweed.v1$Year))
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

# Plant.ID code
sort(unique(milkweed.v2$Plant.ID))
  ## Formatting is wonky and may need revisiting but good enough for now (may not use anyway)

# Let's remove the accidental rows here
milkweed.v3 <- milkweed.v2 %>%
  filter(Plant.ID != "(accidental row)" & Plant.ID != "accidental row" &
           Plant.ID != "Accidental row" & Plant.ID != "ACCIDENTAL ROW")
sort(unique(milkweed.v3$Plant.ID))

# Average height
sort(unique(milkweed.v3$Avg.Height))
milkweed.v3$Avg.Height <- gsub("no data", "", milkweed.v3$Avg.Height)
milkweed.v3$Avg.Height <- as.numeric(milkweed.v3$Avg.Height)
sort(unique(milkweed.v3$Avg.Height))

# Average number of buds or flowers
sort(unique(milkweed.v3$Avg.Bud))
sort(unique(milkweed.v3$Avg.Flr))

# Total number of buds or flowers
sort(unique(milkweed.v3$Tot.Bud))
sort(unique(milkweed.v3$Tot.Flr))

# Average bloom status
sort(unique(milkweed.v3$Avg.Bloom.Status))
milkweed.v3$Avg.Bloom.Status <- gsub("unknown", "", milkweed.v3$Avg.Bloom.Status)
milkweed.v3$Avg.Bloom.Status <- as.numeric(milkweed.v3$Avg.Bloom.Status)
sort(unique(milkweed.v3$Avg.Bloom.Status))

# Number budding stems
sort(unique(milkweed.v3$Num.Stems.Budding))
milkweed.v3$Num.Stems.Budding <- gsub("could not find|plant|no sign of ", "",
                                      milkweed.v3$Num.Stems.Budding)
milkweed.v3$Num.Stems.Budding <- as.numeric(milkweed.v3$Num.Stems.Budding)
sort(unique(milkweed.v3$Num.Stems.Budding))

# Number flowering stems
sort(unique(milkweed.v3$Num.Stems.Flowering))
milkweed.v3$Num.Stems.Flowering <- as.numeric(milkweed.v3$Num.Stems.Flowering)
sort(unique(milkweed.v3$Num.Stems.Flowering))

# Number stems post flowering (but were once flowering)
sort(unique(milkweed.v3$Num.Stems.PostFlower))
milkweed.v3$Num.Stems.PostFlower <- as.numeric(milkweed.v3$Num.Stems.PostFlower)
sort(unique(milkweed.v3$Num.Stems.PostFlower))

# Number of stems that won't flower (at least that year)
sort(unique(milkweed.v3$Num.Stems.Nonflowering))
milkweed.v3$Num.Stems.Nonflowering <- gsub("1 \\(bitten off 8 \" from ground, presumably by deer",
                                           "1", milkweed.v3$Num.Stems.Nonflowering)
milkweed.v3$Num.Stems.Nonflowering <- gsub("1 dying", "1", milkweed.v3$Num.Stems.Nonflowering)
milkweed.v3$Num.Stems.Nonflowering <- gsub("2- these particular stems are unusually weak",
                                           "2", milkweed.v3$Num.Stems.Nonflowering)
milkweed.v3$Num.Stems.Nonflowering <- as.numeric(milkweed.v3$Num.Stems.Nonflowering)
sort(unique(milkweed.v3$Num.Stems.Nonflowering))

# Number of stems in any flowering stage
sort(unique(milkweed.v3$Num.Stems.ALL.Flowering.Stages))
summary(milkweed.v3$Num.Stems.ALL.Flowering.Stages) # a lot of NAs, let's fix that
milkweed.v3$Num.Stems.ALL.Flowering.Stages <- ifelse(is.na(milkweed.v3$Num.Stems.ALL.Flowering.Stages) == T,
                                                     yes = with(milkweed.v3, (Num.Stems.Budding + Num.Stems.Flowering + Num.Stems.PostFlower)), 
                                                     no = milkweed.v3$Num.Stems.ALL.Flowering.Stages)
summary(milkweed.v3$Num.Stems.ALL.Flowering.Stages) # many NAs fixed!
sort(unique(milkweed.v3$Num.Stems.ALL.Flowering.Stages))

# Number of unbitten stems with axillary shoots
sort(unique(milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots))
milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots <- gsub("^n.a.$|^n\\/a$|^na$|^no data$|^u$|^unk$|^unknown$",
                                                      "", milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots)
milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots <- as.numeric(milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots)
sort(unique(milkweed.v3$Num.Unbit.Stems.w.Axillary.Shoots))

# Number of Asclepias tuberosa within 1 meter
milkweed.v3$ASCTUB.Abun.1m <- tolower(milkweed.v3$ASCTUB.Abun.1m)
sort(unique(milkweed.v3$ASCTUB.Abun.1m))
milkweed.v3$ASCTUB.Abun.1m <- gsub("^yes; one a. tuberosa with 1 flowering stem is 90cm to ssw$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
  ## Note judgement call
milkweed.v3$ASCTUB.Abun.1m <- gsub("^yes; number not recorded$",
                                   "2", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^unk$|^uncertain$", NA, milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^tiny \\(5\" tall\\) a. tuberosa seedling only 40cm to sse$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^this plant is less than 1m from plants 105 and 106; ~20cm s$",
                                   "2", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^there is 1 a. tuberosa \\(with 1 stem in bud\\) 35cm to se \\(~90 small green buds\\) and 1 a. tuberosa \\(1stem, bitten\\) 35cm to sw$",
                                   "2", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^seedling 15cm tall 70cm to s$", "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^not recorded$|^no data$|^no$|^did not measure$|^did not assess$",
                                   NA, milkweed.v3$ASCTUB.Abun.1m)
  ### I double checked the raw data.
milkweed.v3$ASCTUB.Abun.1m <- gsub("^as on 6\\/29\\/2014$", "2", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^another robust a. tuberosa 1m to e and a smaller a. tuberosa 0.8m to wsw$",
                                   "2", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^another a. tuberosa only 0.65m to wnw - named it paws1401$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
  ### Note judgement call
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 or more$", "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 at with 2 stems in bud only 80cm to ne$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa with only 2 stems in bloom only 0.9m e$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa with 3 stems in bloom \\(and 1 stem that won't bloom this year\\) only 1m to nnw$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa with 2 stems in flower 1m to se; no other within 1.4m$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa with 1 stem in bud only 60cm to w$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa 0.9m to e$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^1 a. tuberosa \\(only 1 stem, not flowering\\) is 50cm ne of this plant$",
                                   "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^0.65m wnw of paws1007$", "1", milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- gsub("^\\?$", NA, milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- as.numeric(milkweed.v3$ASCTUB.Abun.1m)
sort(unique(milkweed.v3$ASCTUB.Abun.1m))

# Number of Asclepias tuberosa within 2 meters
milkweed.v3$ASCTUB.Abun.2m <- tolower(milkweed.v3$ASCTUB.Abun.2m)
sort(unique(milkweed.v3$ASCTUB.Abun.2m))
milkweed.v3$ASCTUB.Abun.2m <- gsub("^yes; one$", "1", milkweed.v3$ASCTUB.Abun.2m)
  ### Note judgement call
milkweed.v3$ASCTUB.Abun.2m <- gsub("^yes; number not recorded$|^yes; \\# unk$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^yes; 1 a. tuberosa 1.2m sw$", "1", milkweed.v3$ASCTUB.Abun.2m)
  ### Note judgement call
milkweed.v3$ASCTUB.Abun.2m <- gsub("^yes, one plant 1.7m away, small plant with only 2 stems, neither will bloom; no monarch eggs on this neighbor$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
  ### Note judgement call
milkweed.v3$ASCTUB.Abun.2m <- gsub("^yes \\(\\# unk\\)$|^yes$|^1 or more$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^this plant is 1.7m ene of risc1002$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^there is another a. tuberosa 1.5m n of gps point \\(2 stems with no flowers this year\\) and 1 a. tuberosa 1.5m se of gps point \\(1 stem with no flowers\\)$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^seedling 18cm tall \\(two stems\\) 1.2m to wsw$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^one 2m to ene, another 2m to nnw; both are healthy;$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^n2006 1.1m to nnw$", "1", milkweed.v3$ASCTUB.Abun.2m)
  ## Note judgement call
milkweed.v3$ASCTUB.Abun.2m <- gsub("^multiple a. tuberosa within 1.5 m \\!!$", "3", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^healthy a. tuberosa with 2 stems in early bud is 1.5m to the w; that plant, which is earlier phenologically, does not have monarch eggs$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^closest milkweed is 1.1m north$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
  ## Note judgement call
milkweed.v3$ASCTUB.Abun.2m <- gsub("^at least 1$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^another a. tuberosa with 1 stem \\(no flowers\\) is 30cm to w of plant$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^another a. tuberosa 1.6m s of a. tuberosa$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^another a. tuberosa 1.1m s and another 1.4m nw$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^another a. tuberosa 1.1m away$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^a. tuberosa 1.5m to ne, which has 2 stems, each stem having hundreds of buds and/or flowers$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^12 ~1.2m from w1003$", "12", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 robust a. tuberosa 1.15m n of this plant \\(with 13 stems that are done flowering\\) and plant pawe2045 is 1.3m s of this plant$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 other a. tuberosa only 95cm to sw, it will not bloom this year$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 milkweed within 1.1m to se$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a.t. is 2m to the s$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tubersosa with 2 stems in bloom only 1.4m ne$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa only 1.7m to e; it had 3 stems, but all were bitten by cattle, and none will bloom$",
                                   "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa 1.9m to s$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa 1.5m to e \\(with 1 stem that won't flower this year\\); 1 a. tuberosa to s \\(with 1 stem in bud\\) with 13 buds and no signs of monarch immatures$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa 1.5m to e$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa 1.3 m to sw$", "1", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^1 a. tuberosa \\(with 2 stems\\) 2.0 m nw, 1 a.tuberosa 2.0m sw \\(with 1 stem in bloom\\)$",
                                   "2", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^no$|^no data$|^unk$|^unknown$|^not recorded$|^did not assess$|^did not measure$|^\\?$",
                                   NA, milkweed.v3$ASCTUB.Abun.2m)
# Non A. tuberosa congeners
milkweed.v3$ASCTUB.Abun.2m <- gsub("^no; a. syriaca, but protecte dby two branches of dead tree \\(1.3m n of a. tuberosa\\)$",
                                   "0", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^no; 6 whorled milkweed \\(non-flowering\\)$",
                                   "0", milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- gsub("^no; 1 asclep. hirtella approx. 2m to e$", "0", milkweed.v3$ASCTUB.Abun.2m)
# Make R read the column as numbers
milkweed.v3$ASCTUB.Abun.2m <- as.numeric(milkweed.v3$ASCTUB.Abun.2m)
sort(unique(milkweed.v3$ASCTUB.Abun.2m))

# Okay, now let's do a quick fix for if only one of the two values (1m versus 2m) was entered
  ## If 1m is blank replace with 2m
summary(milkweed.v3$ASCTUB.Abun.1m)
milkweed.v3$ASCTUB.Abun.1m <- ifelse(is.na(milkweed.v3$ASCTUB.Abun.1m) == T,
                                     yes = milkweed.v3$ASCTUB.Abun.2m,
                                     no = milkweed.v3$ASCTUB.Abun.1m)
summary(milkweed.v3$ASCTUB.Abun.1m) # Fixed some at least

  ## Vice versa
summary(milkweed.v3$ASCTUB.Abun.2m)
milkweed.v3$ASCTUB.Abun.2m <- ifelse(is.na(milkweed.v3$ASCTUB.Abun.2m) == T,
                                     yes = milkweed.v3$ASCTUB.Abun.1m,
                                     no = milkweed.v3$ASCTUB.Abun.2m)
summary(milkweed.v3$ASCTUB.Abun.2m) # We did our best

# Preserve our work to this point
milkweed.v4 <- milkweed.v3

# This column includes the identity (species) and number of nectaring butterflies
  ## We will do a partial cleaning of generally relevant stuff
  ## and then duplicate this column once for each species recorded
milkweed.v4$BfliesNectaring <- tolower(milkweed.v4$BfliesNectaring)
sort(unique(milkweed.v4$BfliesNectaring))
milkweed.v4$BfliesNectaring <- gsub("^ 0$", "0", milkweed.v4$BfliesNectaring)
milkweed.v4$BfliesNectaring <- gsub("^did not assess$|^did not measure$|^n\\/a; no$|^na$|^no$|^too late$|^unk \\(not recorded\\)$",
                                    NA, milkweed.v4$BfliesNectaring)
milkweed.v4$BfliesNectaring <- gsub("upon arriving, ", "", milkweed.v4$BfliesNectaring)
  ### Note judgement call (we only want to keep ones we can tie to a particular species
milkweed.v4$BfliesNectaring <- gsub("^1$|^2$|^yes$|^yes \\(initial visit 7\\/3\\/2014\\)$", 
                                    NA, milkweed.v4$BfliesNectaring)
sort(unique(milkweed.v4$BfliesNectaring))

# Make a new column for each species then clean them all
milkweed.v4$black.swallowtail <- milkweed.v4$BfliesNectaring
milkweed.v4$bronze.copper <- milkweed.v4$BfliesNectaring
milkweed.v4$coral.hairstreak <- milkweed.v4$BfliesNectaring
milkweed.v4$etb <- milkweed.v4$BfliesNectaring
milkweed.v4$edward.hairstreak <- milkweed.v4$BfliesNectaring
milkweed.v4$gray.copper <- milkweed.v4$BfliesNectaring
milkweed.v4$great.spangled.frit <- milkweed.v4$BfliesNectaring
milkweed.v4$juniper.hairstreak <- milkweed.v4$BfliesNectaring
milkweed.v4$meadow.frit <- milkweed.v4$BfliesNectaring
milkweed.v4$orange.sulphur <- milkweed.v4$BfliesNectaring
milkweed.v4$pearl.crescent <- milkweed.v4$BfliesNectaring
milkweed.v4$regal.frit <- milkweed.v4$BfliesNectaring
milkweed.v4$tiger.swallowtail <- milkweed.v4$BfliesNectaring

# Make a new dataframe to save our progress
milkweed.v5 <- milkweed.v4

## ------------------------------------------------ ##
        # Nectaring Butterfly Cleaning ####
## ------------------------------------------------ ##
  ## Black swallowtail
sort(unique(milkweed.v5$black.swallowtail))
milkweed.v5$black.swallowtail <- gsub("^1 black swallowtail$",
                                      "1", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 coral hairstreak and 1 great spangled$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 coral hairstreak nectaring on this plant$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 coral hairstreak was nectaring on longest stem$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 eastern tailed blue$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 great spangled$|^1 great spangled frit$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 great spangled nectaring as i arrived$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 orange sulphur at 6:40 pm$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^great spangled frit nectaring$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^n\\/a; yes, pearl crescent$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^pearl crescent, meadow frit.$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes, 1 pearl crescent$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes; 1 coral hairstreak nectaring$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                      "0", milkweed.v5$black.swallowtail)
milkweed.v5$black.swallowtail <- as.numeric(milkweed.v5$black.swallowtail)
sort(unique(milkweed.v5$black.swallowtail))

# Bronze copper
sort(unique(milkweed.v5$bronze.copper))
milkweed.v5$bronze.copper <- gsub("^1 black swallowtail$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 coral hairstreak and 1 great spangled$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 coral hairstreak nectaring on this plant$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 coral hairstreak was nectaring on longest stem$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 eastern tailed blue$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 great spangled$|^1 great spangled frit$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 great spangled nectaring as i arrived$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 orange sulphur at 6:40 pm$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^great spangled frit nectaring$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^n\\/a; yes, pearl crescent$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^pearl crescent, meadow frit.$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes, 1 pearl crescent$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes; 1 coral hairstreak nectaring$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                      "0", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                      "1", milkweed.v5$bronze.copper)
milkweed.v5$bronze.copper <- as.numeric(milkweed.v5$bronze.copper)
sort(unique(milkweed.v5$bronze.copper))

# Coral Hairstreak
sort(unique(milkweed.v5$coral.hairstreak))
milkweed.v5$coral.hairstreak <- gsub("^1 black swallowtail$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 coral hairstreak and 1 great spangled$",
                                  "1", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                  "1", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 eastern tailed blue$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 great spangled$|^1 great spangled frit$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 great spangled nectaring as i arrived$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                  "1", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 orange sulphur at 6:40 pm$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                  "1", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                  "2", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^great spangled frit nectaring$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^n\\/a; yes, pearl crescent$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^pearl crescent, meadow frit.$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes, 1 pearl crescent$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes; 1 coral hairstreak nectaring$",
                                  "1", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                  "0", milkweed.v5$coral.hairstreak)
milkweed.v5$coral.hairstreak <- as.numeric(milkweed.v5$coral.hairstreak)
sort(unique(milkweed.v5$coral.hairstreak))

# Eastern Tailed-Blue
sort(unique(milkweed.v5$etb))
milkweed.v5$etb <- gsub("^1 black swallowtail$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 coral hairstreak and 1 great spangled$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 eastern tailed blue$",
                                     "1", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 great spangled$|^1 great spangled frit$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 great spangled nectaring as i arrived$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 orange sulphur at 6:40 pm$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^great spangled frit nectaring$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^n\\/a; yes, pearl crescent$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^pearl crescent, meadow frit.$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes, 1 pearl crescent$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes; 1 coral hairstreak nectaring$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                     "0", milkweed.v5$etb)
milkweed.v5$etb <- as.numeric(milkweed.v5$etb)
sort(unique(milkweed.v5$etb))

# Edward's Hairstreak
sort(unique(milkweed.v5$edward.hairstreak))
milkweed.v5$edward.hairstreak <- gsub("^1 black swallowtail$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 coral hairstreak and 1 great spangled$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 eastern tailed blue$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                        "1", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 great spangled$|^1 great spangled frit$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 great spangled nectaring as i arrived$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 orange sulphur at 6:40 pm$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^great spangled frit nectaring$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^n\\/a; yes, pearl crescent$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^pearl crescent, meadow frit.$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes, 1 pearl crescent$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes; 1 coral hairstreak nectaring$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes; 1 tiger swallowtail nectaring$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                        "0", milkweed.v5$edward.hairstreak)
milkweed.v5$edward.hairstreak <- as.numeric(milkweed.v5$edward.hairstreak)
sort(unique(milkweed.v5$edward.hairstreak))

# Gray Copper
sort(unique(milkweed.v5$gray.copper))
milkweed.v5$gray.copper <- gsub("^1 black swallowtail$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 coral hairstreak and 1 great spangled$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 eastern tailed blue$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                      "1", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 great spangled$|^1 great spangled frit$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                      "1", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 great spangled nectaring as i arrived$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                      "1", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 orange sulphur at 6:40 pm$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^great spangled frit nectaring$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^n\\/a; yes, pearl crescent$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^pearl crescent, meadow frit.$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes, 1 pearl crescent$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes; 1 coral hairstreak nectaring$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                      "1", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                      "0", milkweed.v5$gray.copper)
milkweed.v5$gray.copper <- as.numeric(milkweed.v5$gray.copper)
sort(unique(milkweed.v5$gray.copper))

# Great Spangled Fritillary
sort(unique(milkweed.v5$great.spangled.frit))
milkweed.v5$great.spangled.frit <- gsub("^1 black swallowtail$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 coral hairstreak and 1 great spangled$",
                                "1", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 eastern tailed blue$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 great spangled$|^1 great spangled frit$",
                                "1", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                "1", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 great spangled nectaring as i arrived$",
                                "1", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 orange sulphur at 6:40 pm$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^great spangled frit nectaring$",
                                "1", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^n\\/a; yes, pearl crescent$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^pearl crescent, meadow frit.$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes, 1 pearl crescent$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes; 1 coral hairstreak nectaring$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                "0", milkweed.v5$great.spangled.frit)
milkweed.v5$great.spangled.frit <- as.numeric(milkweed.v5$great.spangled.frit)
sort(unique(milkweed.v5$great.spangled.frit))

# Juniper Hairstreak
sort(unique(milkweed.v5$juniper.hairstreak))
milkweed.v5$juniper.hairstreak <- gsub("^1 black swallowtail$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 coral hairstreak and 1 great spangled$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 eastern tailed blue$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 great spangled$|^1 great spangled frit$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 great spangled nectaring as i arrived$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 orange sulphur at 6:40 pm$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^great spangled frit nectaring$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^n\\/a; yes, pearl crescent$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^pearl crescent, meadow frit.$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                        "1", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes, 1 pearl crescent$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes; 1 coral hairstreak nectaring$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                        "0", milkweed.v5$juniper.hairstreak)
milkweed.v5$juniper.hairstreak <- as.numeric(milkweed.v5$juniper.hairstreak)
sort(unique(milkweed.v5$juniper.hairstreak))

# Meadow Fritillary
sort(unique(milkweed.v5$meadow.frit))
milkweed.v5$meadow.frit <- gsub("^1 black swallowtail$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 coral hairstreak and 1 great spangled$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 eastern tailed blue$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 great spangled$|^1 great spangled frit$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 great spangled nectaring as i arrived$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 orange sulphur at 6:40 pm$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^great spangled frit nectaring$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^n\\/a; yes, pearl crescent$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^pearl crescent, meadow frit.$",
                                       "1", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes, 1 pearl crescent$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes; 1 coral hairstreak nectaring$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                       "0", milkweed.v5$meadow.frit)
milkweed.v5$meadow.frit <- as.numeric(milkweed.v5$meadow.frit)
sort(unique(milkweed.v5$meadow.frit))

# Orange Sulphur
sort(unique(milkweed.v5$orange.sulphur))
milkweed.v5$orange.sulphur <- gsub("^1 black swallowtail$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 coral hairstreak and 1 great spangled$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 eastern tailed blue$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 great spangled$|^1 great spangled frit$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 great spangled nectaring as i arrived$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 orange sulphur at 6:40 pm$",
                                "1", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^great spangled frit nectaring$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^n\\/a; yes, pearl crescent$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^pearl crescent, meadow frit.$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes, 1 pearl crescent$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes; 1 coral hairstreak nectaring$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                "0", milkweed.v5$orange.sulphur)
milkweed.v5$orange.sulphur <- as.numeric(milkweed.v5$orange.sulphur)
sort(unique(milkweed.v5$orange.sulphur))

# Pearl Crescent
sort(unique(milkweed.v5$pearl.crescent))
milkweed.v5$pearl.crescent <- gsub("^1 black swallowtail$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 coral hairstreak and 1 great spangled$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 eastern tailed blue$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 great spangled$|^1 great spangled frit$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 great spangled nectaring as i arrived$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 orange sulphur at 6:40 pm$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^great spangled frit nectaring$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^n\\/a; yes, pearl crescent$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^pearl crescent, meadow frit.$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes, 1 pearl crescent$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes; 1 coral hairstreak nectaring$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                   "1", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                   "0", milkweed.v5$pearl.crescent)
milkweed.v5$pearl.crescent <- as.numeric(milkweed.v5$pearl.crescent)
sort(unique(milkweed.v5$pearl.crescent))

# Regal Fritillary
sort(unique(milkweed.v5$regal.frit))
milkweed.v5$regal.frit <- gsub("^1 black swallowtail$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 coral hairstreak and 1 great spangled$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 eastern tailed blue$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 great spangled$|^1 great spangled frit$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 great spangled nectaring as i arrived$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                   "1", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                   "1", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 orange sulphur at 6:40 pm$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^great spangled frit nectaring$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^n\\/a; yes, pearl crescent$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^pearl crescent, meadow frit.$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes, 1 pearl crescent$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes; 1 coral hairstreak nectaring$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                   "0", milkweed.v5$regal.frit)
milkweed.v5$regal.frit <- as.numeric(milkweed.v5$regal.frit)
sort(unique(milkweed.v5$regal.frit))

# Tiger Swallowtail
sort(unique(milkweed.v5$tiger.swallowtail))
milkweed.v5$tiger.swallowtail <- gsub("^1 black swallowtail$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 coral hairstreak and 1 great spangled$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 coral hairstreak nectaring on this plant$|^1 coral hairstreak was nectaring on longest stem$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 eastern tailed blue$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 edward's hairstreak nectaring on this plant when i arrived at 2:45pm$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 gray copper$|^1 gray copper at 6:40 pm$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 great spangled$|^1 great spangled frit$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 great spangled fritillary and 1 gray copper nectaring at 4:20 pm$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 great spangled nectaring as i arrived$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 male regal nectaring; 1 gray copper nectared during data collection$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 male regal, 1 pearl crescent, 1 coral hairstreak$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 orange sulphur at 6:40 pm$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 pearl cres.$|^1 pearl crescent$|^1 pearl crescent was nectaring on 2nd longest stem$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^1 pearl crescent, 1 coral hairstreak$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^2 coral hairstreaks nectaring when i arrived$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^great spangled frit nectaring$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^n\\/a; yes, pearl crescent$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^pearl crescent, meadow frit.$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes, 1 juniper hairstreak nectaring for 20 minutes \\(photos 1365, 1366 on iphone\\)$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes, 1 pearl crescent$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes; 1 coral hairstreak nectaring$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes; 1 gray copper nectaring on stem b when i arrived$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes; 1 pearl crescent nectaring at ~15\\:00 hrs; full sun; 84f; mild wind$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes; 1 tiger swallowtail nectaring$",
                                   "1", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- gsub("^yes; bronze copper nectaring at this plant \\(i took multiple shots on my nikon in flower mode\\)$",
                                   "0", milkweed.v5$tiger.swallowtail)
milkweed.v5$tiger.swallowtail <- as.numeric(milkweed.v5$tiger.swallowtail)
sort(unique(milkweed.v5$tiger.swallowtail))

# Check the structure of all of these guys
str(select(milkweed.v5, black.swallowtail:tiger.swallowtail))

# Let's get a total butterfly abundance (and species richness) column
bf.abun <- rowSums(select(milkweed.v5, black.swallowtail:tiger.swallowtail))
bf.rich <- specnumber(select(milkweed.v5, black.swallowtail:tiger.swallowtail))

# Add them back into the dataframe
milkweed.v5$Nectaring.Bfly.Abun <- bf.abun
milkweed.v5$Nectaring.Bfly.Rich <- bf.rich

# Check 'em out real fast before moving on
sort(unique(milkweed.v5$Nectaring.Bfly.Abun))
sort(unique(milkweed.v5$Nectaring.Bfly.Rich))

## ------------------------------------------------ ##
          # Full Data Tidying (Part 2) ####
## ------------------------------------------------ ##
# Make (yet another) new version of the data to preserve our progress
milkweed.v6 <- milkweed.v5
  ## Keep the cleaning party going and move on to the next one!

# Crab spider abundance
milkweed.v6$Crab.Spider.Abun <- tolower(milkweed.v6$Crab.Spider.Abun)
sort(unique(milkweed.v6$Crab.Spider.Abun))
milkweed.v6$Crab.Spider.Abun <- gsub("^ no$|^no$", "0", milkweed.v6$Crab.Spider.Abun)
milkweed.v6$Crab.Spider.Abun <- gsub("^1 yellow crab spider in blossom of this plant$|^1 yellow crab spider on stem b$|^1 yellow crab spider on stem c,$|^1 yellow crab spider with dead apis mellifera in its clutches$",
                                     "1", milkweed.v6$Crab.Spider.Abun)
milkweed.v6$Crab.Spider.Abun <- gsub("^1 crab spider in flower on stem a. yellow spider at \\@\\:45 pm$|^2 crab spider$",
                                     "2", milkweed.v6$Crab.Spider.Abun)
milkweed.v6$Crab.Spider.Abun <- gsub("^yes; 1 yellow crab spider hiding amidst blossoms$|^yes, on a$|^yes; 1 yellow crab spider hiding in blossoms$|^yes; yellow crab spider$",
                                     "1", milkweed.v6$Crab.Spider.Abun)
milkweed.v6$Crab.Spider.Abun <- gsub("^did not assess$|^did not measure$|^n\\/a; no$|^na$|^no data$|^presum no$|^unk \\(not recorded\\)$",
                                     NA, milkweed.v6$Crab.Spider.Abun)
milkweed.v6$Crab.Spider.Abun <- as.numeric(milkweed.v6$Crab.Spider.Abun)
sort(unique(milkweed.v6$Crab.Spider.Abun))

# The degree of grazing lawn
milkweed.v6$GrazingLawn <- tolower(milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("no, but |yes |yes, |n\\/a; ", "", milkweed.v6$GrazingLawn)
sort(unique(milkweed.v6$GrazingLawn))
milkweed.v6$GrazingLawn <- gsub("^\\(mild\\)$|^//(minor//)$|^\\(very mild\\)$|^no; 9 cattle bedding spots within 12m radius$|^\\(minor\\)$",
                                "mild", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^no; only 1m from cattle trail; 7 large areas flattened by sleeping ungulates within 12m radius$",
                                "mild", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^\\(mod.\\)$|^\\(patchy\\)$|^a spotty lawn$|^moderate lawn$|^yes$|^partial$|^partial lawn$|^yes$",
                                "moderate", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^moderate to severe$",
                                "severe", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^.5 m away$|^\\(at edge\\)$|^1 m away$|^10 cm away from one$|^1m from one$|^2 m away$|^20 cm away$|^20 cm from one$",
                                "within 5 meters", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^30 cm away$|^30cm away$|^30cm away$|^adjacent$|^adjacent to one$|^adjacent to small lawn$|^at edge$|^close$",
                                "within 5 meters", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^no right next to one$|^near$|^near one$|^near small one$|next to one^$|^no \\(but near\\)$",
                                "within 5 meters", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^no but 1 m away$|^no, 1 m away$|^on one side$|^only 30 cm away from one$|^within 1 m$",
                                "within 5 meters", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^next to one$", "within 5 meters", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^in cattle path$|^maybe last year$|^no$",
                                "no lawn", milkweed.v6$GrazingLawn)
milkweed.v6$GrazingLawn <- gsub("^did not measure$|^no data$|^unk$",
                                NA, milkweed.v6$GrazingLawn)
sort(unique(milkweed.v6$GrazingLawn))

# Make R see it as a factor
milkweed.v6$GrazingLawn <- as.factor(milkweed.v6$GrazingLawn)
sort(unique(milkweed.v6$GrazingLawn))

# Save our work from earlier before tackling the shrub column
milkweed.v7 <- milkweed.v6

# Check the shrub abundance column
  ## Same issue as the butterfly column (species and # blended in a single column)
  ## I am not certain that species-level shrub ID will be useful
  ## SO, we're going to leave the door open to species-level ID in future, but condense them to a single number now
milkweed.v7$Shrub.Abun.1m <- tolower(milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("n\\/a; ", "", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("yes; ", "", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("large ", "", milkweed.v7$Shrub.Abun.1m)
  ### Note judgement call
milkweed.v7$Shrub.Abun.1m <- gsub("^20 or 30$", "25", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^no$", "0", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^no data$|^did not assess$|^did not measure$|^unk$|^unknown$",
                                  NA, milkweed.v7$Shrub.Abun.1m)
sort(unique(milkweed.v7$Shrub.Abun.1m))

## Removing non-shrub plants
milkweed.v7$Shrub.Abun.1m <- gsub("0, but 1 baptisia alba|0, but 2 baptisia alba and solidago rigida|0, but 3 baptistia alba nearby|0, but lots of ironweed|4 ironweed|barbed wire fence|^2 baptisia alba$",
                                  "0", milkweed.v7$Shrub.Abun.1m)

## Synonym fixes
milkweed.v7$Shrub.Abun.1m <- gsub("cornus|dogweed", "dogwood", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("black raspberry|rubus", "blackberry", milkweed.v7$Shrub.Abun.1m)
  ### Note decision to have only one ID for all roses
milkweed.v7$Shrub.Abun.1m <- gsub("prairie rose|multi-flora rose|carolina rose|multi-floral rose|multiflora rose",
                                  "rose", milkweed.v7$Shrub.Abun.1m)

## Now get the numbers only
  ### ≥10 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^12 blackberry$",
                                  "12", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^11 buckbrush$",
                                  "11", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^9 dogwoods, 1 shingle oak$|^10 buckbrush$",
                                  "10", milkweed.v7$Shrub.Abun.1m)
  ### 9 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^9 buckbrush$|^6 blackberry, 1 rose, 2 buckbrush$",
                                  "9", milkweed.v7$Shrub.Abun.1m)
  ### 8 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^8 dogwood$|^8 buckbrush$|^5 blackberry, 3 buckbrush$",
                                  "8", milkweed.v7$Shrub.Abun.1m)
  ### 7 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^7 rose$|^7 buckbrush$|^7 blackberry$|^5 dogwood 2 blackberry$",
                                  "7", milkweed.v7$Shrub.Abun.1m)
  ### 6 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^6 blackberry$|^6 buckbrush$|^6 dogwood black berry$|^6 dogwood oak$|^5 buckbrush, 1 osage orange$|^3 sumac, 3 blackberry$|^1 locust tree, 5 black berry$",
                                  "6", milkweed.v7$Shrub.Abun.1m)
  ### 5 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^5 buckbrush\\(1 huge clump\\)$|^5 buckbrush; 20 buckbrush stems \\(may have helped protect plant\\)$",
                                  "5", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^5 buck brush$|^5 buckbrush$|^3 buckbrush; 20cm from woody elm stem; 40cm from buckbrush$",
                                  "5", milkweed.v7$Shrub.Abun.1m)
  ### 4 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^4 rose$|^4 buckbrush$|^4 buck brush$|^4 blackberry$|^3 buckbursh, 1 osage orange$|^3 buckbrush, osage orange$|^3 blackberry, 1 rosa$",
                                  "4", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^3 buck brush, 1 multi prarie rose$|^3 buckbrush and 1 rose$|^1 buckbrush, 2 blackberry, 1 elm sapling$|^1 honey locust, 3 licorice$",
                                  "4", milkweed.v7$Shrub.Abun.1m)
  ### 3 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^3 dogwood$|^3 rose$|^3 buckbrush$|^3 black berries$|^1 osage orange, 1 plum, 1 raspberry$|^1 buckbrush, 1 multi-stemmed, 2 fall elm$|^1 elm, 1 buckbrush, 1 hawthorn$",
                                  "3", milkweed.v7$Shrub.Abun.1m)
  ### 2 shrubs
milkweed.v7$Shrub.Abun.1m <- gsub("^2 small rose$|^2 shrubs \\(1 hawthorn\\)$|^2 buckbrush within 30cm$|^2 buckbrush$|^1 osage orange, 1 buckbrush$",
                                  "2", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^2 multi flora rose$|^2 buck brush$|^2 buckbrush within 30 cm$|^2 small buckbrush$|^1 osage orange, 1 raspberry$",
                                  "2", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 blackberry, 1sumac$|^1 buckbrush, 1 osage orange$",
                                  "2", milkweed.v7$Shrub.Abun.1m)
### 1 shrub
milkweed.v7$Shrub.Abun.1m <- gsub("^plant within 50cm of buckbrush or other shrub$|^osage orange$|^6' tall osage orange$|^15cm from base of a buckbrush$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 buckbrush, 1 baptisia alba$|^1 \\(.8m west of osage orange\\)$|^1 2m tall cedar only 30cm away$|^1 blackberry$|^1 buck brush$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 buckbrush$|^1 buckbrush 10cm to w$|^1 buckbrush 1m away$|^1 buckbrush 30cm away$|^1 buckbrush 40cm away$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 buckbrush 60 cm s$|^1 buckbrush 80cm sw$|^1 buckbrush 95cm to n$|^1 buckbrush 99cm away$|^1 elm$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 osage orange$|^1 osage orange \\(2 m tall\\)$|^1 plum$|^1 rosa multiflora$|^1 rose$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 sharp cedar snag 15cm to w$|^1 sharp cedar snag 50cm to se$|^1 sharp cedar snag 80cm to wsw$|^1 shrubby elm$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 small elm only 5cm from a. tuberosa$|^1 small multiflower rose$|^1 sumac$|^1 wild plum$",
                                  "1", milkweed.v7$Shrub.Abun.1m)
milkweed.v7$Shrub.Abun.1m <- gsub("^1 smooth sumac$|^1 wild grape$|^1 wild plum \\(20cm s\\)$",
                                                                    "1", milkweed.v7$Shrub.Abun.1m)
sort(unique(milkweed.v7$Shrub.Abun.1m))

## Turn it into a true number
milkweed.v7$Shrub.Abun.1m <- as.numeric(milkweed.v7$Shrub.Abun.1m)
sort(unique(milkweed.v7$Shrub.Abun.1m))

# As per always, make a new version of the dataframe
milkweed.v8 <- milkweed.v7

# Number of stems producing flowers that are bitten
sort(unique(milkweed.v8$Num.Flowering.Stems.Bitten))
milkweed.v8$Num.Flowering.Stems.Bitten <- gsub("unknown|^unk$|na", NA, milkweed.v8$Num.Flowering.Stems.Bitten)
milkweed.v8$Num.Flowering.Stems.Bitten <- as.numeric(milkweed.v8$Num.Flowering.Stems.Bitten)
sort(unique(milkweed.v8$Num.Flowering.Stems.Bitten))

# Number of bitten stems that produced axillary shoots
sort(unique(milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots))
milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots <- gsub("^unk$|unknown|unclear|no data|na",
                                                       NA, milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots)
milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots <- as.numeric(milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots)
sort(unique(milkweed.v8$Num.Bitten.Stems.w.Axillary.Shoots))

# Total axillary shoots
sort(unique(milkweed.v8$Tot.Axillary.Shoots))
milkweed.v8$Tot.Axillary.Shoots <- gsub("^\\?$|^unk$|unknown|na|no data", NA, milkweed.v8$Tot.Axillary.Shoots)
  ### Note judgement call
milkweed.v8$Tot.Axillary.Shoots <- gsub("dozens\\?", "24", milkweed.v8$Tot.Axillary.Shoots)
milkweed.v8$Tot.Axillary.Shoots <- gsub("4 bitten, 2 unbitten", "6", milkweed.v8$Tot.Axillary.Shoots)
milkweed.v8$Tot.Axillary.Shoots <- gsub("3 tiny ones", "3", milkweed.v8$Tot.Axillary.Shoots)
milkweed.v8$Tot.Axillary.Shoots <- gsub("7 \\(6\\+1\\)", "7", milkweed.v8$Tot.Axillary.Shoots)
milkweed.v8$Tot.Axillary.Shoots <- as.numeric(milkweed.v8$Tot.Axillary.Shoots)
sort(unique(milkweed.v8$Tot.Axillary.Shoots))

# Number of axillary shoots that are bitten
sort(unique(milkweed.v8$Num.Axillary.Shoots.Bitten))
milkweed.v8$Num.Axillary.Shoots.Bitten <- gsub("^\\?$|^na$|^no data$|^unk$|^unknown$", NA,milkweed.v8$Num.Axillary.Shoots.Bitten)
milkweed.v8$Num.Axillary.Shoots.Bitten <- as.numeric(milkweed.v8$Num.Axillary.Shoots.Bitten)
sort(unique(milkweed.v8$Num.Axillary.Shoots.Bitten))

# Total bitten stems
sort(unique(milkweed.v8$Tot.Bitten.Stems))

# I made and cleaned the remaining columns earlier in this code so these are good to go now
sort(unique(milkweed.v8$Num.Monarch.Eggs))
sort(unique(milkweed.v8$Num.Monarch.Larvae))
sort(unique(milkweed.v8$Tot.Monarch.Immatures))
sort(unique(milkweed.v8$Monarch.Immature.Evidence))

# There are some rows that were entirely empty that we should ditch now
milkweed.v9 <- milkweed.v8[complete.cases(milkweed.v8[, "Site"]), ]

# Also, let's quickly make a buds + flowers column
milkweed.v9$Tot.Bud.n.Flr <- rowSums(select(milkweed.v9, Tot.Bud, Tot.Flr))
sort(unique(milkweed.v9$Tot.Bud.n.Flr))

# Also also, let's get a ratio of bitten stems versus total stems
  ## There is no "total stems" column so I am adding total reproductive vs. nonreproductive stems
milkweed.v9$Ratio.Bitten.vs.Total.Stems <- with(milkweed.v9, (Tot.Bitten.Stems / (Num.Stems.ALL.Flowering.Stages + Num.Stems.Nonflowering)))
  ## Check for issues
sort(unique(milkweed.v9$Ratio.Bitten.vs.Total.Stems))
  ## Fix 'em
milkweed.v9$Ratio.Bitten.vs.Total.Stems <- ifelse(test = milkweed.v9$Ratio.Bitten.vs.Total.Stems == "Inf" | milkweed.v9$Ratio.Bitten.vs.Total.Stems > 1,
                                                  yes = NA, no = milkweed.v9$Ratio.Bitten.vs.Total.Stems)
  ## Check again
sort(unique(milkweed.v9$Ratio.Bitten.vs.Total.Stems))

# Also (x3), make a ratio of flowering to total stems
  ## Calculate the new ratio column
milkweed.v9$Ratio.Flowering.vs.Total.Stems <- with(milkweed.v9, (Num.Stems.ALL.Flowering.Stages / (Num.Stems.ALL.Flowering.Stages + Num.Stems.Nonflowering)))
  ## Check for issues
sort(unique(milkweed.v9$Ratio.Flowering.vs.Total.Stems))

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
milkweed.v9$TSF.Index.Code <- with(milkweed.v9, paste0(Site, "-", Patch, "-", Year))
sort(unique(milkweed.v9$TSF.Index.Code))
  ## Bring in desired variable
milkweed.v9$TSF <- mgmt.index$TSF[match(milkweed.v9$TSF.Index.Code, mgmt.index$Pasture.Patch.Year)]
  ## Check it
sort(unique(milkweed.v9$TSF))
summary(milkweed.v9$TSF)
  ## Looks good! On to the next one!

# Management method
  ### Index code not needed because it correlates with site
  ### Desired variable
milkweed.v9$Management <- as.factor(mgmt.index$Management.Method[match(milkweed.v9$Site, mgmt.index$Pasture)])
  ### Check it
sort(unique(milkweed.v9$Management))
summary(milkweed.v9$Management)

# Stocking rate needs 'Year-Site'
  ### Stocking can use the TSF index code
  ### Desired variable
milkweed.v9$Stocking <- as.factor(mgmt.index$Stocking.Type[match(milkweed.v9$TSF.Index.Code, mgmt.index$Pasture.Patch.Year)])
  ### Check
sort(unique(milkweed.v9$Stocking))
summary(milkweed.v9$Stocking)

# Julian day just needs 'Date'!
str(julian.index)
sort(unique(milkweed.v9$Date))
milkweed.v9$Julian <- julian.index$Julian[match(milkweed.v9$Date, julian.index$Date)]
sort(unique(milkweed.v9$Julian))
summary(milkweed.v9$Julian)

# Once we have all of these columns, pare down to just the columns we need
  ## I.e., ditch the various concatenated index columns we needed to bring in the new variables
milkweed.v10 <- milkweed.v9 %>%
  select(Year:Date, Julian, Site:Plant.ID, TSF:Stocking, GrazingLawn,
         Avg.Height:Tot.Flr, Tot.Bud.n.Flr, Ratio.Bitten.vs.Total.Stems, Ratio.Flowering.vs.Total.Stems,
         Avg.Bloom.Status:ASCTUB.Abun.2m, Crab.Spider.Abun, Shrub.Abun.1m:Nectaring.Bfly.Rich)

# Check what we ditched (should be just the unneeded index code)
setdiff(names(milkweed.v9), names(milkweed.v10))
  ## Looks good!

## ------------------------------------------------ ##
         # Missing Data Retrieval Prep ####
## ------------------------------------------------ ##
# As noted in the "2013-16 Data Tidying" heading, each year's file has some data found nowhere else
  ## This is usually, but not always, in the 2013-16 data (no I don't know why this was done)
  ## So, here is where we're going to load each data file and import that stuff
  ## We'll do this on a column by column basis and then clean the results

# Read in each year's data (excl. 2016 because that was our starting point)
mkwd.13.v0 <- read.csv("./Data/Asclepias-2013-RAW-plants.csv")
mkwd.14.v0 <- read.csv("./Data/Asclepias-2014-RAW-plants.csv")
mkwd.15.v0 <- read.csv("./Data/Asclepias-2015-RAW-plants.csv")

# In order for this to work, each of these datasets need a "year" column and a "plant ID" column
  ## Let's get those columns from the metadata file as we did with the 2016 data
names(mkwd.13.v0)
mkwd.13.v1 <- left_join(mkwd.13.v0, mkwd.13.16.meta, by = "AsclepTransID")
names(mkwd.13.v1)

names(mkwd.14.v0)
mkwd.14.v1 <- left_join(mkwd.14.v0, mkwd.13.16.meta, by = "AsclepTransID")
names(mkwd.14.v1)

names(mkwd.15.v0)
mkwd.15.v1 <- left_join(mkwd.15.v0, mkwd.13.16.meta, by = "AsclepTransID")
names(mkwd.15.v1)

# First, check the contents of the "YearVis" column in each dataset
unique(mkwd.13.v1$YearVis) # unentered for this year
unique(mkwd.14.v1$YearVis)
unique(mkwd.15.v1$YearVis)

# Subset 2014 and '15 to be just those years
mkwd.13.v2 <- mkwd.13.v1 # want the name to be consistent even though subsetting not required here
mkwd.13.v2$YearVis <- as.numeric(rep("2013", nrow(mkwd.13.v2)))
mkwd.14.v2 <- mkwd.14.v1 %>%
  filter(YearVis == "2014")
mkwd.15.v2 <- mkwd.15.v1 %>%
  filter(YearVis == "2015")

# Did it work?
unique(mkwd.13.v2$YearVis)
unique(mkwd.14.v2$YearVis)
unique(mkwd.15.v2$YearVis)
  ## Yep

# Okay, we need the "plant ID" column still
  ## To simplify, let's name that column to match the tidy data
  ## We don't care about redundant columns in these guys because we're just cannibalizing some columns
names(mkwd.13.v2)
mkwd.13.v2$Plant.ID <- mkwd.13.v2$PlantID.Code.from.2012
names(mkwd.14.v2)
mkwd.14.v2$Plant.ID <- mkwd.14.v2$PlantID.Code.from.2012
names(mkwd.15.v2)
mkwd.15.v2$Plant.ID <- mkwd.15.v2$PlantID.Code.from.2012

# Did this work?
unique(mkwd.13.v2$Plant.ID)
unique(mkwd.14.v2$Plant.ID)
unique(mkwd.15.v2$Plant.ID)
  ## Yep!

# Okay, last step before we can import the data
  ## Get a combo year and plant.ID column (in the large tidy data too!)
mkwd.13.v2$Temp.Plant.Code <- paste0(mkwd.13.v2$Year, "-", mkwd.13.v2$Plant.ID)
mkwd.14.v2$Temp.Plant.Code <- paste0(mkwd.14.v2$Year, "-", mkwd.14.v2$Plant.ID)
mkwd.15.v2$Temp.Plant.Code <- paste0(mkwd.15.v2$Year, "-", mkwd.15.v2$Plant.ID)
milkweed.v10$Temp.Plant.Code <- paste0(milkweed.v10$Year, "-", milkweed.v10$Plant.ID)

# Did *this* work?
unique(mkwd.13.v2$Temp.Plant.Code)
unique(mkwd.14.v2$Temp.Plant.Code)
unique(mkwd.15.v2$Temp.Plant.Code)
unique(milkweed.v10$Temp.Plant.Code)
  ## Thank god

# Now, 2013 and '14 are further complicated because some of the data was added to a different sheet
  ## Joy.

# Get the "stem" data
mkwd.13.stm.v0 <- read.csv("./Data/Asclepias-2013-RAW-stems.csv")
mkwd.14.stm.v0 <- read.csv("./Data/Asclepias-2014-RAW-stems.csv")

# Take a quick look at them
str(mkwd.13.stm.v0)
str(mkwd.14.stm.v0)

# Both dfs are in long format but we can dodge pivoting them if we summarise them
  ## BUT, both are (as per usual) a combination of letters and numbers and are unsummarizable
  ## So, if we fix that issue, we can summarize for the values we want and import them as needed

# Fix stem length
  ## '13
sort(unique(mkwd.13.stm.v0$Stem.Length..cm.))
mkwd.13.stm.v0$Stem.Length..cm. <- as.numeric(gsub("accidental row|Accidental Row|not recorded",
                                        NA, mkwd.13.stm.v0$Stem.Length..cm.))
sort(unique(mkwd.13.stm.v0$Stem.Length..cm.))

  ## '14
sort(unique(mkwd.14.stm.v0$Stem.Length..cm.))
mkwd.14.stm.v0$Stem.Length..cm. <- gsub("accidental row|Accidental row|Accidental Row|no data|not recorded|unk",
                                        NA, mkwd.14.stm.v0$Stem.Length..cm.)
mkwd.14.stm.v0$Stem.Length..cm. <- as.numeric(gsub("\\?", NA, mkwd.14.stm.v0$Stem.Length..cm.))
sort(unique(mkwd.14.stm.v0$Stem.Length..cm.))

# Fix number of buds
  ## '13
sort(unique(mkwd.13.stm.v0$X..of.buds))
mkwd.13.stm.v0$X..of.buds <- as.numeric(gsub("accidental row|to small|too early|too small",
                                  NA, mkwd.13.stm.v0$X..of.buds))
sort(unique(mkwd.13.stm.v0$X..of.buds))

  ## '14
sort(unique(mkwd.14.stm.v0$X..of.buds))
mkwd.14.stm.v0$X..of.buds <- gsub("accidental row|to small|too early|too small|too small to count|unk",
                                  NA, mkwd.14.stm.v0$X..of.buds)
mkwd.14.stm.v0$X..of.buds <- gsub("Accidental row|\\?",
                                  NA, mkwd.14.stm.v0$X..of.buds)
mkwd.14.stm.v0$X..of.buds <- as.numeric(gsub("150\\+", "150", mkwd.14.stm.v0$X..of.buds))
sort(unique(mkwd.14.stm.v0$X..of.buds))

# Fix number of flowers
  ## '13
sort(unique(mkwd.13.stm.v0$X..of.flowers))
    ### Actually correct to start

  ## '14
sort(unique(mkwd.14.stm.v0$X..of.flowers))
mkwd.14.stm.v0$X..of.flowers <- as.numeric(gsub("\\?|accidental row|Accidental row",
                                     NA, mkwd.14.stm.v0$X..of.flowers))
sort(unique(mkwd.14.stm.v0$X..of.flowers))

# Fix bloom status
  ## '13
sort(unique(mkwd.13.stm.v0$Bloom.Status))
mkwd.13.stm.v0$Bloom.Status <- gsub("prob will bloom late summer|in bud|full",
                                    NA, mkwd.13.stm.v0$Bloom.Status)
mkwd.13.stm.v0$Bloom.Status <- as.numeric(gsub("40", "4", mkwd.13.stm.v0$Bloom.Status))
sort(unique(mkwd.13.stm.v0$Bloom.Status))

  ## '14
sort(unique(mkwd.14.stm.v0$Bloom.Status))
mkwd.14.stm.v0$Bloom.Status <- gsub("\\(|\\/|\\?|accidental row|Accidental row|full|in bud|n.a.|prob will bloom late summer",
                                    NA, mkwd.14.stm.v0$Bloom.Status)
mkwd.14.stm.v0$Bloom.Status <- as.numeric(gsub("40", "4", mkwd.14.stm.v0$Bloom.Status))
sort(unique(mkwd.14.stm.v0$Bloom.Status))

# Check that all is right with the world
str(mkwd.13.stm.v0)
str(mkwd.14.stm.v0)
  ## Looks good!

# Now summarize to get averages/totals (as needed) for each plant
  ## 2013 summarization
mkwd.13.stm.v1 <- mkwd.13.stm.v0 %>%
  group_by(PlantNum) %>%
  dplyr::summarise(Avg.Height = mean(Stem.Length..cm.),
            Avg.Bud = mean(X..of.buds),
            Avg.Flr = mean(X..of.flowers),
            Tot.Bud = sum(X..of.buds),
            Tot.Flr = sum(X..of.flowers),
            Tot.Bud.n.Flr = sum(X..of.buds, X..of.flowers),
            Avg.Bloom.Status = mean(Bloom.Status)) %>%
  as.data.frame()

  ## 2014 summarization
mkwd.14.stm.v1 <- mkwd.14.stm.v0 %>%
  group_by(PlantNum) %>%
  dplyr::summarise(Avg.Height = mean(Stem.Length..cm.),
            Avg.Bud = mean(X..of.buds),
            Avg.Flr = mean(X..of.flowers),
            Tot.Bud = sum(X..of.buds),
            Tot.Flr = sum(X..of.flowers),
            Tot.Bud.n.Flr = sum(X..of.buds, X..of.flowers),
            Avg.Bloom.Status = mean(Bloom.Status)) %>%
  as.data.frame()

# Did that work?
str(mkwd.13.stm.v1)
str(mkwd.14.stm.v1)
  ## Looks like it did!

# Now let's just bring that whole mess into the larger data frames from those years
  ## Need to quickly change the index column in the larger dataframe to make them match
mkwd.13.v2$PlantNum <- mkwd.13.v2$PlantNumAuto
mkwd.14.v2$PlantNum <- mkwd.14.v2$PlantNumAuto

# Now bring the data over
mkwd.13.v3 <- left_join(mkwd.13.v2, mkwd.13.stm.v1, by = "PlantNum")
mkwd.14.v3 <- left_join(mkwd.14.v2, mkwd.14.stm.v1, by = "PlantNum")
mkwd.15.v3 <- mkwd.15.v2

# Make sure the switch happened
str(mkwd.13.v3)
str(mkwd.14.v3)
  ## It did!

# Okay, here's where things are going to get interesting
  ## If a given row has an NA *in the tidy data*
  ## We want to replace that NA with the data (if they exist) from that year's file
  ## If there are data (even zeros) then do nothing

# In our (mostly) tidy data, where are there NAs that may have data in other files?
summary(milkweed.v10)
  ## Basically all of them have 1000+ NAs
  ## So let's just do the variables that were consistently collected across the years

## ------------------------------------------------ ##
        # Missing Data Retrieval Actual ####
## ------------------------------------------------ ##
# Make a new dataframe in case something goes wrong
milkweed.v11 <- milkweed.v10

# Let's go variable by variable
  ## This will allow for easy fixes if my guess of a column's contents was wrong
  ## Guessing is only necessary because of column abbreviations whose defs have been lost to time

# For each of the following:
  ## 1) Check the number of NAs before attempting the "fix"
  ## 2) Add each year's data
  ## 3) Double check the number of NAs

# Fix average height
summary(milkweed.v11$Avg.Height)
milkweed.v11$Avg.Height <- ifelse(test = is.na(milkweed.v11$Avg.Height) == T,
                                  yes = mkwd.13.v3$Avg.Height[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                  no = milkweed.v11$Avg.Height)
milkweed.v11$Avg.Height <- ifelse(test = is.na(milkweed.v11$Avg.Height) == T,
                                  yes = mkwd.14.v3$Avg.Height[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                  no = milkweed.v11$Avg.Height)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Avg.Height) # 1021 NAs fixed

# Fix average number of buds
summary(milkweed.v11$Avg.Bud)
milkweed.v11$Avg.Bud <- ifelse(test = is.na(milkweed.v11$Avg.Bud) == T,
                                  yes = mkwd.13.v3$Avg.Bud[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                  no = milkweed.v11$Avg.Bud)
milkweed.v11$Avg.Bud <- ifelse(test = is.na(milkweed.v11$Avg.Bud) == T,
                                  yes = mkwd.14.v3$Avg.Bud[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                  no = milkweed.v11$Avg.Bud)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Avg.Bud) # 991 NAs fixed

# Fix average number of flowers
summary(milkweed.v11$Avg.Flr)
milkweed.v11$Avg.Flr <- ifelse(test = is.na(milkweed.v11$Avg.Flr) == T,
                               yes = mkwd.13.v3$Avg.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Avg.Flr)
milkweed.v11$Avg.Flr <- ifelse(test = is.na(milkweed.v11$Avg.Flr) == T,
                               yes = mkwd.14.v3$Avg.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Avg.Flr)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Avg.Flr) # 1019 NAs fixed

# Fix total number of buds
summary(milkweed.v11$Tot.Bud)
milkweed.v11$Tot.Bud <- ifelse(test = is.na(milkweed.v11$Tot.Bud) == T,
                               yes = mkwd.13.v3$Tot.Bud[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Bud)
milkweed.v11$Tot.Bud <- ifelse(test = is.na(milkweed.v11$Tot.Bud) == T,
                               yes = mkwd.14.v3$Tot.Bud[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Bud)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Tot.Bud) # 991 NAs fixed

# Fix total number of flowers
summary(milkweed.v11$Tot.Flr)
milkweed.v11$Tot.Flr <- ifelse(test = is.na(milkweed.v11$Tot.Flr) == T,
                               yes = mkwd.13.v3$Tot.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Flr)
milkweed.v11$Tot.Flr <- ifelse(test = is.na(milkweed.v11$Tot.Flr) == T,
                               yes = mkwd.14.v3$Tot.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Flr)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Tot.Flr) # 1019 NAs fixed

# Fix total number of buds AND flowers
summary(milkweed.v11$Tot.Bud.n.Flr)
milkweed.v11$Tot.Bud.n.Flr <- ifelse(test = is.na(milkweed.v11$Tot.Bud.n.Flr) == T,
                               yes = mkwd.13.v3$Tot.Bud.n.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Bud.n.Flr)
milkweed.v11$Tot.Bud.n.Flr <- ifelse(test = is.na(milkweed.v11$Tot.Bud.n.Flr) == T,
                               yes = mkwd.14.v3$Tot.Bud.n.Flr[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                               no = milkweed.v11$Tot.Bud.n.Flr)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Tot.Bud.n.Flr) # 988 NAs fixed

# Fix average bloom status
summary(milkweed.v11$Avg.Bloom.Status)
milkweed.v11$Avg.Bloom.Status <- ifelse(test = is.na(milkweed.v11$Avg.Bloom.Status) == T,
                                     yes = mkwd.13.v3$Avg.Bloom.Status[match(milkweed.v11$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                     no = milkweed.v11$Avg.Bloom.Status)
milkweed.v11$Avg.Bloom.Status <- ifelse(test = is.na(milkweed.v11$Avg.Bloom.Status) == T,
                                     yes = mkwd.14.v3$Avg.Bloom.Status[match(milkweed.v11$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                     no = milkweed.v11$Avg.Bloom.Status)
# 2015 excluded due to lack of pre-existing column (each stem's data included though if needed)
summary(milkweed.v11$Avg.Bloom.Status) # 1005 NAs fixed

# This is (roughly) the halfway point so make a new dataframe
milkweed.v12 <- milkweed.v11

# Fix number of budding stems
summary(milkweed.v12$Num.Stems.Budding)
milkweed.v12$Num.Stems.Budding <- ifelse(test = is.na(milkweed.v12$Num.Stems.Budding) == T,
                                        yes = mkwd.13.v3$TRIMBStemsBUD[match(milkweed.v12$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                        no = milkweed.v12$Num.Stems.Budding)
milkweed.v12$Num.Stems.Budding <- ifelse(test = is.na(milkweed.v12$Num.Stems.Budding) == T,
                                        yes = mkwd.14.v3$TRIMBSBUD[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                        no = milkweed.v12$Num.Stems.Budding)
milkweed.v12$Num.Stems.Budding <- ifelse(test = is.na(milkweed.v12$Num.Stems.Budding) == T,
                                         yes = mkwd.15.v3$TRIMBSBUD[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                         no = milkweed.v12$Num.Stems.Budding)
milkweed.v12$Num.Stems.Budding <- as.numeric(milkweed.v12$Num.Stems.Budding)
summary(milkweed.v12$Num.Stems.Budding) # 586 (of 643) NAs fixed

# Fix number of flowering stems
summary(milkweed.v12$Num.Stems.Flowering)
milkweed.v12$Num.Stems.Flowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Flowering) == T,
                                         yes = mkwd.13.v3$TRIMBStemsFLOW[match(milkweed.v12$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                         no = milkweed.v12$Num.Stems.Flowering)
milkweed.v12$Num.Stems.Flowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Flowering) == T,
                                         yes = mkwd.14.v3$TRIMBSFLOW[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                         no = milkweed.v12$Num.Stems.Flowering)
milkweed.v12$Num.Stems.Flowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Flowering) == T,
                                         yes = mkwd.15.v3$TRIMBSFLOW[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                         no = milkweed.v12$Num.Stems.Flowering)
milkweed.v12$Num.Stems.Flowering <- as.numeric(milkweed.v12$Num.Stems.Flowering)
summary(milkweed.v12$Num.Stems.Flowering) # 586 (of 643) NAs fixed

# Fix number of stems post flowering (i.e., senesced)
summary(milkweed.v12$Num.Stems.PostFlower)
milkweed.v12$Num.Stems.PostFlower <- ifelse(test = is.na(milkweed.v12$Num.Stems.PostFlower) == T,
                                           yes = mkwd.13.v3$TRIMBStemsDONE[match(milkweed.v12$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                           no = milkweed.v12$Num.Stems.PostFlower)
milkweed.v12$Num.Stems.PostFlower <- ifelse(test = is.na(milkweed.v12$Num.Stems.PostFlower) == T,
                                           yes = mkwd.14.v3$TRIMBSDONE[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                           no = milkweed.v12$Num.Stems.PostFlower)
milkweed.v12$Num.Stems.PostFlower <- ifelse(test = is.na(milkweed.v12$Num.Stems.PostFlower) == T,
                                           yes = mkwd.15.v3$TRIMBSDONE[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                           no = milkweed.v12$Num.Stems.PostFlower)
milkweed.v12$Num.Stems.PostFlower <- as.numeric(milkweed.v12$Num.Stems.PostFlower)
summary(milkweed.v12$Num.Stems.PostFlower) # 586 (of 643) NAs fixed

# Fix number of non-flowering stems
summary(milkweed.v12$Num.Stems.Nonflowering)
milkweed.v12$Num.Stems.Nonflowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Nonflowering) == T,
                                            yes = mkwd.13.v3$TRIMBStemsNOflow[match(milkweed.v12$Temp.Plant.Code, mkwd.13.v3$Temp.Plant.Code)],
                                            no = milkweed.v12$Num.Stems.Nonflowering)
milkweed.v12$Num.Stems.Nonflowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Nonflowering) == T,
                                            yes = mkwd.14.v3$TRIMBS.NOflow[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                            no = milkweed.v12$Num.Stems.Nonflowering)
milkweed.v12$Num.Stems.Nonflowering <- ifelse(test = is.na(milkweed.v12$Num.Stems.Nonflowering) == T,
                                            yes = mkwd.15.v3$TRIMBS.NOflow[match(milkweed.v12$Temp.Plant.Code, mkwd.14.v3$Temp.Plant.Code)],
                                            no = milkweed.v12$Num.Stems.Nonflowering)
milkweed.v12$Num.Stems.Nonflowering <- as.numeric(milkweed.v12$Num.Stems.Nonflowering)
summary(milkweed.v12$Num.Stems.Nonflowering) # 586 (of 643) NAs fixed

## ------------------------------------------------ ##
            # "New" Data Checking ####
## ------------------------------------------------ ##
# As the previous 2000+ lines made abundantly clear, this raw data is remarkably error prone
  ## So, we now need to re-check all of our "new" columns again before calling it a day

# Plant ID code
sort(unique(milkweed.v12$Plant.ID))
  ## Still don't want to open this can of worms so I'll leave it alone for now

# Average plant height
sort(unique(milkweed.v12$Avg.Height))
  ## Looks good!

# Average bud number
sort(unique(milkweed.v12$Avg.Bud))
  ## LGTM (looks good to me)

# Average flower number
sort(unique(milkweed.v12$Avg.Flr))
  ## LGTM

# Total number buds
sort(unique(milkweed.v12$Tot.Bud))

# Total flowers
sort(unique(milkweed.v12$Tot.Flr))

# Total buds and flowers
sort(unique(milkweed.v12$Tot.Bud.n.Flr))

# Average bloom status
sort(unique(milkweed.v12$Avg.Bloom.Status))

# Number of budding stems
sort(unique(milkweed.v12$Num.Stems.Budding))

# Number of flowering stems
sort(unique(milkweed.v12$Num.Stems.Flowering))

# Number of stems post flowering
sort(unique(milkweed.v12$Num.Stems.PostFlower))

# Number of nonflowering stems
sort(unique(milkweed.v12$Num.Stems.Nonflowering))

# Need to recalculate the number of stems of all flowering stages (now that some NAs are resolved)
  ## Recalculate
summary(milkweed.v12$Num.Stems.ALL.Flowering.Stages)
milkweed.v12$Num.Stems.ALL.Flowering.Stages <- ifelse(test = is.na(milkweed.v12$Num.Stems.ALL.Flowering.Stages) == T,
                                                   yes = rowSums(select(milkweed.v12, Num.Stems.Budding:Num.Stems.PostFlower)),
                                                   no = milkweed.v12$Num.Stems.ALL.Flowering.Stages)
summary(milkweed.v12$Num.Stems.ALL.Flowering.Stages) # 586 NAs fixed

  ## Now, check the contents of that column
sort(unique(milkweed.v12$Num.Stems.ALL.Flowering.Stages))

# For ease of recalculating the ratio of bitten to total stems, lets get a column for total stems
milkweed.v12$Tot.Stems <- rowSums(select(milkweed.v12, Num.Stems.ALL.Flowering.Stages, Num.Stems.Nonflowering))
sort(unique(milkweed.v12$Tot.Stems))
  ## Looks good!

# Now recalculate & check the ratio column
  ## Recalculate
summary(milkweed.v12$Ratio.Bitten.vs.Total.Stems)
milkweed.v12$Ratio.Bitten.vs.Total.Stems <- ifelse(test = is.na(milkweed.v12$Ratio.Bitten.vs.Total.Stems) == T,
                                                   yes = (milkweed.v12$Tot.Bitten.Stems / milkweed.v12$Tot.Stems),
                                                   no = milkweed.v12$Ratio.Bitten.vs.Total.Stems)
summary(milkweed.v12$Ratio.Bitten.vs.Total.Stems) # fixed 12 NAs (not as effective...)

  ## Check
sort(unique(milkweed.v12$Ratio.Bitten.vs.Total.Stems))
milkweed.v12$Ratio.Bitten.vs.Total.Stems <- gsub("Inf|NaN", NA,
                                                 milkweed.v12$Ratio.Bitten.vs.Total.Stems)
milkweed.v12$Ratio.Bitten.vs.Total.Stems <- ifelse(test = milkweed.v12$Ratio.Bitten.vs.Total.Stems > 1,
                                                   yes = NA,
                                                   no = milkweed.v12$Ratio.Bitten.vs.Total.Stems)
milkweed.v12$Ratio.Bitten.vs.Total.Stems <- as.numeric(milkweed.v12$Ratio.Bitten.vs.Total.Stems)
sort(unique(milkweed.v12$Ratio.Bitten.vs.Total.Stems))

## ------------------------------------------------ ##
            # Monarch Adult Tidying ####
## ------------------------------------------------ ##
# Load in the full tidying data from the other part of the GRG study
grg.bfly.v0 <- read_excel("./Data/2007-2018 GRG INVERTS MASTER DATA.xlsx",
                          sheet = "Butterfly")
  ## Note this file won't be in the Git repo because we...
  ## ...have not (yet) published on it 

# Remind ourselves what sites and years we want monarchs from
sort(unique(milkweed.v13$Year))
sort(unique(milkweed.v13$Site))

# What's in the dataframe
sort(unique(grg.bfly.v0$Year))
sort(unique(grg.bfly.v0$Site))

# Filter the big butterfly dataset to just monarchs
  ## on the sites measured for this project
grg.bfly.v1 <- grg.bfly.v0 %>%
  ## 2012-16 only
  filter(Year >= 12 & Year <= 16) %>%
  ## same 9 sites as in Moranz project
  filter(Site == "GIL" | Site == "LTR" | Site == "PAW" | 
           Site == "PYN" | Site == "PYS" | Site == "PYW" | 
           Site == "RCH" | Site == "RIN" | Site == "RIS" ) %>%
  ## Monarchs only
  filter(Butterfly.Common.Name == "monarch") %>%
  ## Only needed columns
  select(Year, Site, Patch, Round, Month, Date,
         Butterfly.Common.Name, Number) %>%
  ## Return as data frame
  as.data.frame()

# Examine product
str(grg.bfly.v1)

# Aggregate through monarch to get a per patch total
grg.bfly.v2 <- aggregate(Number ~ Year + Site + Patch + Round +
                           Month + Date + Butterfly.Common.Name, 
                         data = grg.bfly.v1, FUN = sum)

# Examine
str(grg.bfly.v2)

# Get this into a better column format
grg.bfly.v3 <- grg.bfly.v2 %>%
  ## Get a the number column more intuitively named
  dplyr::mutate(
    Monarch.Adults = Number,
    Pasture.Patch = Patch,
    Pasture = Site,
    Year = as.numeric(paste0("20", grg.bfly.v2$Year)),
    ) %>%
  ## Remove now-redundant "monarch" column and "number" column
  select(-Butterfly.Common.Name, -Number) %>%
  ## Get metadata
  left_join(mgmt.index, by = c("Year", "Pasture", "Pasture.Patch")) %>%
  ## Get rid of our re-named duplicate columns
  dplyr::mutate(
    Patch = coalesce(Patch.x, Patch.y, Pasture.Patch)
  ) %>%
  ## Keep only relevant columns
  select(Year, Site, Date, TSF, Stocking.Type, Monarch.Adults) %>%
  ## Return dataframe
  as.data.frame()

# Examine
head(grg.bfly.v3)

# Make a new grazing column
grg.bfly.v3$Grazing <- grg.bfly.v3$Stocking
grg.bfly.v3$Grazing <- gsub("IES", "Intensive Early", grg.bfly.v3$Grazing)
grg.bfly.v3$Grazing <- gsub("SLS", "Season Long", grg.bfly.v3$Grazing)
sort(unique(grg.bfly.v3$Grazing))

# Get Julian day
grg.bfly.v4 <- left_join(grg.bfly.v3, julian.index, by = "Date")
head(grg.bfly.v4)

## ------------------------------------------------ ##
             # Final Tidying Steps ####
## ------------------------------------------------ ##
# Remove the temporary plant code column we needed from earlier
milkweed.v13 <- milkweed.v12 %>%
  ## Remove that column while we're here
  select(-Temp.Plant.Code) %>%
  as.data.frame()

# Somehow there are duplicate rows that need removing here
  ## "Duplicate" as in every single row is duplicated
  ## It gets worse:
  ## Some of the response variables are duplicated across ostensibly different patches
  ## To fix this, let's strip out the patch codes and plant IDs (neither is used anyway)
  ## This is almost certainly an artifact of combining the different years separately earlier
    ### See "Missing Data Retrieval Prep" & "... Actual"
milkweed.v14 <- milkweed.v13 %>%
  select(-Plant.ID, -Patch) %>%
  unique()

# Save the tidy data to use for analysis later
write_xlsx(list(Data = milkweed.v14),
           path = "./Data/Asclepias-TIDY.xlsx",
           col_names = T, format_headers = T)

# Save the monarch adult data as well
write_xlsx(list(Data = grg.bfly.v4),
           path = "./Data/Monarch-Adult-TIDY.xlsx",
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
  ## Audrey McCombs
  ## Chaz Abarr
  ## Gatha [last name?]
  ## Jake Mortensen
  ## Jessica Williams
  ## John Delaney
  ## Karin Grimlund
  ## Marina Osier
  ## Shannon Rusk
  ## Veronica Mecko [Meeko?]

# END ####

