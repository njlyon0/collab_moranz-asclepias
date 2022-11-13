## ---------------------------------------------------------- ##
          # Moranz et al. Asclepias tuberosa Project
## ---------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ----
## This code wrangles mostly untidy but harmonized data into truly tidy data

## ------------------------------------------------ ##
                    # Housekeeping ----
## ------------------------------------------------ ##

# Call any needed libraries here (good to centralize this step)
# install.packages("librarian")
librarian::shelf(tidyverse, njlyon0/helpR)

# Clear the environment
rm(list = ls())

# Read in data
mkwd_v1 <- read.csv(file = file.path("tidy_data", "Asclepias-HARMONIZED.csv"))

# Glimpse this!
dplyr::glimpse(mkwd_v1)

## ------------------------------------------------ ##
        # Handle Multi-Stem Observations ----
## ------------------------------------------------ ##

# We want to wrangle the per stem observations into averages across the three
mkwd_v2 <- mkwd_v1 %>%
  # Mutate stem columns into characters so we can combine them
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(".StObs"), .fns = as.character)) %>%
  # Add an id column to use for pivoting back
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  # Pivot to long format to be able to fix this more easily
  tidyr::pivot_longer(cols = dplyr::contains(".StObs"),
                      names_to = "stem_metric",
                      values_to = "stem_value")

# Take a look
dplyr::glimpse(mkwd_v2)

# Check for non-numbers in the value column
helpR::num_chk(data = mkwd_v2, col = "stem_value")
# Also existing multi-stem summarization columns
helpR::multi_num_chk(data = mkwd_v2,
                     col_vec = c("Avg.Height", "Avg.Bloom.Status"))

# Fix those issues (note judgement calls)
mkwd_v3 <- mkwd_v2 %>%
  dplyr::mutate(
    stem_value = dplyr::case_when(
      stem_value %in% c("not meas", "not counted", "no data",
                        "not recorded", "skipped", "?") ~ "",
      stem_value %in% c("too early", "to early", "tiny") ~ "0",
      stem_value == "dozens" ~ "24",
      stem_value == "35(dead)" ~ "35",
      TRUE ~ stem_value)) %>%
  dplyr::mutate(
    Avg.Height = ifelse(
      Avg.Height == "no data",
      yes = "", no = Avg.Height),
    Avg.Bloom.Status = ifelse(
      Avg.Bloom.Status == "unknown",
      yes = "", no = Avg.Bloom.Status))

# Check to see that they are resolved
helpR::num_chk(data = mkwd_v3, col = "stem_value")
helpR::multi_num_chk(data = mkwd_v3,
                     col_vec = c("Avg.Height", "Avg.Bloom.Status"))

# Now we can move on
mkwd_v4 <- mkwd_v3 %>%
  # Make that column numeric
  dplyr::mutate(stem_value = as.numeric(stem_value),
                Avg.Height = as.numeric(Avg.Height),
                Avg.Bloom.Status = as.numeric(Avg.Bloom.Status)) %>%
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = stem_metric,
                     values_from = stem_value) %>%
  # Drop row ID column
  dplyr::select(-row_id) %>%
  # Calculate summaries
  dplyr::rowwise() %>%
  dplyr::mutate(
    ## Average height
    Avg.Height_2 = mean(
      dplyr::c_across(cols = dplyr::contains("Length.StObs")),
      na.rm = TRUE),
    ## Average buds
    Avg.Bud_2 = mean(
      dplyr::c_across(cols = dplyr::contains("Buds.StObs")),
      na.rm = TRUE),
    ## Total buds
    bud_na = sum(is.na(
      dplyr::c_across(cols = dplyr::contains("Buds.StObs")))),
    Tot.Bud_2 = ifelse(test = bud_na == 3,
                     yes = NA,
                     no = sum(dplyr::c_across(
                       cols = dplyr::contains("Buds.StObs")),
                       na.rm = TRUE)),
    ## Average flowers
    Avg.Flr_2 = mean(
      dplyr::c_across(cols = dplyr::contains("Flow.StObs")),
      na.rm = TRUE),
    ## Total flowers
    flow_na = sum(is.na(
      dplyr::c_across(cols = dplyr::contains("Flow.StObs")))),
    Tot.Flr_2 = ifelse(test = bud_na == 3,
                     yes = NA,
                     no = sum(dplyr::c_across(
                       cols = dplyr::contains("Flow.StObs")),
                       na.rm = TRUE)),
    ## Average bloom status
    Avg.Bloom.Status_2 = mean(
      dplyr::c_across(cols = dplyr::contains("BlooStatus.StObs")),
      na.rm = TRUE)) %>%
  # Drop unwanted columns
  dplyr::select(-dplyr::contains(".StObs"), -bud_na, -flow_na) %>%
  # Coalesce these calculated columns with their directly-recorded parallels
  dplyr::mutate(
    ## Prioritize one we just calculated
    Avg.Height = dplyr::coalesce(Avg.Height_2, Avg.Height),
    Avg.Bud = dplyr::coalesce(Avg.Bud_2, Avg.Bud),
    Tot.Bud = dplyr::coalesce(Tot.Bud_2, Tot.Bud),
    Avg.Flr = dplyr::coalesce(Avg.Flr_2, Avg.Flr),
    Tot.Flr = dplyr::coalesce(Tot.Flr_2, Tot.Flr),
    Avg.Bloom.Status = dplyr::coalesce(Avg.Bloom.Status_2, Avg.Bloom.Status)) %>%
  # Drop calculated columns now that they are integrated where necessary
  dplyr::select(-dplyr::ends_with("_2"))

# Glimpse it
dplyr::glimpse(mkwd_v4)

# See how many observations this fixed!
summary(dplyr::select(mkwd_v3, Avg.Height:Avg.Bloom.Status))
summary(dplyr::select(mkwd_v4, Avg.Height:Avg.Bloom.Status))

# Check to make sure we didn't lose any wanted columns
helpR::diff_chk(old = names(mkwd_v3), new = names(mkwd_v4))

## ------------------------------------------------ ##
             # Monarch Info Wrangling ----
## ------------------------------------------------ ##
# Monarch immatures checks
sort(unique(mkwd_v4$MonarchImmatures2))
## This combines eggs and larvae (caterpillars) but we want them separated

# Let's handle that here
mkwd_v5 <- mkwd_v4 %>%
  # Standardize the format of every qualitative entry
  dplyr::mutate(immatures_simp = dplyr::case_when(
    ## Format is # egg_# larvae_evidence
    MonarchImmatures2 %in% c(
      "", "did not assess", "did not measure", "maybe", "n/a; no",
      "maybe frass", "maybe; 5 buds chewed by small invert", 
      "maybe; some buds have been chewed on", 
      "maybe; some insect has been eating some buds (in fashion of monarch larva)",
      "na", "no", "No", "NO", "no (could not find plant!)", "no data",
      "no; frass found on leaves but not of monarch",
      "presum no", "trans", "unk (not recorded)", "not recorded",
      "n/a; no; caterpillar poop on stem, some missing buds (eaten?)"
    ) ~ "__",
    ## 0 eggs & 0 larvae
    MonarchImmatures2 %in% c("0", "0-? See comment 4") ~ "0_0_0",
    ## evidence but no observation
    MonarchImmatures2 %in% c(
      "frass",  "yes" , "YES", "yes (frass)",
      "monarch larva(e) had chewed leaves, pods",
      "yes (frass on stem, some buds eaten away)",
      "yes; lots of leaves missing/chewed up by insects, and some large monarch frass; did not see larvae",
      "yes; small monarch frass on each stem; some buds eaten away"
      ) ~ "0_0_1",
    ## maximum 1 of either
    MonarchImmatures2 %in% c(
      "1 egg", "1 egg on leaves near stem", "1 monarch egg",
      "1 egg on leaf, plus evidence of herbivory by monarch larvae",
      "1 monarch egg on a bud", "yes (1 monarch egg on bud of stem C)",
      "1 monarch egg, plus evidence of other larvae on the stem",
      "yes; 1 egg on 1cm long leaf next to buds on stem A", 
      "yes; 1 monarch egg on bud",
      "yes; 1 white monarch egg on fresh leaf that subtends the bud cluster of stem C",
      "yes; monarch egg on stem A; frass on stem B and C",
      "yes; monarch egg on upper surface of a leaf approx. 8\" off ground on stem A (and 10\" below flower)",
      "yes; Strangely, one egg on upper surface of leaf (10\" below flowers)"
      ) ~ "1_0_1",
    MonarchImmatures2 %in% c(
      "1 3rd Instar larvae", "yes (1 4th instar)", "yes (1 5th instar)",
      "yes (3 4th or 5th instar)", "yes; 1 5th instar",
      "yes; 1 fourth instar",
      "yes; 1 fifth instar monarch larvae feeding on stem A when we arrived"
      ) ~ "0_1_1",
    MonarchImmatures2 %in% c(
      "1 3rd Instar Iarva, 1 monarch egg, evidence of other larvae.",
      "1 5th instar larva on 3rd tallest stem. 1 monarch egg on flower bud.",
      "1 dead 5th instar larva at base of the plant, plus 1 monarch egg on bud. ! Live 5th instar ot base of the plant",
      "1 larva (1st instar resting on bud stalk on Stem A. One egg on big leaf on Stem B.",
      "1 egg, 1 larva") ~ "1_1_1",
    ## maximum 2 of either
    MonarchImmatures2 %in% c(
      "1 4th instar near base, 1 2nd instar eating flowers",
      "2 5th Instar larvae", "2 3rd instar larvae on flower buds",
      "yes, 1 small caterpillar on smallest blooming axillary shoot; 1 4th instar (?) caterpillar hiding in grass next to tallest stem; large amounts of poop",
      "yes; at 7:50am, found 2 large 5th instar monarch feeding on plant; larvae feeding on stem A, the tallest stem; at 8:45am they were still on that stem and were feeding off different edges of the same leaf"
      ) ~ "0_2_1",
    MonarchImmatures2 %in% c(
      "1 egg on leaf next to buds on Stem C; 1 egg on leaf near top of Stem A (no buds)",
      "2 eggs", "2 hatched monarch eggs on bud",
      "2 monarch eggs", "yes, 2 monarch eggs",
      "yes, 2 dark monarch eggs (both on flower buds)",
      "yes; 1 white monarch egg, 1 dark gray monarch egg, both laid on flower buds",
      "yes; two monarch eggs on stem C (specifically on underside of leaves, 10cm and 22cm from top of plant)",
      # Note judgement call (vvv)
      "yes; eggs on leaf of C stem") ~ "2_0_1",
    MonarchImmatures2 == "2 eggs, 2 2nd Instar larvae" ~ "2_2_1",
    ## Max three
    MonarchImmatures2 %in% c("3 monarch eggs, all on Stem A.",
                             "yes; 1 monarch egg on flower bud of an axillary shoot; another monarch egg on red tipped flower bud on stem B (another resprout) and large frass (potentially monarch frass); third monarch egg also on red tipped buds of an axillary shoot on stem C",
                             "yes; 1 monarch egg per stem, each egg laid on back of leaf within 2.5cm of bud cluster",
                             "yes; 3 monarch eggs, all laid on young leaves within 2cm of flower buds"
                             ) ~ "3_0_1",
    MonarchImmatures2 %in% c("found 3  2nd instar monarch larva on stem B and 5 on stem C. They were feeding on buds. Both have mixture of small green buds and red buds.",
    "yes; three 4th or 5th instar",
    "yes; three second instar monarch larvae, one on each of the stems, all three are eating flower buds (and were somewhat hidden in the cluster of buds); all three stems are the most advanced phenologically for this plant (all have a few red tipped buds)"
    ) ~ "0_3_1",
    ## 4 - 6
    MonarchImmatures2 %in% c("yes; 4 monarch eggs total (2 on taller stems - both on young leaves), (2 on shorter stems  - 1 on stem, 1 on young leaves)",
                             "yes; 4 monarch eggs, 1 for each stem, each one on underside of leaf between 1/2\" and 2\" below bud cluster"
                             ) ~ "4_0_1",
    MonarchImmatures2 %in% c("5 monarch eggs",
                             "yes; 5 monarch eggs plus small monarch frass)") ~ "5_0_1",
    ## Other amounts
    MonarchImmatures2 == "yes; 2 monarch eggs on buds. 1 monarch larva (2nd instar) in buds. Frass from large 5th instar larva." ~ "2_1_1",
    MonarchImmatures2 == "2 2nd instars on Stem A (eating buds), 1 egg on bud of Stem B, 1 2nd instar and 1 egg on Stem C (on buds)" ~ "2_3_1",
    MonarchImmatures2 == "10 monarch eggs (7 eggs on buds, 3 on leaves)" ~ "10_0_1",
    TRUE ~ "ISSUE__")) %>%
  # Split that fixed column apart
  tidyr::separate(col = immatures_simp, sep = "_", remove = T,
                  into = c("eggs", "larvae", "evidence")) %>%
  # Make those columns numeric
  dplyr::mutate(Num.Monarch.Eggs = as.numeric(eggs),
                Num.Monarch.Larvae = as.numeric(larvae),
                Monarch.Immature.Evidence = as.numeric(evidence)) %>%
  # Calculate total immatures where at least one of either was found
  dplyr::rowwise() %>%
  dplyr::mutate(
    Tot.Monarch.Immatures = ifelse(
      test = (!is.na(Num.Monarch.Eggs) | !is.na(Num.Monarch.Larvae)),
      yes = sum(
        dplyr::c_across(cols = Num.Monarch.Eggs:Num.Monarch.Larvae),
        na.rm = TRUE),
      no = NA))

# Make sure we fixed all possible comments
mkwd_v5 %>%
  dplyr::filter(eggs == "ISSUE") %>%
  select(MonarchImmatures2) %>%
  unique()

# Glimpse data
dplyr::glimpse(mkwd_v5)

## ------------------------------------------------ ##
              # Streamline Columns ----
## ------------------------------------------------ ##
# Time to pare down columns
mkwd_v6 <- mkwd_v5 %>%
  # Add Whittaker number to patch
  dplyr::mutate(Patch = paste0(Patch, "-", Whittaker)) %>%
  # Pare down to desired columns
  dplyr::select(Year, Date, Site, Patch, Plant.ID,
                Avg.Height, Avg.Bud, Avg.Flr, Tot.Bud,
                Tot.Flr, Avg.Bloom.Status, 
                Num.Stems.Budding, Num.Stems.Flowering,
                Num.Stems.PostFlower, Num.Stems.Nonflowering,
                Num.Unbit.Stems.w.Axillary.Shoots,
                ASCTUB.Abun.1m, ASCTUB.Abun.2m, 
                Crab.Spider.Abun, GrazingLawn, Shrub.Abun.1m,
                Tot.Bitten.Stems, Num.Flowering.Stems.Bitten,
                Num.Bitten.Stems.w.Axillary.Shoots, Tot.Axillary.Shoots, 
                Num.Axillary.Shoots.Bitten, Num.Monarch.Eggs,
                Num.Monarch.Larvae, Monarch.Immature.Evidence,
                Tot.Monarch.Immatures)

# Check what was lost / gained
helpR::diff_chk(old = names(mkwd_v5), new = names(mkwd_v6))

# Glimpse this
dplyr::glimpse(mkwd_v6)

## ------------------------------------------------ ##
          # Site & Date Format Wrangling ----
## ------------------------------------------------ ##
# Now more wrangling
mkwd_v7 <- mkwd_v6 %>%
  # Make year into a number
  dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
  # Handle the issues with the date column
  dplyr::mutate(
    # Standardize format of date
    Date_2 = ifelse(stringr::str_sub(string = Date,
                                    start = 1, end = 4) == "2012",
                   yes = stringr::str_sub(string = Date,
                                          start = 7, end = 10),
                   no = paste0(stringr::str_sub(string = Date,
                                                start = 4, end = 6), "-",
                               stringr::str_sub(string = Date,
                                                start = 1, end = 2)))
    ) %>%
  # Handle weirdness with month
  dplyr::mutate(
    Date_3 = gsub(pattern = "Jul", replacement = "7", x = Date_2),
    Date_4 = gsub(pattern = "Jun", replacement = "6", x = Date_3)) %>%
  # Finish with date!
  dplyr::mutate(Date = as.numeric(gsub(pattern = "-", replacement = ".",
                                       x = Date_4))) %>%
  # Drop intermediary columns
  dplyr::select(-dplyr::starts_with("Date_")) %>%
  # Simplify site names
  dplyr::mutate(Site = dplyr::case_when(
    Site == "Gilleland" ~ "GIL", 
    Site == "Lee Trail Rd" ~ "LTR", 
    Site == "Pawnee Prairie" ~ "PAW", 
    Site == "Pyland North" ~ "PYN", 
    Site == "Pyland South" ~ "PYS", 
    Site == "Pyland West" ~ "PYW", 
    Site == "Richardson" ~ "RCH", 
    Site == "Ringgold North" ~ "RIN", 
    Site == "Ringgold South" ~ "RIS")) %>%
  # Fix some patch information that is contained in the plant ID column
  dplyr::mutate(Patch = dplyr::case_when(
    ## Some have correct information in Plant ID column
    Patch %in% c("East and Center-1 &2", "East and Center-NA",
                 "East and Center-",
                 "multiple-2", "North & Center-1 & 2") ~ Plant.ID,
    TRUE ~ Patch)) %>%
  # Simplify all patches
  dplyr::mutate(Patch = dplyr::case_when(
    ## North patches
    Patch %in% c("N1", "N2", "N1-", "N2-", "North-1", "North-1 and 2", 
                 "North-1and2", "North-2", "North-2 and 3", 
                 "North-N1W1", "North-N2W2", "GILN1001") ~ "N",
    ## South patches
    Patch %in% c("S1", "S2", "S3", "S1-", "S2-", "S3-", 
                 "South-1", "South-1 & 2", 
                 "South-1 and 2", "South-1 and 3", "South-1,2", 
                 "South-1and2", "South-1and3", "South-2", 
                 "South-3") ~ "S",
    ## East patches
    Patch %in% c("E1", "E2", "E1-", "E2-", "East-1", "East-2", "East-1 & 2",
                 "LTRE1001", "LTRE2001", "LTRE2002", "LTRE2003",
                 "LTRE2004") ~ "E",
    ## West patches
    Patch %in% c("W1", "W2", "W3", "West-1", "W1-", "W2-", "W3-", 
                 "West-", "West-1 and 2", 
                 "West-2", "West-NA", "Y-1", "LTRW2001") ~ "W",
    ## Center patches
    Patch %in% c("C1", "C2", "C3", "C1-", "C2-", "C3-", 
                 "Center-1", "Center-1 and 2",
                 "Center-1,2,3", "Center-2", "Center-3",
                 "GIL-C-2013-901", "GIL-C-2013-902", 
                 "GIL-C1-13-R1-001", "GILC1001", "GILC1002", 
                 "GILC1003", "LTRC1001", "LTRC2001", "LTRC2003",
                 "LTRC2007", "LTRC2008", "LTRC2009", "LTRC2015") ~ "C",
    # Fix blank too
    Patch == "NA-NA" ~ "",
    # If not specified, keep old entry
    TRUE ~ Patch)) %>%
  # Filter out accidental rows
  dplyr::filter(!Plant.ID %in% c("(accidental row)", "accidental row",
                                 "Accidental row", "ACCIDENTAL ROW"))

# Glimpse this
dplyr::glimpse(mkwd_v7)

# Check out remaining contents
sort(unique(mkwd_v7$Date))
sort(unique(mkwd_v7$Site))
sort(unique(mkwd_v7$Patch))

## ------------------------------------------------ ##
        # Numeric Column Checks & Fixes ----
## ------------------------------------------------ ##
# Check for some bad entries (we'll resolve them shortly)
helpR::multi_num_chk(data = mkwd_v7, col_vec = c(
  "Num.Stems.Budding", "Num.Stems.Flowering", 
  "Num.Stems.PostFlower" , "Num.Stems.Nonflowering", "Tot.Bitten.Stems",
  "Num.Unbit.Stems.w.Axillary.Shoots", 
  "ASCTUB.Abun.1m", "ASCTUB.Abun.2m", "Crab.Spider.Abun",
  "Shrub.Abun.1m", "Tot.Bitten.Stems", "Num.Flowering.Stems.Bitten",
  "Num.Bitten.Stems.w.Axillary.Shoots", "Tot.Axillary.Shoots", 
  "Num.Axillary.Shoots.Bitten"))

# Address these issues
mkwd_v8 <- mkwd_v7 %>%
  # Fix problematic entries
  dplyr::mutate(
    # Total bitten stems
    Tot.Bitten.Stems = dplyr::case_when(
      Tot.Bitten.Stems %in% c("unk", "unk.", "unknown", "na", 
                              "unk (plant gone", "skipped",
                              "unk (likely they've atrophied)",
                              "unk (probably had stems that were eaten and atrophied)") ~ "",
      Tot.Bitten.Stems == "at least 4" ~ "4",
      TRUE ~ Tot.Bitten.Stems)) %>%
  # Budding stems
  dplyr::mutate(Num.Stems.Budding = dplyr::case_when(
    Num.Stems.Budding %in% c("no sign of plant", "could not find plant",
                             "skipped", "could not find") ~ "",
    TRUE ~ Num.Stems.Budding)) %>%
  # Flowering stems
  dplyr::mutate(Num.Stems.Flowering = dplyr::case_when(
    Num.Stems.Flowering == "skipped" ~ "",
    TRUE ~ Num.Stems.Flowering)) %>%
  # Postflower stems
  dplyr::mutate(Num.Stems.PostFlower = dplyr::case_when(
    Num.Stems.PostFlower == "skipped" ~ "",
    TRUE ~ Num.Stems.PostFlower)) %>%
  # Nonflowering stems
  dplyr::mutate(Num.Stems.Nonflowering = dplyr::case_when(
    Num.Stems.Nonflowering == "skipped" ~ "",
    Num.Stems.Nonflowering %in% c(
      "1 (bitten off 8 \" from ground, presumably by deer",
      "1 dying") ~ "1",
    Num.Stems.Nonflowering == "2- these particular stems are unusually weak" ~ "2",
    TRUE ~ Num.Stems.Nonflowering)) %>%
  # Unbitten stems with axillary shoots
  dplyr::mutate(Num.Unbit.Stems.w.Axillary.Shoots = dplyr::case_when(
    Num.Unbit.Stems.w.Axillary.Shoots %in% c(
      "no data", "n.a.", "unk", "n/a", "u", "unknown", "na") ~ "",
    TRUE ~ Num.Unbit.Stems.w.Axillary.Shoots)) %>%
  # Asclepias tuberosa conspecifics within 1 meter
  dplyr::mutate(ASCTUB.Abun.1m = dplyr::case_when(
    ASCTUB.Abun.1m %in% c(
      "NO DATA", "no data", "unk", "uncertain", "did not assess",
      "did not measure", "not recorded", "skipped", "?") ~ "",
    ASCTUB.Abun.1m %in% c("no", "No") ~ "0",
    ASCTUB.Abun.1m %in% c(
      "1 A. tuberosa with 2 stems in flower 1m to SE; no other within 1.4m",
      "YES; number not recorded", 
      "1 A. tuberosa with 1 stem in bud only 60cm to W",
      "yes; One A. tuberosa with 1 flowering stem is 90cm to SSW",
      "tiny (5\" tall) A. tuberosa seedling only 40cm to SSE",
      "1 A. tuberosa (only 1 stem, not flowering) is 50cm NE of this plant",
      "1 AT with 2 stems in bud only 80cm to NE",
      "seedling 15cm tall 70cm to S",
      "1 A. tuberosa with 3 stems in bloom (and 1 stem that won't bloom this year) only 1m to NNW",
      "1 A. tuberosa with only 2 stems in bloom only 0.9m E",
      "1 A. tuberosa 0.9m to E",
      "another A. tuberosa only 0.65m to WNW - named it PAWS1401",
      "0.65m WNW of PAWS1007") ~ "1",
    ASCTUB.Abun.1m %in% c(
      "there is 1 A. tuberosa (with 1 stem in bud) 35cm to SE (~90 small green buds) and 1 A. tuberosa (1stem, bitten) 35cm to SW",
      "this plant is less than 1m from plants 105 and 106; ~20cm S",
      "another robust A. tuberosa 1m to E and a smaller A. tuberosa 0.8m to WSW", "1 or more",
      # Double checked this date one in the raw data
      "as on 6/29/2014") ~ "2",
    # Some 1 meter plants were recorded in the 2m column
    ASCTUB.Abun.2m == "another A. tuberosa with 1 stem (no flowers) is 30cm to W of plant" ~ "1",
    TRUE ~ ASCTUB.Abun.1m)) %>%
  # Asclepias tuberosa conspecifics within 2 meters
  dplyr::mutate(ASCTUB.Abun.2m = dplyr::case_when(
    ASCTUB.Abun.2m %in% c(
      "NO DATA", "unknown", "unk", "did not assess", "skipped", 
      "did not measure", "not recorded", "?", "no data") ~ "",
    ASCTUB.Abun.2m %in% c(
      "no", "1 A. tuberosa 1.3 m to SW", "No", 
      "no; 1 Asclep. hirtella approx. 2m to E",
      "no; 6 whorled milkweed (non-flowering)",
      "no; A. syriaca, but protecte dby two branches of dead tree (1.3m N of A. tuberosa)"
    ) ~ "0",
    ASCTUB.Abun.2m %in% c(
      "YES; number not recorded", "yes; # unk", "1 A.t. is 2m to the S",
      "1 milkweed within 1.1m to SE", "closest milkweed is 1.1m north",
      "another A. tuberosa 1.1m away", "yes; 1 A. tuberosa 1.2m SW",
      "1 A. tuberosa only 1.7m to E; it had 3 stems, but all were bitten by cattle, and none will bloom", "this plant is 1.7m ENE of RISC1002",
      "yes; one", "A. tuberosa 1.5m to NE, which has 2 stems, each stem having hundreds of buds and/or flowers",
      "N2006 1.1m to NNW", "another A. tuberosa 1.1m S and another 1.4m NW",
      "another A. tuberosa 1.6m S of A. tuberosa",
      "Yes, one plant 1.7m away, small plant with only 2 stems, neither will bloom; no monarch eggs on this neighbor",
      "healthy A. tuberosa with 2 stems in early bud is 1.5m to the W; that plant, which is earlier phenologically, does not have monarch eggs",
      "yes", "seedling 18cm tall (two stems) 1.2m to WSW",
      "1 A. tubersosa with 2 stems in bloom only 1.4m NE",
      "another A. tuberosa with 1 stem (no flowers) is 30cm to W of plant",
      "1 other A. tuberosa only 95cm to SW, it will not bloom this year",
      "1 A. tuberosa 1.5m to E", "1 A. tuberosa 1.9m to S",
      "at least 1", "1 or more", "yes (# unk)") ~ "1",
    ASCTUB.Abun.2m %in% c(
      "1 A. tuberosa (with 2 stems) 2.0 m NW, 1 A.tuberosa 2.0m SW (with 1 stem in bloom)",
      "one 2m to ENE, another 2m to NNW; both are healthy;",
      "1 robust A. tuberosa 1.15m N of this plant (with 13 stems that are done flowering) and plant PAWE2045 is 1.3m S of this plant",
      "there is another A. tuberosa 1.5m N of GPS point (2 stems with no flowers this year) and 1 A. tuberosa 1.5m SE of GPS point (1 stem with no flowers)",
      "1 A. tuberosa 1.5m to E (with 1 stem that won't flower this year); 1 A. tuberosa to S (with 1 stem in bud) with 13 buds and no signs of monarch immatures"
    ) ~ "2",
    ASCTUB.Abun.2m == "Multiple A. tuberosa within 1.5 m !!" ~ "3",
    ASCTUB.Abun.2m == "12 ~1.2m from W1003" ~ "12",
    TRUE ~ ASCTUB.Abun.2m)) %>%
  # Crab spider abundance
  # Crab spider abundance
  dplyr::mutate(Crab.Spider.Abun = dplyr::case_when(
    Crab.Spider.Abun %in% c(
      "no data", "presum no", "na", "unk (not recorded)",
      "did not assess", "did not measure") ~ "",
    Crab.Spider.Abun %in% c("no", " no", "NO", "n/a; no") ~ "0",
    Crab.Spider.Abun %in% c(
      "1 yellow crab spider in blossom of this plant",
      "yes; 1 yellow crab spider hiding amidst blossoms",
      "yes; 1 yellow crab spider hiding in blossoms",
      "1 yellow crab spider with dead Apis mellifera in its clutches",
      "yes; yellow crab spider", "1 yellow crab spider on stem C,",
      "1 yellow crab spider on stem B", "yes, on A",
      "1 crab spider in flower on stem A. yellow spider at @:45 pm"
    ) ~ "1",
    Crab.Spider.Abun == "2 Crab spider" ~ "2",
    TRUE ~ Crab.Spider.Abun)) %>%
  # Shrub abundance within 1 meter
  dplyr::mutate(Shrub.Abun.1m = dplyr::case_when(
    Shrub.Abun.1m %in% c(
      "no data", "NO DATA", "unknown", "unk", "did not measure",
      "did not assess", "not recorded") ~ "",
    Shrub.Abun.1m %in% c(
      "no", "0, but 3 Baptistia alba nearby", "0, but 1 Baptisia alba",
      "0, but 2 Baptisia alba and Solidago rigida", "2 Baptisia alba",
      "0, but lots of ironweed", "barbed wire fence") ~ "0",
    Shrub.Abun.1m %in% c(
      "1 buckbrush", "1 multiflora rose", "1 smooth sumac",
      "1 buckbrush 95cm to N", "1 multi-floral rose",
      "1 elm", "1 (.8m west of osage orange)",
      "1 buckbrush 99cm away", "1 buckbrush 30cm away", 
      "1 buckbrush 80cm SW", "1 buckbrush 60 cm S", 
      "1 buckbrush 1m away", "1 small elm only 5cm from A. tuberosa",
      "1 buckbrush 40cm away", "1 large multiflora rose",
      "1 buckbrush 10cm to W", "yes; 1 sharp cedar snag 50cm to SE", 
      "yes; 1 sharp cedar snag 15cm to W", "1 rose",
      "yes; 1 sharp cedar snag 80cm to WSW",
      "plant within 50cm of buckbrush or other shrub",
      "15cm from base of a buckbrush", "1 wild plum (20cm S)",
      "1 black raspberry", "1 2m tall cedar only 30cm away",
      "1 Rosa multiflora", "1 Rosa multiflora", "1 Rosa multiflora",
      "1 wild grape", "1 buck brush", "1 plum", "1 osage orange",
      "1 Rubus", "1 blackberry", "1 Sumac", "1 Buckbrush", 
      "1 large osage orange", "6' tall osage orange", "1 wild plum",
      "1 buckbrush, 1 Baptisia alba", "1 shrubby elm",
      "1 small multiflower rose", "osage orange",
      "1 osage orange (2 m tall)") ~ "1",
    Shrub.Abun.1m %in% c(
      "1 buckbrush, 1 osage orange", "2 buckbrush", 
      "2 buckbrush within 30cm", "2 small multi-floral rose",
      "2 small buckbrush", "2 shrubs (1 hawthorn)",
      "2 buckbrush within 30 cm", "2 buckbrush within 30 cm",
      "2 small multiflora rose", "1 large osage orange, 1 buckbrush",
      "1 large osage orange, 1 buckbrush", "2 small multiflora rose",
      "2 multi flora rose", "2 buck brush", "1 Rubus, 1Sumac", 
      "1 osage orange, 1 raspberry") ~ "2",
    Shrub.Abun.1m %in% c(
      "3 buckbrush; 20cm from woody elm stem; 40cm from buckbrush",
      "3 buckbrush", "3 Carolina rose", "3 black berries",
      "1 osage orange, 1 plum, 1 raspberry", "3 multi-flora rose",
      "1 elm, 1 buckbrush, 1 hawthorn", "3 Buckbrush", "3 dogweed") ~ "3",
    Shrub.Abun.1m %in% c(
      "4 buckbrush", "1 buckbrush, 2 blackberry, 1 elm sapling",
      "3 buckbrush and 1 prairie rose", "3 Rubus, 1 Rosa",
      "1 buckbrush, 1 multi-stemmed, 2 fall elm", "4 prairie rose",
      "3 buckbrush, osage orange", "4 prairie rose",
      "3 buckbursh, 1 osage orange", "1 honey locust, 3 licorice",
      "3 buck brush, 1 multi prarie rose", "4 buck brush", "4 Buckbrush",
      "4 multi-flora rose", "4 blackberry", "4 ironweed") ~ "4",
    Shrub.Abun.1m %in% c(
      "5 buckbrush", "5 buck brush", "5 buckbrush(1 huge clump)",
      "5 buckbrush; 20 buckbrush stems (may have helped protect plant)",
      "3 sumac, 3 rubus") ~ "5",
    Shrub.Abun.1m %in% c(
      "6 buckbrush", "n/a; 6 buckbrush", "6 Rubus", 
      "5 buckbrush, 1 Osage orange", "6 dogwood oak",
      "1 locust tree, 5 black berry", "6 Buckbrush",
      "6 dogwood black berry") ~ "6",
    Shrub.Abun.1m %in% c(
      "7 buckbrush", "7 prairie rose", "5 Cornus 2 Rubus", 
      "7 blackberry") ~ "7",
    Shrub.Abun.1m %in% c(
      "8 Cornus", "n/a; 8 buckbrush", "8 buckbrush",
      "5 blackberry, 3 buckbrush") ~ "8",
    Shrub.Abun.1m %in% c(
      "n/a; 9 buckbrush", "9 buckbrush", "9 buckbrush", "9 buckbrush",
      "6 blackberry, 1 prairie rose, 2 buckbrush") ~ "9",
    Shrub.Abun.1m %in% c("9 dogwoods, 1 shingle oak", "10 Buckbrush") ~ "10",
    # Other numbers
    Shrub.Abun.1m == "11 buckbrush" ~ "11",
    Shrub.Abun.1m == "12 Rubus" ~ "12",
    Shrub.Abun.1m == "20 or 30" ~ "25",
    TRUE ~ Shrub.Abun.1m)) %>%
  # Bitten flowering stems
  dplyr::mutate(Num.Flowering.Stems.Bitten = dplyr::case_when(
    Num.Flowering.Stems.Bitten %in% c("unk", "unknown", "na") ~ "",
    TRUE ~ Num.Flowering.Stems.Bitten)) %>%
  # Stems with axillary shoots
  dplyr::mutate(Num.Bitten.Stems.w.Axillary.Shoots = dplyr::case_when(
    Num.Bitten.Stems.w.Axillary.Shoots %in% c(
      "unclear", "no data", "unk", "na", "unknown") ~ "",
    TRUE ~ Num.Bitten.Stems.w.Axillary.Shoots)) %>%
  # Axillary shoots total
  dplyr::mutate(Tot.Axillary.Shoots = dplyr::case_when(
    Tot.Axillary.Shoots %in% c("?", "no data", "unknown",
                               "unk", "na") ~ "",
    Tot.Axillary.Shoots == "3 tiny ones" ~ "3",
    Tot.Axillary.Shoots == "4 bitten, 2 unbitten" ~ "6",
    Tot.Axillary.Shoots == "7 (6+1)" ~ "7",
    Tot.Axillary.Shoots == "dozens?" ~ "24",
    TRUE ~ Tot.Axillary.Shoots)) %>%
  # Axillary shoots bitten
  dplyr::mutate(Num.Axillary.Shoots.Bitten = dplyr::case_when(
    Num.Axillary.Shoots.Bitten %in% c("?", "no data", "unk", "not recorded",
                                      "unknown", "na") ~ "",
    TRUE ~ Num.Axillary.Shoots.Bitten)) %>%
  # For the conspecifics columns we can fix it if only one is recorded
  dplyr::mutate(
    # If 2m is blank but 1m isn't we can fill 2m with 1m data
    ASCTUB.Abun.2m = ifelse(
      test = is.na(ASCTUB.Abun.2m) | nchar(ASCTUB.Abun.2m) == 0,
      yes = ASCTUB.Abun.1m,
      no = ASCTUB.Abun.2m))

# Check again to ensure issues are resolved
helpR::multi_num_chk(data = mkwd_v8, col_vec = c(
  "Num.Stems.Budding", "Num.Stems.Flowering", 
  "Num.Stems.PostFlower" , "Num.Stems.Nonflowering", "Tot.Bitten.Stems",
  "Num.Unbit.Stems.w.Axillary.Shoots", 
  "ASCTUB.Abun.1m", "ASCTUB.Abun.2m", "Crab.Spider.Abun",
  "Shrub.Abun.1m", "Tot.Bitten.Stems", "Num.Flowering.Stems.Bitten",
  "Num.Bitten.Stems.w.Axillary.Shoots", "Tot.Axillary.Shoots", 
  "Num.Axillary.Shoots.Bitten"))

## ------------------------------------------------ ##
       # Character Column Standardization ----
## ------------------------------------------------ ##

# Want to standardize some character columns
mkwd_v9 <- mkwd_v8 %>%
  # Grazing lawn
  dplyr::mutate(GrazingLawn = dplyr::case_when(
    GrazingLawn %in% c("did not measure", "no data", 
                       "NO DATA", "unk", "not recorded") ~ "",
    GrazingLawn %in% c(
      "no", "No", "NO", "maybe last year", "No, but in cattle path",
      "no; 9 cattle bedding spots within 12m radius",
      "no; only 1m from cattle trail; 7 large areas flattened by sleeping ungulates within 12m radius"
    ) ~ "no lawn",
    GrazingLawn %in% c(
      "adjacent to one", "adjacent to small lawn", "at edge",
      "n/a; no right next to one", "near one", "no (but near)",
      "no but 1 m away", "no, 1 m away", "no, but .5 m away",
      "no, but 1 m away", "no, but 10 cm away from one",
      "no, but 1m from one", "no, but 2 m away", "no, but 20 cm away",
      "no, but 20 cm from one", "no, but 30 cm away", "on one side",
      "no, but 30cm away", "no, but adjacent", "no, but close",
      "no, but close", "no, but near", "no, but near small one",
      "no, but next to one", "no, but only 30 cm away from one",
      "no, but within 1 m") ~ "within 5 meters",
    GrazingLawn %in% c(
      "n/a; yes (mild)", "partial", "yes (mild)", "yes (minor)",
      "yes (patchy)", "yes (very mild)", "yes, a spotty lawn",
      "yes, mild", "yes, partial lawn") ~ "mild",
    GrazingLawn %in% c(
      "n/a; yes", "yes", "yes (at edge)", "yes (mod.)", "yes, moderate",
      "yes, moderate lawn") ~ "moderate",
    GrazingLawn == "yes, moderate to severe" ~ "severe",
    TRUE ~ GrazingLawn))

# Check out columns
sort(unique(mkwd_v9$GrazingLawn))

# Look at the whole data now
dplyr::glimpse(mkwd_v9)

## ------------------------------------------------ ##
        # Response Variable Calculation ----
## ------------------------------------------------ ##

# Drop entirely empty rows
mkwd_v10 <- mkwd_v9[complete.cases(mkwd_v9[, "Site"]), ] %>%
  # Move grazing lawn column to the left
  dplyr::relocate(GrazingLawn, .after = Plant.ID) %>%
  # Make numeric columns truly numeric
  dplyr::mutate(dplyr::across(.cols = Avg.Height:Tot.Monarch.Immatures,
                              .fns = as.numeric)) %>%
  # Calculate some needed response variables
  dplyr::mutate(
    Tot.Bud.n.Flr = (Tot.Bud + Tot.Flr),
    Num.Stems.ALL.Flowering.Stages = (Num.Stems.Budding + Num.Stems.Flowering + Num.Stems.PostFlower),
    Tot.Stems = (Num.Stems.ALL.Flowering.Stages + Num.Stems.Nonflowering),
    Ratio.Bitten.vs.Total.Stems = ifelse(
      test = Tot.Bitten.Stems < Tot.Stems,
      yes = (Tot.Bitten.Stems / Tot.Stems),
      no = NA))

# Check dimensions
dim(mkwd_v9); dim(mkwd_v10)

# Identify gained/lost columns
helpR::diff_chk(old = names(mkwd_v9), new = names(mkwd_v10))

# Glimpse data
dplyr::glimpse(mkwd_v10)

# Get a summary of the data
summary(mkwd_v10)

## ------------------------------------------------ ##
        # Explanatory Variable Retrieval ----
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
            # "New" Data Checking ----
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
            # Monarch Adult Tidying ----
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
             # Final Tidying Steps ----
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
               # Acknowledgements ----
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

# END ----

