## ---------------------------------------------------------- ##
          # Moranz et al. Asclepias tuberosa Project
## ---------------------------------------------------------- ##
# Code written by Nicholas J Lyon

# PURPOSE ----
## This code harmonizes each year's data to create a single (untidy) dataframe

# Call any needed libraries here (good to centralize this step)
# install.packages("librarian")
librarian::shelf(readxl, tidyverse, vegan, writexl, njlyon0/helpR)

# Clear the environment
rm(list = ls())

# Read in metadata file
mkwd_13_16_meta <- read.csv(file.path("raw_data", "Asclepias-metadata.csv")) %>%
  # Fix an ID column name
  dplyr::rename(AsclepTransID = AsclepTransIDauto)

## ------------------------------------------------ ##
               # 2012 Rough Tidying ----
## ------------------------------------------------ ##
# Read in the data for 2012
mkwd_12_v1 <- read_excel(path = file.path("raw_data", "Asclepias-2012-RAW.xlsx"),
                         sheet = "MAIN Trimbled minus not 3yrs")

# Take a look at the data
dplyr::glimpse(mkwd_12_v1)

# Drop rows that are completely NA / no entries
mkwd_12_v2 <- mkwd_12_v1[complete.cases(mkwd_12_v1[, "Pasture"]), ] %>%
  # Make all columns characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Slicing down now to only needed columns
  dplyr::select(Year, Date, Pasture, PatchWhit,
                `2012PlantIDCode`, `2012PlantIDCode_2nd Rnd`,
                `Length zAVERAGE`, `# buds zAVERAGE`, 
                `# flowers zAVERAGE`,
                `# buds TOTAL`, `# flowers TOTAL`, 
                `bloom status zAVERAGE`,
                TrimbStemsAbud, TrimbStemsBflower, TrimbStemsCdone,
                TrimbStemsDnoflowers, TrimbStemsFbudflowerdone,
                `# of UNbit stems w axillary`, AsclWithin1m, AsclWithin2m,
                BfliesNectaring, `Crab spiders?`,
                GrazingLawn, `height on 1st longest`, 
                `height on 2nd longest`, `height on 3rd longest`,
                Comments, `major issues`, MonarchImmatures1,
                MonarchImmatures2, ShrubsWithin1m, 
                TotalNumBittenStems) %>%
  # Rename columns more informatively
  dplyr::rename(Site = Pasture, 
                Patch = PatchWhit,
                Plant.ID = `2012PlantIDCode`, 
                Plant.ID.R2 = `2012PlantIDCode_2nd Rnd`,
                Avg.Height = `Length zAVERAGE`, 
                Avg.Bud = `# buds zAVERAGE`, 
                Avg.Flr = `# flowers zAVERAGE`,
                Tot.Bud = `# buds TOTAL`, 
                Tot.Flr = `# flowers TOTAL`, 
                Avg.Bloom.Status = `bloom status zAVERAGE`,
                Num.Stems.Budding = TrimbStemsAbud, 
                Num.Stems.Flowering = TrimbStemsBflower,
                Num.Stems.PostFlower = TrimbStemsCdone, 
                Num.Stems.Nonflowering = TrimbStemsDnoflowers, 
                Num.Stems.ALL.Flowering.Stages = TrimbStemsFbudflowerdone,
                Num.Unbit.Stems.w.Axillary.Shoots = `# of UNbit stems w axillary`, 
                ASCTUB.Abun.1m = AsclWithin1m, 
                ASCTUB.Abun.2m = AsclWithin2m, 
                BfliesNectaring = BfliesNectaring, 
              Crab.Spider.Abun = `Crab spiders?`,
                GrazingLawn = GrazingLawn, 
                Height.1st.Longest = `height on 1st longest`, 
                Height.2nd.Longest = `height on 2nd longest`, 
                Height.3rd.Longest = `height on 3rd longest`,
                Comments = Comments, Major.Issues = `major issues`, 
                MonarchImmatures1 = MonarchImmatures1,
                MonarchImmatures2 = MonarchImmatures2,
                Shrub.Abun.1m = ShrubsWithin1m,
                Tot.Bitten.Stems = TotalNumBittenStems) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # If year is missing, fill with first bit of date
  dplyr::mutate(Year = ifelse(
    test = is.na(Year) | nchar(Year) == 0,
    yes = stringr::str_sub(string = Date, start = 1, end = 4),
    no = Year)) %>%
  # Create a useful plant ID code
  dplyr::mutate(Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything())

# Glimpse the data
dplyr::glimpse(mkwd_12_v2)

## ------------------------------------------------ ##
              # 2013 Stem Data Tidying ----
## ------------------------------------------------ ##
# Some needed data was in a different sheet of the Excel file
mkwd_13_stm_v1 <- read.csv(file.path("raw_data", "Asclepias-2013-RAW-stems.csv"))

# Glimpse it
dplyr::glimpse(mkwd_13_stm_v1)

# Wrangle it for inclusion with the rest of this year's data
mkwd_13_stm_v2 <- mkwd_13_stm_v1 %>%
  # Rename a column
  dplyr::rename(PlantNumAuto = PlantNum) %>%
  # Make a row ID column
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  # Drop unwanted columns
  dplyr::select(-STEMNumAuto, -Comments.5) %>%
  # Make all columns characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = Stem.Length..cm.:Bloom.Status,
                      names_to = "metrics",
                      values_to = "values")

# Check for bad entries
helpR::num_chk(data = mkwd_13_stm_v2, col = "values")

# Want to summarize within plants
mkwd_13_stm_v3 <- mkwd_13_stm_v2 %>%
  # Fix bad non-number entries
  dplyr::mutate(values = dplyr::case_when(
    values %in% c("too early", "in bud", "full", "too small", "", 
                  "to small", "prob will bloom late summer", "Accidental Row",
                  "accidental row", "not recorded") ~ "",
    values == "150+" ~ "150",
    TRUE ~ values)) %>%
  # Make that column numeric
  dplyr::mutate(values = as.numeric(values)) %>%
  # Pivot wider again
  tidyr::pivot_wider(names_from = metrics,
                     values_from = values) %>%
  # Now summarize within plant
  dplyr::group_by(PlantNumAuto) %>%
  dplyr::summarize(
    Avg.Height = mean(Stem.Length..cm., na.rm = T),
    Avg.Bud = mean(X..of.buds, na.rm = T),
    Avg.Flr = mean(X..of.flowers, na.rm = T),
    Tot.Bud = sum(X..of.buds, na.rm = T),
    Tot.Flr = sum(X..of.flowers, na.rm = T),
    Avg.Bloom.Status = mean(Bloom.Status, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate total buds and flowers
  dplyr::mutate(Tot.Bud.n.Flr = Tot.Bud + Tot.Flr) %>%
  # Make everything a character
  dplyr::mutate(dplyr::across(.fns = as.character))

# Glimpse again
dplyr::glimpse(mkwd_13_stm_v3)

## ------------------------------------------------ ##
              # 2013 Rough Tidying ----
## ------------------------------------------------ ##

# Read in main data
mkwd_13_v1 <- read.csv(file.path("raw_data", "Asclepias-2013-RAW-plants.csv"))

# Glimpse it
dplyr::glimpse(mkwd_13_v1)

# Tidy column names
mkwd_13_v2 <- mkwd_13_v1 %>%
  # Add a year column
  dplyr::mutate(Year = "2013") %>%
  # And attach the metadata information
  dplyr::left_join(y = mkwd_13_16_meta, by = "AsclepTransID") %>%
  # Make every column into characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Move columns around / drop unwanted ones
  dplyr::select(Year, Pasture, Patch, Whittaker, Date, 
                PlantNumAuto, AsclepTransID, PlantID.Code.from.2012,
                Name.from.2012, X2013.Plant.ID.Code,
                StemsBUD, StemsFLOWER, StemsDONE, StemsNOflowers) %>%
  # Rename remaining columns
  dplyr::rename(Plant.ID = PlantID.Code.from.2012,
                Site = Pasture,
                Plant.ID.V2 = Name.from.2012,
                Plant.ID.V3 = X2013.Plant.ID.Code,
                Num.Stems.Budding = StemsBUD,
                Num.Stems.Flowering = StemsFLOWER,
                Num.Stems.PostFlower = StemsDONE,
                Num.Stems.Nonflowering = StemsNOflowers) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # Left join on stem data
  dplyr::left_join(y = mkwd_13_stm_v3, by = "PlantNumAuto") %>%
  # If year is missing, fill with first bit of date
  dplyr::mutate(Year = ifelse(
    test = is.na(Year) | nchar(Year) == 0,
    yes = stringr::str_sub(string = Date, start = 1, end = 4),
    no = Year)) %>%
  # Create a useful plant ID code
  dplyr::mutate(Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything())

# Glimpse again
dplyr::glimpse(mkwd_13_v2)

## ------------------------------------------------ ##
            # 2013-14 Stem Data Tidying ----
## ------------------------------------------------ ##
# Some needed data was in a different sheet of the Excel file
mkwd_14_stm_v1 <- read.csv(file.path("raw_data", "Asclepias-2014-RAW-stems.csv"))

# Glimpse it
dplyr::glimpse(mkwd_14_stm_v1)

# Wrangle it for inclusion with the rest of this year's data
mkwd_14_stm_v2 <- mkwd_14_stm_v1 %>%
  # Rename a column
  dplyr::rename(PlantNumAuto = PlantNum) %>%
  # Make a row ID column
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  # Drop unwanted columns
  dplyr::select(-STEMNumAuto, -dplyr::contains("Comments."),
                -bite.status.by.stem) %>%
  # Make all columns characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = Stem.Length..cm.:Bloom.Status,
                      names_to = "metrics",
                      values_to = "values")

# Check for bad entries
helpR::num_chk(data = mkwd_14_stm_v2, col = "values")

# Want to summarize within plants
mkwd_14_stm_v3 <- mkwd_14_stm_v2 %>%
  # Fix bad non-number entries
  dplyr::mutate(values = dplyr::case_when(
    values %in% c("too early", "in bud", "full", "too small", "", 
                  "to small", "prob will bloom late summer", "Accidental Row",
                  "accidental row", "not recorded", "7/1", "no data",
                  "n.a.", "Accidental row", "?", "8(1)", "8(13)",
                  "8(4)", "8(2)", "8(6)", "(too small to count)",
                  "too small to count", "3/5", "4/5", "?/54",
                  "?/0", "?/120", "?/48", "?/140", "?/69",
                  "?/245", "5/6", "?/42", "?/85", "5/7", "?/62", "?/185",
                  "5/8(2)", "?/53", "?/90", "4/7", "?/45", "4/6", "?/55",
                  "?/250", "?/150", "?/51", "?/130", "?/27", "unk"
                  ) ~ "",
    values == "150+" ~ "150",
    TRUE ~ values)) %>%
  # Make that column numeric
  dplyr::mutate(values = as.numeric(values)) %>%
  # Pivot wider again
  tidyr::pivot_wider(names_from = metrics,
                     values_from = values) %>%
  # Now summarize within plant
  dplyr::group_by(PlantNumAuto) %>%
  dplyr::summarize(
    Avg.Height = mean(Stem.Length..cm., na.rm = T),
    Avg.Bud = mean(X..of.buds, na.rm = T),
    Avg.Flr = mean(X..of.flowers, na.rm = T),
    Tot.Bud = sum(X..of.buds, na.rm = T),
    Tot.Flr = sum(X..of.flowers, na.rm = T),
    Avg.Bloom.Status = mean(Bloom.Status, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate total buds and flowers
  dplyr::mutate(Tot.Bud.n.Flr = Tot.Bud + Tot.Flr) %>%
  # Make everything a character
  dplyr::mutate(dplyr::across(.fns = as.character))

# Glimpse again
dplyr::glimpse(mkwd_14_stm_v3)

# Bind this together with the 2013 data
mkwd_13_14_stems <- mkwd_13_stm_v3 %>%
  dplyr::bind_rows(mkwd_14_stm_v3) %>%
  # And drop duplicate rows
  dplyr::distinct()

## ------------------------------------------------ ##
              # 2013-14 Rough Tidying ----
## ------------------------------------------------ ##
# Read in data
mkwd_14_v1 <- read.csv(file.path("raw_data", "Asclepias-2014-RAW-plants.csv"))

# Glimpse it
dplyr::glimpse(mkwd_14_v1)

# Tidy column names
mkwd_14_v2 <- mkwd_14_v1 %>%
  # And attach the metadata information
  dplyr::left_join(y = mkwd_13_16_meta, by = "AsclepTransID") %>%
  # Make every column into characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Move columns around / drop unwanted ones
  dplyr::select(YearVis, Date, Pasture, Patch, Whittaker,
                PlantID.Code.from.2012, PlantNumAuto,
                X2013.Plant.ID.Code,
                Monarch.immatures2.,
                total...bitten.stems,
                TRIMBSBUD:TRIMBS.NOflow, Ascl.within.1.m., 
                Ascl.within.2.m,
                Grazing.lawn., Bflies.nectaring., Crab.spiders., 
                Shrubs.within.1.m.,
                X..of.bitten.stems.flower.this.year,
                X..bitten.stems.w.axillary,
                X..of.UNbit.stems.w.axillary,
                X..of.axillary.shoots, 
                X..of.BITTEN.Axillary.shoots) %>%
  # Rename remaining columns
  dplyr::rename(Year = YearVis,
                Site = Pasture,
                Plant.ID = PlantID.Code.from.2012,
                Plant.ID.V3 = X2013.Plant.ID.Code,
                MonarchImmatures2 = Monarch.immatures2., 
                Tot.Bitten.Stems = total...bitten.stems,
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
                Num.Axillary.Shoots.Bitten = X..of.BITTEN.Axillary.shoots) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # Left join on stem data from '13 AND '14
  dplyr::left_join(y = mkwd_13_14_stems, by = "PlantNumAuto") %>%
  # If year is missing, fill with first bit of date
  dplyr::mutate(Year = ifelse(
    test = is.na(Year) | nchar(Year) == 0,
    yes = stringr::str_sub(string = Date, start = 1, end = 4),
    no = Year)) %>%
  # Create a useful plant ID code
  dplyr::mutate(Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything())

# Glimpse again
dplyr::glimpse(mkwd_14_v2)

## ------------------------------------------------ ##
            # 2013-15 Rough Tidying ----
## ------------------------------------------------ ##
# Read in data
mkwd_15_v1 <- read.csv(file.path("raw_data", "Asclepias-2015-RAW-plants.csv"))

# Glimpse it
dplyr::glimpse(mkwd_15_v1)

# Tidy column names
mkwd_15_v2 <- mkwd_15_v1 %>%
  # And attach the metadata information
  dplyr::left_join(y = mkwd_13_16_meta, by = "AsclepTransID") %>%
  # Make every column into characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Move columns around / drop unwanted ones
  dplyr::select(YearVis, Date, Pasture, Patch, Whittaker,
                PlantID.Code.from.2012,
                Length.St1:LengthSt3, 
                Buds.St.1:Buds.St.3, 
                Flow.St.1:Flow.St.3,
                BlooStatus.St.1:BlooStatus.St.3, 
                TRIMBSBUD:TRIMBS.NOflow, 
                Monarch.immatures2.,
                total...bitten.stems,
                Ascl.within.1.m., 
                Ascl.within.2.m,
                Grazing.lawn., Bflies.nectaring., Crab.spiders., 
                Shrubs.within.1.m.,
                X..of.bitten.stems.flower.this.year,
                X..bitten.stems.w.axillary,
                X..of.UNbit.stems.w.axillary,
                X..of.axillary.shoots, 
                X..of.BITTEN.Axillary.shoots) %>%
  # Rename remaining columns
  dplyr::rename(Year = YearVis,
                Site = Pasture,
                Plant.ID = PlantID.Code.from.2012,
                Length.StObs1 = Length.St1,
                Length.StObs2 = LengthSt2,
                Length.StObs3 = LengthSt3,
                Buds.StObs1 = Buds.St.1,
                Buds.StObs2 = Buds.St.2,
                Buds.StObs3 = Buds.St.3,
                Flow.StObs1 = Flow.St.1,
                Flow.StObs2 = Flow.St.2,
                Flow.StObs3 = Flow.St.3,
                BlooStatus.StObs1 = BlooStatus.St.1, 
                BlooStatus.StObs2 = BlooStatus.St.2, 
                BlooStatus.StObs3 = BlooStatus.St.3,
                MonarchImmatures2 = Monarch.immatures2., 
                Tot.Bitten.Stems = total...bitten.stems,
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
                Num.Axillary.Shoots.Bitten = X..of.BITTEN.Axillary.shoots) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # If year is missing, fill with first bit of date
  dplyr::mutate(Year = ifelse(
    test = is.na(Year) | nchar(Year) == 0,
    yes = stringr::str_sub(string = Date, start = 1, end = 4),
    no = Year))

  # Glimpse again
dplyr::glimpse(mkwd_15_v2)

## ------------------------------------------------ ##
              # 2013-16 Rough Tidying ----
## ------------------------------------------------ ##
# Read in data
mkwd_16_v1 <- read.csv(file.path("raw_data", "Asclepias-2016-RAW-plants.csv"))

# Glimpse it
dplyr::glimpse(mkwd_16_v1)

# Tidy column names
mkwd_16_v2 <- mkwd_16_v1 %>%
  # And attach the metadata information
  dplyr::left_join(y = mkwd_13_16_meta, by = "AsclepTransID") %>%
  # Make every column into characters
  dplyr::mutate(dplyr::across(.fns = as.character)) %>%
  # Move columns around / drop unwanted ones
  dplyr::select(YearVis, Date, Pasture, Patch, Whittaker,
                PlantID.Code.from.2012, Length.St1:LengthSt3, 
                Buds.St.1:Buds.St.3, Flow.St.1:Flow.St.3,
                BlooStatus.St.1:BlooStatus.St.3, Monarch.immatures2.,
                total...bitten.stems,
                TRIMBSBUD:TRIMBS.NOflow, Ascl.within.1.m., 
                Ascl.within.2.m,
                Grazing.lawn., Bflies.nectaring., Crab.spiders., 
                Shrubs.within.1.m.,
                X..of.bitten.stems.flower.this.year,
                X..bitten.stems.w.axillary,
                X..of.UNbit.stems.w.axillary,
                X..of.axillary.shoots, 
                X..of.BITTEN.Axillary.shoots) %>%
  # Rename remaining columns
  dplyr::rename(Year = YearVis,
                Site = Pasture,
                Plant.ID = PlantID.Code.from.2012, 
                Length.StObs1 = Length.St1,
                Length.StObs2 = LengthSt2,
                Length.StObs3 = LengthSt3,
                Buds.StObs1 = Buds.St.1,
                Buds.StObs2 = Buds.St.2,
                Buds.StObs3 = Buds.St.3,
                Flow.StObs1 = Flow.St.1,
                Flow.StObs2 = Flow.St.2,
                Flow.StObs3 = Flow.St.3,
                BlooStatus.StObs1 = BlooStatus.St.1, 
                BlooStatus.StObs2 = BlooStatus.St.2, 
                BlooStatus.StObs3 = BlooStatus.St.3,
                MonarchImmatures2 = Monarch.immatures2., 
                Tot.Bitten.Stems = total...bitten.stems,
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
                Num.Axillary.Shoots.Bitten = X..of.BITTEN.Axillary.shoots) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # If year is missing, fill with first bit of date
  dplyr::mutate(Year = ifelse(
    test = is.na(Year) | nchar(Year) == 0,
    yes = stringr::str_sub(string = Date, start = 1, end = 4),
    no = Year)) %>%
  # Create a useful plant ID code
  dplyr::mutate(Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything())

# Glimpse again
dplyr::glimpse(mkwd_16_v2)

## ------------------------------------------------ ##
            # Data Integration Prep ----
## ------------------------------------------------ ##

# Unfortunately, the "2013-16" data doesn't always include information that can be found in the dataframes for each of those years
# We need to now integrate that information as well

# Let's take each of the datasets and...
mkwd_13 <- mkwd_13_v2 %>%
  # Get a row number column
  dplyr::mutate(row_id = 1:nrow(.),
                Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything()) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = Num.Stems.Budding:Tot.Bud.n.Flr) %>%
  # Filter out missing values (we don't care about them for filling gaps)
  dplyr::filter(!is.na(value) & nchar(value) != 0 & value != "NaN") %>%
  # Now make an ID for plant + variable
  dplyr::mutate(Variable.ID = paste0(Plant.ID.Code, "-", name))

# Glimpse 
dplyr::glimpse(mkwd_13)

# Do the same for 2014
mkwd_14 <- mkwd_14_v2 %>%
  dplyr::mutate(row_id = 1:nrow(.),
                Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything()) %>%
  tidyr::pivot_longer(cols = MonarchImmatures2:Tot.Bud.n.Flr) %>%
  dplyr::filter(!is.na(value) & nchar(value) != 0 & value != "NaN") %>%
  dplyr::mutate(Variable.ID = paste0(Plant.ID.Code, "-", name))

## Check it out
dplyr::glimpse(mkwd_14)

# And 2015
mkwd_15 <- mkwd_15_v2 %>%
  dplyr::mutate(row_id = 1:nrow(.),
                Plant.ID.Code = paste0(Year, "_", Plant.ID),
                .before = dplyr::everything()) %>%
  tidyr::pivot_longer(cols = Length.StObs1:Num.Axillary.Shoots.Bitten) %>%
  dplyr::filter(!is.na(value) & nchar(value) != 0 & value != "NaN") %>%
  dplyr::mutate(Variable.ID = paste0(Plant.ID.Code, "-", name))

## Check
dplyr::glimpse(mkwd_15)

# Bind these objects together
mkwd_13_14_15 <- mkwd_13 %>%
  dplyr::bind_rows(mkwd_14) %>%
  dplyr::bind_rows(mkwd_15) %>%
  # Filter out missing values
  dplyr::filter(!is.na(value) & nchar(value) != 0) %>%
  # Drop missing IDs
  dplyr::filter(!is.na(Variable.ID)) %>%
  # Pare down to only bare minimum needed columns
  dplyr::select(Variable.ID, value) %>%
  # Drop duplicates
  unique()

# Glimpse this
dplyr::glimpse(mkwd_13_14_15)

## ------------------------------------------------ ##
              # Data Integration ----
## ------------------------------------------------ ##

# Okay, we can bind 2012 to '13-16 without fear
mkwd_12_16 <- mkwd_12_v2 %>%
  dplyr::bind_rows(mkwd_16_v2) %>%
  # And drop some unwanted columns
  dplyr::select(-Comments, -Major.Issues, 
                -dplyr::ends_with(".Longest"),
                -BfliesNectaring)

# Glimpse this
dplyr::glimpse(mkwd_12_16)

# Make sure we're happy with which columns are dropped
helpR::diff_chk(old = c(names(mkwd_12_v2), names(mkwd_16_v2)),
                new = names(mkwd_12_16))

# Quickly standardize missing values
milkweed_v1 <- mkwd_12_16 %>%
  # Make a row number column
  dplyr::mutate(row_id = 1:nrow(.), .before = dplyr::everything()) %>%
  # Relocate the whittaker column
  dplyr::relocate(Whittaker, .after = Patch) %>%
  # Pivot long
  tidyr::pivot_longer(cols = Avg.Height:Num.Axillary.Shoots.Bitten) %>%
  # Conditionally standardize missing vaues
  dplyr::mutate(value = dplyr::case_when(
    is.na(value) ~ "",
    value %in% c("unk", "unk.", "n.a.", "n/a", "na") ~ "",
    !is.na(value) & nchar(value) == 0 ~ "",
    TRUE ~ value)) %>%
  # Make a variable ID column here too
  dplyr::mutate(Variable.ID = paste0(Plant.ID.Code, "-", name))

# Glimpse this
dplyr::glimpse(milkweed_v1)

# How many responses are unrecorded?
milkweed_v1 %>%
  dplyr::filter(nchar(value) == 0 | is.na(value)) %>%
  nrow() # 29,513 (across all vars)

# Now match in the non-missing values
milkweed_v2 <- milkweed_v1 %>%
  dplyr::mutate(value = dplyr::case_when(
    nchar(value) == 0 ~ mkwd_13_14_15$value[match(.$Variable.ID,
                                                  mkwd_13_14_15$Variable.ID)],
    TRUE ~ value))

# How many are still problematic?
milkweed_v2 %>%
  dplyr::filter(nchar(value) == 0 | is.na(value)) %>%
  nrow() # 25,639 continued absent

# Check how many got fixed!
str <- nrow(dplyr::filter(milkweed_v1, nchar(value) == 0 | is.na(value)))
end <- nrow(dplyr::filter(milkweed_v2, nchar(value) == 0 | is.na(value)))
str-end
## 3,874 problems fixed!

# Now reclaim the original data structure
milkweed_v3 <- milkweed_v2 %>%
  # Drop variable ID column
  dplyr::select(-Variable.ID) %>%
  # Pivot wider to reclaim original structure
  tidyr::pivot_wider(names_from = name,
                     values_from = value) %>%
  # Drop row id column
  dplyr::select(-row_id)

# Glimpse it
dplyr::glimpse(milkweed_v3)

## ------------------------------------------------ ##
              # Export Harmonized Data ----
## ------------------------------------------------ ##

# Make a folder to export to
dir.create("tidy_data", showWarnings = F)

# Export this harmonized object
write.csv(x = milkweed_v3, na = "", row.names = F,
          file = file.path("tidy_data", "Asclepias-HARMONIZED.csv"))

# End ----
