
#**Verified to be working on 10/24/25, with improvements made based on updates to rfishbase and openxlsx2**

# Packages ----------------------------------------------------------------
library(clock)
library(suncalc)
library(rfishbase)
library(vegan)
library(reshape2)
library(tidyverse)
library(openxlsx2)
# load("./WrangledData.RData")
# Set Color Hex Codes and Shape Values for Gear Combinations
# Most colors come from the Okabe-Ito colorblind-friendly palette
GearColors <- function() {
  return(c("All Gears" = "#000000", "Cast Net" = "#E69F00", "Centipede Net" = "#56B4E9", "Seine" = "#009E73"))
}
GearColorsAll <- function() {
  return(c(
    "All Gears" = "#000000",                 # Black
    "Cast Net" = "#E69F00",                # Orange
    "Cast Net & Centipede Net" = "#CC79A7", # Pink/Rose
    "Cast Net & Seine" = "#785EF0",         # Violet
    "Centipede Net" = "#56B4E9",            # Sky Blue
    "Centipede Net & Seine" = "#B22222",    # Red
    "Seine" = "#009E73"                     # Green
  ))
}

GearShapes <- list(
  "All Gears" = 18,               # Filled diamond
  "Cast Net" = 16,                # Filled circle
  "Centipede Net" = 15,           # Filled square
  "Seine" = 17,                   # Filled triangle
  "Cast Net & Centipede Net" = 22, # Open square with border 
  "Cast Net & Seine" = 23,        # Open diamond with border 
  "Centipede Net & Seine" = 24    # Open triangle with border
)

GearLineTypes <- list("Cast Net" = "22",          # "dashed"
                      "Centipede Net" = "73",     # "longdash"
                      "Seine" = "1343",         # "dotdash"
                      "Cast Net & Centipede Net" = "1446",
                      "Cast Net & Seine" = "491549",
                      "Centipede Net & Seine" = "188888",
                      "All Gears" = "solid")

GearLines <- function() {
  return(c(
    # Single Gears (simple, standard patterns)
    "Cast Net" = "22",          # "dashed"
    "Centipede Net" = "73",     # "longdash"
    "Seine" = "1343",         # "dotdash"
    # Gear Combinations (custom, more complex patterns)
    "Cast Net & Centipede Net" = "1446",
    "Cast Net & Seine" = "491549", # This was the old Cast Net pattern
    "Centipede Net & Seine" = "188888",
    # All Gears
    "All Gears" = "solid"
  ))
}

SiteColors <- function() {
  palette.colors(6, "Tableau")[1:6]
}

# To facilitate correct lettering style for multi-panel plots
Letters <- function() {
  return(c("A", "B", "C", "D", "E", "F"))
}


# Read in Site data & for each row with a blank StartTime, update it to the StartTime for the same site and visit and Gear=Centipede Net and Group=2. Remove nonexistent samples (AKA Effort = 0)
SiteData <- wb_to_df("../Sampling Data.xlsx",
  sheet = "AllSamples_Info") %>%
  select(-c(Secchi1_cm, Secchi2_cm, mVpH, mVORP, `DO_%`, `Conductivity_mS/cm`, `AbsConductivity_mS/cm`, Resistance_MΩcm, TDS_ppt, σt, AtmosphericPressure_PSI)) %>%
  mutate(
    StartTime = if_else(!is.na(StartTime), StartTime, min(StartTime, na.rm = T)),
    EndTime = if_else(!is.na(EndTime), EndTime, max(EndTime, na.rm = T)), .by = c(Site, Visit)
  ) %>%
  mutate(SampleID = paste(substr(Site, 1, 3), substr(Visit, 1, 1), "_", substr(Gear, 1, 4), sep = ""), .before = Site) %>% 
  filter(Effort > 0)

## Clearing Excel-averaged values from Seine & Cast Net to prep for later summarizing by Site
SiteData[which(SiteData$Gear != "Centipede Net"), match("SecchiDepth_cm", names(SiteData)):match("Temperature_C", names(SiteData))] <- NA

# Add site-wide descriptors Dominant Substrate (descriptive), Mud Dominance (0,1) & Steepness (0,1,2)
SiteData <- left_join(SiteData, wb_to_df("../Sampling Data.xlsx",
  sheet = "Site_SubstrateSteepness"))

# Add Start & End Dates & Times
SiteData <- SiteData %>% 
  mutate(StartTime = date_time_build(get_year(SiteData$Arrival_DateTime),
                                     get_month(SiteData$Arrival_DateTime),
                                     get_day(SiteData$Arrival_DateTime),
                                     get_hour(SiteData$StartTime),
                                     get_minute(SiteData$StartTime),
                                     zone = "America/Costa_Rica"),
         EndTime = if_else(get_hour(SiteData$EndTime) < get_hour(SiteData$StartTime),
                           date_time_build(get_year(SiteData$Arrival_DateTime),
                                           get_month(SiteData$Arrival_DateTime),
                                           get_day(SiteData$Arrival_DateTime) + 1,
                                           get_hour(SiteData$EndTime),
                                           get_minute(SiteData$EndTime),
                                           zone = "America/Costa_Rica"
                           ),
                           date_time_build(get_year(SiteData$Arrival_DateTime),
                                           get_month(SiteData$Arrival_DateTime),
                                           get_day(SiteData$Arrival_DateTime),
                                           get_hour(SiteData$EndTime),
                                           get_minute(SiteData$EndTime),
                                           zone = "America/Costa_Rica")),
         Date = date_build(get_year(SiteData$Arrival_DateTime), get_month(SiteData$Arrival_DateTime), get_day(SiteData$Arrival_DateTime))) %>% 
  relocate(Date, .before = Arrival_DateTime)
         

# Calculate Sunrise/Sunset and add to SiteData
SiteData <- left_join(SiteData, getSunlightTimes(
  data = (SiteData %>%
    select(Date, Latitude, Longitude) %>%
      distinct() %>% 
    rename(date = "Date", lat = "Latitude", lon = "Longitude")),
  tz = "America/Costa_Rica", keep = c("sunrise", "sunset")
), by = join_by(Date == date, Latitude == lat, Longitude == lon), relationship = "many-to-many") %>% 
  rename(Sunrise = sunrise, Sunset = sunset)

# Calculate daylight hours & percent of sample during daylight for all samples
SiteData <- SiteData %>%
  mutate(DaylightHrs = parse_number(as.character(case_when(
    StartTime > Sunrise & EndTime < Sunset ~ difftime(EndTime, StartTime, units = "hours"),
    StartTime < Sunset & EndTime > Sunset ~ difftime(Sunset, StartTime, units = "hours"),
    StartTime < Sunrise & EndTime > Sunrise ~ difftime(EndTime, Sunrise, units = "hours"),
    .default = difftime(EndTime, EndTime, units = "hours")
  ))), .after = EffortUnit) %>% 
  select(-(Sunrise:Sunset)) %>%
  mutate(DaylightPercent = DaylightHrs / as.numeric(difftime(EndTime, StartTime, units = "hours")) * 100, .after = DaylightHrs)

# Read-in Slope/Aspect & Merge with SiteData. Calculated in ArcGIS Pro from "Costa Rica DEM" by j_nelson, sourced from 30 meter NASA SRTM elevation models.
SiteData <- left_join(SiteData, wb_to_df("../Sites_SlopeAspect.xlsx")) %>%
  relocate(c(SampleID, Site, Visit, Gear, Group)) %>%
  arrange(SampleID)


# Occlusion Image Processing ----------------------------------------------

## Occlusion Data
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("..")
# setwd("./Percent Coverage Images/Cropped")
#
# Occlusion Image Analysis
#
#
##** Occlusion image analysis (~4-6 hrs processing)*
# library(devtools)
# devtools::install_github("hiweller/countcolors")
# library(countcolors)
#
## Use ~24 measured "yellow" values from all images, as RGB unit triplets
# colorMatrix <- matrix(ncol = 3, c(0.533333333333333, 0.376470588235294, 0, 0.56078431372549, 0.345098039215686, 0, 0.635294117647059, 0.454901960784314, 0, 0.682352941176471, 0.462745098039216, 0, 0.709803921568627, 0.482352941176471, 0, 0.725490196078431, 0.509803921568627, 0, 0.811764705882353, 0.607843137254902, 0, 0.823529411764706, 0.635294117647059, 0, 0.870588235294118, 0.701960784313725, 0, 0.905882352941176, 0.733333333333333, 0, 0.913725490196078, 0.788235294117647, 0.431372549019608, 0.941176470588235, 0.729411764705882, 0, 0.941176470588235, 0.8, 0.00392156862745098, 0.964705882352941, 0.823529411764706, 0, 0.996078431372549, 0.87843137254902, 0.372549019607843, 1, 0.670588235294118, 0, 1, 0.768627450980392, 0.00392156862745098, 1, 0.803921568627451, 0, 1, 0.83921568627451, 0, 1, 0.866666666666667, 0.0784313725490196, 0.949, 0.886, 0.02, .949,.914,.42,.949,.663,.133,.949,.898,.18), byrow = T)
# colnames(colorMatrix) <- c("R", "G", "B")
#
### Calculate proportion of all cropped occlusion images that is yellow
# #Testing with newly sampled colors
# ColorCount <- countColorsInDirectory(".", color.range = "spherical", center = colorMatrix, radius = rep(.1, length(colorMatrix[,1])), target.color = "magenta", return.indicator = T, save.indicator = "./Masked")
#
# load("../OcclusionData.RData")
#
# OcclDF <- map_dfr(ColorCount, ~tibble(Openness = .x[2])) %>%
#   mutate(Image = names(ColorCount)) %>%
#   mutate(Occlusion = 100*(1 - as.numeric(Openness))) %>%
#   mutate(Net = str_sub_all(Image, end = -2)) %>%
#   separate(Net, into = c("Site", "Group"), sep = "_") %>%
#   mutate(Gear = "Centipede Net") %>%
#   mutate(.by = c(Site, Gear, Group), OcclusionGroupAvg = mean(Occlusion), OcclusionGroupSTDev = sd(Occlusion)) %>%
#   mutate(.by = Site, OcclusionSiteAvg = mean(Occlusion), OcclusionSiteSTDev = sd(Occlusion)) %>%
#   select(Site, Gear, Group, OcclusionGroupAvg, OcclusionGroupSTDev, OcclusionSiteAvg, OcclusionSiteSTDev) %>%
#   distinct()
# save(OcclDF, file = "../Occlusion.RData")
# rm(ColorCount, colorMatrix)
#
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Merge Site Data with Occlusion Data if not re-processing above.

# Skip to here if not re-processing occlusion images------------------------------------
load("../Occlusion.RData")
OcclDF$Site <- str_replace_all(OcclDF$Site, "PuntaMorales", "Punta Morales")
OcclDF$Group <- as.numeric(OcclDF$Group)

# Join Occlusion data with SiteData
SiteData <- left_join(SiteData, OcclDF[, c(1:5)]) %>%
  relocate(c(SampleID, Site, Visit, Gear, Group))
rm(OcclDF)

# Read in fish data & remove two unidentified small fish. Likely small enough to escape, potentially too small to ID even if preserved. Assumed to be non-novel species already represented in dataset.
IndivData <- wb_to_df("../Sampling Data.xlsx", sheet = "AllSamples_Fish", cols = "A:I") %>%
  filter(Species != "Anchoa sp. *" & Species != "Juvenil perciformes*") %>%
  select(-c(Collected, Notes))

# Batch weights & counts when n>30 for a species in a single gear
BatchData <- wb_to_df("../Sampling Data.xlsx", sheet = "AllSamples_Batch") %>%
  select(-TotalWeight_g)

# Combine individual and batch fish data, expanding Batch to 1 row per fish
FishData <- bind_rows(IndivData, as.data.frame(lapply(BatchData, rep, BatchData$Number))) %>%
  select(-Number) %>%
  mutate(SampleID = paste(substr(Site, 1, 3), substr(Visit, 1, 1), "_", substr(Gear, 1, 4), sep = ""), .before = Site)

# Merging Fish data with Site data
MergedData <- full_join(FishData, 
                        SiteData %>%
                          group_by(Site, Visit, Gear, Group) %>% 
                          uncount(
                            ifelse(Gear == 'Centipede Net' | Group == "0" | is.na(Group), 1, max(Effort, na.rm = TRUE)), 
                            .remove = FALSE
                          ) %>%
                          group_by(SampleID) %>%
                          mutate(Group = if_else(Gear == 'Centipede Net', Group, Group + row_number() - 1)) %>% 
                          ungroup(), 
                        by = join_by(SampleID, Site, Visit, Gear, Group)) %>%
  mutate(Count = ifelse(is.na(Species), 0, 1), .after = Species) %>%
  relocate(c(SampleID, Site, Visit, Gear, Group)) %>%
  relocate(Notes, .after = last_col())

rm(IndivData, FishData, BatchData)

## Create list of Species
load("data/SpeciesList.RData")

# SpeciesList <- MergedData %>%
#   filter(!is.na(Species)) %>%
#   summarize(Abundance = sum(Count), .by = Species) %>%
#   mutate(SpeciesRank = rank(-Abundance, ties.method = "last"), .before = Species) %>% 
#   left_join((fb_tbl("species") %>% 
#   left_join(fb_tbl("families") %>% 
#               select(FamCode, Family), by = "FamCode") %>% 
#   mutate(Species = paste(Genus, Species)) %>% 
#   filter(Species %in% MergedData$Species) %>% 
#   collect()), by = "Species") %>% 
#   relocate(SpeciesRank, Species, Abundance, Genus, Family, FBname) %>% 
#   mutate(FBname = case_when(
#     Species == "Bathygobius andrei" ~ "Estuarine frillfin",
#     Species == "Diapterus brevirostris" ~ "Peruvian mojarra",
#     Species == "Gerres simillimus" ~ "Yellowfin mojarra",
#     Species == "Sphoeroides rosenblatti" ~ "Oval puffer",
#     .default = FBname))
#   
# # # Check for missing data
# # filter(SpeciesList, is.na(FBname) | is.na(Genus))
# 
# save(SpeciesList, file = "data/SpeciesList.RData")

FamilyList <- SpeciesList %>%
  select(Family, Abundance) %>%
  summarize(Abundance = sum(Abundance), .by = Family) %>%
  mutate(FamilyRank = rank(-Abundance, ties.method = "first"), .before = Family) %>%
  arrange(FamilyRank)


# Calculate Sample averages + standard deviations for Occlusion, Slope, Aspect, and Site+Visit averages + standard deviations for all other metrics
# Fill values from centipede nets to all gears
SampleData <- SiteData %>%
  select(-c(Group)) %>%
  filter(Effort > 0) %>%
  mutate(Occlusion = mean(OcclusionGroupAvg, na.rm = T), Occlusion_SD = sd(OcclusionGroupAvg, na.rm = T), .by = Site) %>%
  select(-c(OcclusionGroupAvg, OcclusionGroupSTDev)) %>%
  reframe(across(where(is.character), first),
    Effort = if_else(Gear == "Centipede Net", sum(Effort), max(Effort)),
    StartTime = min(StartTime),
    EndTime = max(EndTime),
    Latitude = median(Latitude),
    Longitude = median(Longitude),
    Occlusion = first(Occlusion),
    Occlusion_SD = first(Occlusion_SD),
    MudDominant = first(MudDominant),
    Steepness = first(Steepness),
    across(
      where(is.numeric) & !c(Effort, Latitude, Longitude, Slope, Aspect, Occlusion, Occlusion_SD, MudDominant, Steepness),
      list(Avg = mean, SD = sd)
    ),
    .by = c(Site, Visit, Gear)
  ) %>%
  select(-c(pH_SD, Temperature_C_SD, DO_PPM_SD, DaylightHrs_SD, DaylightPercent_SD)) %>%
  rename_with(~ str_remove_all(.x, "(_Avg|_C|_cm|_PPM)")) %>% # Tidy up names
  relocate(SampleID, Site, Visit, Gear, Season, Effort, EffortUnit) %>%
  group_by(Site, Visit) %>%
  fill(SecchiDepth:Temperature, .direction = "downup") %>% # Fill Cent values across other gears at same visit
  ungroup() %>%
  distinct() %>%
  arrange(SampleID)

# Create Community matrix (Filtering invalid centipede net sample tiv2)
Sample_Community_Matrix <- dcast(MergedData %>% filter(Effort > 0 & SampleID != 'Tiv2_Cent'), SampleID ~ Species, value.var = "Count", fun.aggregate = length, fill = 0) %>%
  column_to_rownames(var = "SampleID") %>%
  t() %>%
  as.data.frame() %>%
  filter(rownames(.) != "NA")

# # Add Shannon & Simpson diversity to Sample data (Don't forget to set to NA when Abundance = 0 before using these data!)
SampleData <- left_join(SampleData, tibble(SampleID = colnames(Sample_Community_Matrix), Shannon = exp(diversity(t(Sample_Community_Matrix), index = "shannon")), Simpson = diversity(t(Sample_Community_Matrix), index = "invsimpson")), by = join_by(SampleID))

# Average Effort by Gear
summarise(SampleData, MeanEffort = mean(Effort, na.rm = TRUE), Unit = first(EffortUnit), .by = Gear) %>%
  mutate(GroupEffort = case_when(Gear == "Centipede Net" ~ MeanEffort / 3))

# Add Fish Sample diversity()summary Values & Standardized CPUE (Standard Sample = 10 Cast Net Throws, 4.64 hr soak of 1 centipede net group, 1.5 seine hauls)
SampleData <- SampleData %>%
  left_join(
    MergedData %>%
      filter(Effort > 0) %>%
      reframe(
        Richness = length(unique(na.omit(Species))),
        Abundance = if (Richness > 0) sum(Count, na.rm = TRUE) else 0,
        Biomass = if (Richness > 0) sum(Weight_g, AverageWeight_g, na.rm = TRUE) else 0,
        AvgLength = if (Richness > 0) mean(StandardLength_mm, na.rm = TRUE) else NaN,
        MinLength = if (Richness > 0) min(StandardLength_mm, na.rm = TRUE) else NaN,
        MaxLength = if (Richness > 0) max(StandardLength_mm, na.rm = TRUE) else NaN,
        AvgWeight = if (Richness > 0) mean(c(AverageWeight_g, Weight_g), na.rm = TRUE) else NaN,
        MinWeight = if (Richness > 0) min(c(AverageWeight_g, Weight_g), na.rm = TRUE) else NaN,
        MaxWeight = if (Richness > 0) max(AverageWeight_g, Weight_g, na.rm = TRUE) else NaN,
        .by = SampleID
      ) %>%
      distinct(),
    by = "SampleID"
  ) %>%
  mutate(
    StandardEffort = case_when(
      Gear == "Cast Net" ~ Effort / 10,
      Gear == "Centipede Net" ~ (Effort / 3) / 4.64,
      Gear == "Seine" ~ Effort / 1.5
    ),
    CPUE = Abundance / StandardEffort,
    logCPUE = log(CPUE + 1),
    BPUE = Biomass / StandardEffort,
    Shannon = if_else(Richness == 0, 0, Shannon),
    Simpson = if_else(Richness == 0, 0, Simpson)
  ) %>%
  mutate(across(everything(), ~ replace(.x, is.infinite(.x) | is.nan(.x), NA))) %>%
  relocate(SampleID, Abundance, CPUE, logCPUE, Richness, Shannon, Simpson, BPUE, Effort, EffortUnit, StandardEffort) %>%
  arrange(SampleID)

# Species abundances by gear
GearSpeciesGrid <- MergedData %>%
  filter(!is.na(Species)) %>%
  select(Species, Gear) %>%
  mutate("Cast Net" = sum(if_else(Gear == "Cast Net", 1, 0)), "Centipede Net" = sum(if_else(Gear == "Centipede Net", 1, 0)), "Seine" = sum(if_else(Gear == "Seine", 1, 0)), .by = Species) %>%
  select(-Gear) %>%
  distinct() %>%
  arrange(Species)

# Checkbox species by gear
GearSpeciesCheck <- MergedData %>%
  filter(!is.na(Species)) %>%
  select(Species, Gear) %>%
  mutate("Cast Net" = if_else(sum(if_else(Gear == "Cast Net", 1, 0)) > 0, "✓", ""), "Centipede Net" = if_else(sum(if_else(Gear == "Centipede Net", 1, 0)) > 0, "✓", ""), "Seine" = if_else(sum(if_else(Gear == "Seine", 1, 0)) > 0, "✓", ""), .by = Species) %>%
  select(-Gear) %>%
  arrange(Species) %>%
  distinct()

# --- NEW: Identify complete site visits BEFORE building matrices ---
# This ensures we only pool data from visits where all required gears were present.
# We use SiteData (which has all valid samples, Effort > 0), exclude invalid Tiv2_Cent
valid_samples <- SiteData %>%
  filter(SampleID != 'Tiv2_Cent') %>%
  select(SampleID, Gear) %>%
  mutate(SiteVisit = substr(SampleID, 1, 4)) %>%
  distinct(SiteVisit, Gear)

# --- 2. Find which SiteVisits have which valid gears ---
visits_with_cast <- valid_samples %>% filter(Gear == "Cast Net") %>% pull(SiteVisit)
visits_with_cent <- valid_samples %>% filter(Gear == "Centipede Net") %>% pull(SiteVisit) # "Tiv2" is now correctly excluded
visits_with_seine <- valid_samples %>% filter(Gear == "Seine") %>% pull(SiteVisit)

# --- 3. Find the intersections (visits that have ALL required gears) ---
visits_cast_cent <- intersect(visits_with_cast, visits_with_cent)
visits_cast_seine <- intersect(visits_with_cast, visits_with_seine) # This will correctly include "Tiv2"
visits_cent_seine <- intersect(visits_with_cent, visits_with_seine)
visits_all_gears <- intersect(visits_cast_seine, visits_with_cent) # This will correctly exclude "Tiv2"


# --- MODIFIED: Build Incidence_Matrices using the complete visit lists ---
Incidence_Matrices <- list(
  "Cast Net" = dcast(MergedData %>% filter(Gear == "Cast Net" & Effort != 0), Species ~ SampleID, value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  # This one is correct (filters the single invalid sample)
  "Centipede Net" = dcast(MergedData %>% filter(Gear == "Centipede Net" & Effort != 0 & SampleID != 'Tiv2_Cent'), Species ~ SampleID, value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  "Seine" = dcast(MergedData %>% filter(Gear == "Seine" & Effort != 0), Species ~ SampleID, value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  # FIX: Filter MergedData to only include site visits that have BOTH gears
  "Cast Net & Centipede Net" = dcast(MergedData %>% 
                                       filter(substr(SampleID, 1, 4) %in% visits_cast_cent & (Gear == "Cast Net" | Gear == "Centipede Net")), 
                                     Species ~ substr(SampleID, 1, 4), value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  # FIX: Filter MergedData to only include site visits that have BOTH gears
  "Cast Net & Seine" = dcast(MergedData %>% 
                               filter(substr(SampleID, 1, 4) %in% visits_cast_seine & (Gear == "Cast Net" | Gear == "Seine")), 
                             Species ~ substr(SampleID, 1, 4), value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  # FIX: Filter MergedData to only include site visits that have BOTH gears
  "Centipede Net & Seine" = dcast(MergedData %>% 
                                    filter(substr(SampleID, 1, 4) %in% visits_cent_seine & (Gear == "Centipede Net" | Gear == "Seine")), 
                                  Species ~ substr(SampleID, 1, 4), value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species"),
  
  # FIX: Filter MergedData to only include site visits that have ALL THREE gears
  "All Gears" = dcast(MergedData %>% 
                        filter(substr(SampleID, 1, 4) %in% visits_all_gears), 
                      Species ~ substr(SampleID, 1, 4), value.var = "Count", fun.aggregate = length, fill = 0) %>%
    mutate(across(where(is.numeric), ~ replace(., . != 0, 1))) %>%
    filter(!is.na(Species)) %>%
    column_to_rownames(var = "Species")
)

# save.image("data/WrangledData.RData")