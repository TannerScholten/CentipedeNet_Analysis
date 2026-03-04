
library(tidyverse)
library(rlang)

# Load pre-wrangled dataframes and remove what I don't need right now
load("data/WrangledData.RData")

#**CAREFUL**! Clear environment except listed objects
rm(list=setdiff(ls(), c("SampleData")))


# Colors & Shapes for theming
palette("Tableau")

# GearColors <- c("All Gears" = "#CC79A7", "Cast Net" = "#E69F00", "Centipede Net" = "#56B4E9", "Seine" = "#009E73")

GearColors <- function() {
  return(c('#E69F00', '#56B4E9', '#009E73'))
}

SiteColors <- function() {
  palette.colors(6, "Tableau")[1:6] 
}
  
  
# GearColorsAll <- c("All Gears" = "#CC79A7", "Cast Net" = "#E69F00", "Cast Net & Centipede Net" = "#c7ab10", "Cast Net & Seine" = "#0072B2", "Centipede Net" = "#56B4E9", "Centipede Net & Seine" = "#D55E00", "Seine" = "#009E73")
# 
# GearShapes <- c("All Gears" = 18, "Cast Net" = 16, "Cast Net & Centipede Net" = 10, "Cast Net & Seine" = 9, "Centipede Net" = 15, "Centipede Net & Seine" = 14, "Seine" = 17)

# Predictors + Corresponding Labels
Predictors <- data.frame(Variable = names(SampleData %>% select(-c(SampleID, DiversityRaw, CPUE, BPUE, MinLength, MaxLength, MinWeight, MaxWeight, Visit, EffortUnit, StartTime, EndTime, Latitude, Longitude, Site, Gear, Season))), Label = c("Abundance (n)", "log-Abundance (log(n + 1))", "Species Richness", "Shannon Diversity", "Biomass (g)", "Mean Length (mm)", "Mean Weight (g)", "Effort", "Percent Occlusion", "Occlusion Variability (Standard Deviation)", "Mud Dominance (0 or 1)", "Steepness", "Hours of Daylight Sampling", "Percent of Daylight Sampling", "Secchi Depth (cm)", "Secchi Depth Variability (Standard Deviation)", "pH", "Dissolved Oxygen (mg/L)", "Salinity (PSU)", "Salinity Variability (Standard Deviation)", "Temperature (°C)"))


# Boxplots

# Effort by Gear & Site
labeller <- function(labels){
  new_labels <- c("Cast Net" = "Cast Net (Throws)", "Centipede Net" = "Centipede Net (Net Group Hours)", "Seine" = "Seine (Hauls)")
  return(new_labels[labels])
}

Effort <- ggplot(SampleData, aes(y = Site, x = Effort, fill = Gear)) +
  geom_boxplot(alpha = 0.65) +  # Create boxplots
  scale_fill_manual(values = GearColors()) +
  facet_wrap(. ~ Gear, scales = "free", nrow = 3,  strip.position = "top", labeller = as_labeller(labeller)) +
  theme_bw() +
  theme(legend.position = "none") 
rm(labeller)


# Boxplot Template Function

PlotBySite <- function(Metric, Label) {
  ggplot(SampleData, aes(x = {{Metric}}, Site)) +
    geom_boxplot(fill = SiteColors(), alpha = 0.8) +
    theme_bw() +
    labs(x = Label, y = "Site")
}

Plots <- list(Abundance <- PlotBySite(Abundance, Predictors$Label[1]),
logAbundance <- PlotBySite(logAbundance, Predictors$Label[2]),
Richness <- PlotBySite(Richness, Predictors$Label[3]),
Diversity <- PlotBySite(Diversity, Predictors$Label[4]),
Biomass <- PlotBySite(Biomass, Predictors$Label[5]),
AvgLength <- PlotBySite(AvgLength, Predictors$Label[6]),
AvgWeight <- PlotBySite(AvgWeight, Predictors$Label[7]),
#Effort is plotted above
Effort <- Effort,
Occlusion <- ggplot(SampleData %>% select(Site, Occlusion) %>% unique(), aes(y = Site, x = Occlusion, fill = Site)) +
  geom_bar(stat = "identity", fill = SiteColors(), alpha = .8) +
  labs(x = "Percent Occlusion") +
  theme_bw(),
Occlusion_SD <- ggplot(SampleData %>% select(Site, Occlusion_SD) %>% unique(), aes(y = Site, x = Occlusion_SD, fill = Site)) +
  geom_bar(stat = "identity", fill = SiteColors(), alpha = .8) +
  labs(x = "Occlusion Variability (Standard Deviation)") +
  theme_bw(),
MudDominant <- ggplot(SampleData %>% select(Site, MudDominant) %>% unique(), aes(x = Site, y = MudDominant, fill = Site)) +
  geom_bar(stat = "identity", fill = SiteColors(), alpha = .8) +
  labs(y = "Mud Dominance") +
  scale_y_continuous(breaks = c(0,1), labels = c("Coarser Substrate", "Mud Dominant")) +
  theme_bw(),
Steepness <- ggplot(SampleData %>% select(Site, Steepness) %>% unique(), aes(x = Site, y = Steepness, fill = Site)) +
  geom_bar(stat = "identity", fill = SiteColors(), alpha = .8) +
  labs(y = "Steepness Category (0, 1, 2)") +
  scale_y_continuous(breaks = c(0,1,2), labels = c("Low", "Medium", "High")) +
  theme_bw(),
DaylightHrs <- PlotBySite(DaylightHrs, Predictors$Label[13]),
DaylightPercent <- PlotBySite(DaylightPercent, Predictors$Label[14]),
SecchiDepth <- PlotBySite(SecchiDepth, Predictors$Label[15]),
SecchiDepth_SD <- PlotBySite(SecchiDepth_SD, Predictors$Label[16]),
pH <- PlotBySite(pH, Predictors$Label[17]),
DO <- PlotBySite(DO, Predictors$Label[18]),
PSU<- PlotBySite(PSU, Predictors$Label[19]),
PSU_SD <- PlotBySite(PSU_SD, Predictors$Label[20]),
Temperature <- PlotBySite(Temperature, Predictors$Label[21]))

for (i in 1:length(Plots)) {
  # Get the current plot object and its name
  current_plot <- Plots[[i]]
  plot_name <- Predictors$Variable[i]  # Access name from the list names vector
  
  # Save the plot using ggsave with the name and ".png" extension
  ggsave(filename = paste0("../Figures & Tables/Appendix Plots/", plot_name, ".png"), plot = current_plot)
}


