# Data Dictionary / Codebook

This document defines the variables and covariates used across the `ThesisAnalysis_AI_Sandbox` dataset to ensure accurate interpretation of model coefficients and descriptive statistics by AI tools.

## Environmental Covariates
- **SecchiDepth**: Water clarity measured in centimeters (cm).
- **Temperature**: Water temperature measured in Celsius (°C).
- **Steepness**: Bank steepness, a categorical variable with levels: `Low`, `Medium`, and `High`.
- **Occlusion**: The percentage (0-100) of the 'understory' (under mangrove trees) that is occupied by tree structure (limbs, roots, branches) as opposed to open space.
- **DaylightPercent**: The percentage (0-100) of the sampling period that occurred during daylight hours.
- **MudDominant**: A binary indicator (1/0 or TRUE/FALSE) where 1/TRUE indicates that Mud is the dominant substrate at the site.
- **Season**: Categorical variable indicating the season, with levels: `Rainy` and `Dry`.

## Sampling Variables
- **Gear**: The sampling method used (e.g., `Cast Net`, `Centipede Net`, `Seine`).
- **Site**: The name/identifier of the sampling site.
- **Visit**: The sampling event/visit number.
- **Effort**: The amount of sampling effort. Units differ by `Gear` type:
  - Cast Net: Throws
  - Centipede Net: Net Group Hours
  - Seine: Hauls
- **Abundance**: Total count of individuals caught (Catch).
- **Richness**: Total number of unique species caught.
- **Shannon / Simpson**: Diversity indices measuring the balance and variety of species.
- **Biomass**: Total weight of the catch (in grams).
- **CPUE**: Catch Per Unit Effort (calculated as `Abundance / Effort`). *Note: CPUE may not be actively used in the final analysis/manuscript.*

## Usage Note
When drafting manuscript results, ensure that unit-specific coefficients (like a +1 increase in SecchiDepth) are correctly interpreted according to the units listed above (e.g., a 1 cm increase, not 1 meter).
