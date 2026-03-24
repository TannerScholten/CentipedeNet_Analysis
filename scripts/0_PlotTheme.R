# AFS Global Plot Theme Settings (Times Roman, 9pt text, simple background)
library(extrafont)

# Load Windows fonts from device and set serif as default.
# Note: Cowplot/Aptos specific margin fixes (set_null_device) have been removed as they are no longer needed.
loadfonts(device = "win")
par(family = "serif")

# Set the global ggplot2 theme
theme_set(
  theme_classic(
    base_family = "serif", 
    base_size = 9
  ) +
    theme(
      text = element_text(colour = "black")
    )
)

# Set the global discrete color and fill palettes to Okabe-Ito (colorblind safe)
options(
  ggplot2.discrete.colour = palette.colors(palette = "Okabe-Ito"),
  ggplot2.discrete.fill = palette.colors(palette = "Okabe-Ito")
)

# --- Visual Helper Functions ---

# Standard Gear Colors (Heritage Anchors - Okabe-Ito)
GearColors <- function() {
  return(c("All Gears" = "#555555", "Cast Net" = "#D55E00", "Centipede Net" = "#56B4E9", "Seine" = "#009E73"))
}

# Full Gear Combinations Palette (Heritage-Optimized Okabe-Ito)
GearColorsAll <- function() {
  return(c(
    # --- The Baseline ---
    "All Gears" = "#555555",                 # Slate Grey (Softened from aggressive black)
    
    # --- Single Gears (Anchor Colors) ---
    "Cast Net" = "#D55E00",                  # Vermilion (Warm Anchor)
    "Centipede Net" = "#56B4E9",             # Sky Blue (Cool Anchor)
    "Seine" = "#009E73",                     # Bluish Green (Standard Green)
    
    # --- Combination Gears (Heritage Colors) ---
    "Cast Net & Centipede Net" = "#E69F00",  # Orange (Warm Heritage)
    "Centipede Net & Seine" = "#0072B2",     # Dark Blue (Cool Heritage)
    "Cast Net & Seine" = "#CC79A7"           # Reddish Purple (Distinct Bridge)
  ))
}

GearShapes <- list(
  "All Gears" = 18,                # Solid Diamond
  "Cast Net" = 16,                 # Solid Circle
  "Centipede Net" = 15,            # Solid Square
  "Seine" = 17,                    # Solid Triangle
  "Cast Net & Centipede Net" = 8,  # Star/Asterisk (heavy weight)
  "Cast Net & Seine" = 19,         # Solid Circle (bold)
  "Centipede Net & Seine" = 20     # Solid Bullet
)

GearLines <- function() {
  return(c(
    "Cast Net" = "22",          # Short dash
    "Centipede Net" = "73",     # Long dash
    "Seine" = "1343",           # Dot-dash
    "Cast Net & Centipede Net" = "42",      # Medium dash
    "Cast Net & Seine" = "1242",            # Dense dot-dash
    "Centipede Net & Seine" = "6212",       # Long dash-dot
    "All Gears" = "solid"
  ))
}

SiteColors <- function() {
  palette.colors(6, "Tableau")[1:6]
}

# Multi-panel labeling helper (No periods/brackets per AFS)
Letters <- function() {
  return(c("A", "B", "C", "D", "E", "F", "G", "H", "I"))
}

# --- Shared Plotting Logic ---

# A list of common aesthetic scales for Gear mapping
gear_scales <- list(
  scale_color_manual(values = GearColors()),
  scale_fill_manual(values = GearColors()),
  scale_linetype_manual(values = GearLines()),
  scale_shape_manual(values = GearShapes)
)

# A reusable theme mapping for standard plots that hides redundant legends and manages margins
plot_theme <- 
  theme(legend.position = "none", 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
