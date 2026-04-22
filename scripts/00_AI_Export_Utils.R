# 00_AI_Export_Utils.R
# This script defines utilities for exporting data to AI-friendly formats.

library(broom.mixed)
library(performance)
library(DHARMa)

# Create output directory if it doesn't exist
if (!dir.exists("output/AI_Ready_Data/")) {
  dir.create("output/AI_Ready_Data/", recursive = TRUE)
}

#' Export model information to AI-ready formats
#'
#' @param model_obj The fitted model object
#' @param model_name A string used to name the output files
export_to_ai <- function(model_obj, model_name) {
  
  # 1. Save summary() as .txt
  txt_path <- paste0("output/AI_Ready_Data/", model_name, "_summary.txt")
  capture.output(summary(model_obj), file = txt_path)
  
  # 2. Save tidy() output as .csv
  csv_path <- paste0("output/AI_Ready_Data/", model_name, "_tidy.csv")
  tryCatch({
    tidy_df <- broom.mixed::tidy(model_obj, conf.int = TRUE)
    write.csv(tidy_df, csv_path, row.names = FALSE)
  }, error = function(e) {
    message("Could not run broom.mixed::tidy on model: ", model_name, ". Error: ", e$message)
  })
  
  # 3. Save collinearity check as .csv (if applicable)
  coll_path <- paste0("output/AI_Ready_Data/", model_name, "_collinearity.csv")
  tryCatch({
    coll_df <- as.data.frame(performance::check_collinearity(model_obj))
    if(nrow(coll_df) > 0) {
      write.csv(coll_df, coll_path, row.names = FALSE)
    }
  }, error = function(e) {
    message("Could not run check_collinearity on model: ", model_name, ". Skipping.")
  })
  
  # 4. Save DHARMa plot as .png
  png_path <- paste0("output/AI_Ready_Data/", model_name, "_DHARMa.png")
  tryCatch({
    png(filename = png_path, width = 800, height = 500)
    sim_res <- DHARMa::simulateResiduals(fittedModel = model_obj, plot = FALSE)
    plot(sim_res)
    dev.off()
  }, error = function(e) {
    if (length(dev.list()) > 0) dev.off() # Ensure device is closed on error
    message("Could not run simulateResiduals on model: ", model_name, ". Error: ", e$message)
  })
}
