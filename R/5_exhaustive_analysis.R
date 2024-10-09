# Directory creation function
create_directories <- function(base_dir, sub_dirs) {

  # Validate inputs
  if (!is.character(base_dir) || !is.character(sub_dirs)) {
    stop("Error: 'base_dir' and 'sub_dirs' must be character strings.")
  }

  # Create base directory if it doesn't exist
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }

  # Create each sub-directory
  for (sub_dir in sub_dirs) {
    # Construct the full path
    full_path <- file.path(base_dir, sub_dir)

    # Create the sub-directory
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
    }
  }

}



#---------------------------------- EXPORT -------------------------------------

#' @title Subset master geo-linked WBES dataset based on survey, country, or region.
#'
#' @description
#' The exhaustive_analysis() function runs all possible analysis offered by the wbfirmadaptation package, saving results in a series of folders within a specified directory.
#'
#' @import haven
#' @import dplyr
#' @import ggplot2
#' @import sandwich
#' @import stargazer
#'
#' @param data_file_path A string of the user's file path to the raw data.
#' @param filter_value A string that indicates the value of the country, survey (CountryYear), or region the user wants to analyze.
#' @param subset_type A string that indicates the type of subsetting: "survey", "country", or "region".
#' @param output_directory A string containing the directory in which the results will be saved.
#'
#' @return All possible analysis (tables and charts from regressions) offered by the wbfirmadaptation package saved in output_directory.
#'
#' @export
exhaustive_analysis <- function(data_file_path, filter_value, subset_type, html_or_tex, output_directory){

  # Validate inputs
  if (!is.character(data_file_path) || length(data_file_path) != 1) {
    stop("Error: 'data_file_path' must be a single string.")
  }

  if (!is.character(filter_value) || length(filter_value) != 1) {
    stop("Error: 'filter_value' must be a single string.")
  }

  if (!is.character(subset_type) || length(subset_type) != 1) {
    stop("Error: 'subset_type' must be a single string.")
  }

  if (!subset_type %in% c("survey", "country", "region")) {
    stop("Error: 'subset_type' must be either 'survey', 'country', or 'region'.")
  }

  if (!is.character(html_or_tex) || length(html_or_tex) != 1) {
    stop("Error: 'html_or_tex' must be a single string.")
  }

  if (!html_or_tex %in% c("html", "tex")) {
    stop("Error: 'html_or_tex' must be either 'html' or 'tex'.")
  }

  if (!is.character(output_directory) || length(output_directory) != 1) {
    stop("Error: 'output_directory' must be a single string.")
  }

  # Create directories
  subdirectories <- c("Climate_Impact_On_Firm_Performance",
                      "Climate_Impact_On_Firm_Performance/Charts", "Climate_Impact_On_Firm_Performance/Tables",

                      "Interactions",
                      "Interactions/Sales", "Interactions/SalesPerWorker", "Interactions/Investment"
  )

  create_directories(output_directory, subdirectories)

  # --- Step 1. Subset data ---
  data <- wbfirmadaptationtool::subset_data(data_file_path, filter_value, subset_type)

  # --- Step 2. Prepare data for regressions ---
  reg_data <- wbfirmadaptationtool::prep_reg_data(data)

  # --- Step 3. Climate impact on firm performance regressions ---

  # Charts
  for (level_or_difference in c("Level", "Difference")){

    wbfirmadaptationtool::climate_on_firm_regs_charts(data = reg_data,
                                                      level_or_difference = level_or_difference,
                                                      output_directory = paste0(output_directory, "/Climate_Impact_On_Firm_Performance/Charts"))

  }

  # Tables
  for (level_or_difference in c("Level", "Difference")){

    wbfirmadaptationtool::climate_on_firm_regs_tables(data = reg_data,
                                                      level_or_difference = level_or_difference,
                                                      html_or_tex = html_or_tex,
                                                      output_directory = paste0(output_directory, "/Climate_Impact_On_Firm_Performance/Tables"))

  }

  # --- Step 4. Interactions with firm characteristics/policy vars

  # Characteristics and policy vars to include in regs
  characteristics_policy_vars <- c("Young", "Small")

  # Loop through dependent variables
  for (dependent_var in c("Sales", "SalesPerWorker", "Investment")){

    if (dependent_var %in% c("Sales", "SalesPerWorker")){
      var_name_append <- "Log"
    } else {
      var_name_append <- "Binary"
    }

    # Loop through climate variables
    for (climate_var in c("Temperature", "TemperatureVolatility", "HeatDays")){

      # Loop through using levels or differences
      for (level_or_difference in c("Level", "Difference")){

        wbfirmadaptationtool::interaction_reg_table(reg_data = reg_data,
                                                    firm_outcome = paste0(dependent_var, var_name_append),
                                                    climate_var = climate_var,
                                                    level_or_difference = level_or_difference,
                                                    firm_characteristics = characteristics_policy_vars,
                                                    html_or_tex = html_or_tex,
                                                    filename = paste0(climate_var, "_", level_or_difference),
                                                    output_directory = paste0(output_directory, "/Interactions/", dependent_var))
      }
    }
  }



}



