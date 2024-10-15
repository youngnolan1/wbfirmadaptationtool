# Internal interaction regression function
interaction_reg <- function(reg_data, firm_outcome, climate_var, firm_characteristic){

  # Create interaction term as a string
  interaction_term <- paste(climate_var, "*", firm_characteristic)

  # Initialize the base formula with the climate variable and the firm characteristic interaction
  formula_components <- c(interaction_term)

  # Conditionally add Sector if there is more than one unique value
  if(length(unique(reg_data$Sector)) > 1) {
    formula_components <- c(formula_components, "Sector")
  }

  # Conditionally add Country if there is more than one unique value
  if(length(unique(reg_data$Country)) > 1) {
    formula_components <- c(formula_components, "Country")
  }

  # Conditionally add Year if there is more than one unique value
  if(length(unique(reg_data$Year)) > 1) {
    formula_components <- c(formula_components, "as.factor(Year)")
  }

  # Build the final formula dynamically
  formula <- as.formula(paste(firm_outcome, " ~ ", paste(formula_components, collapse = " + ")))

  # Run the regression
  model <- lm(formula,
              data = reg_data)

  # Calculate clustered standard errors
  se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = ~reg_data$GridID)))

  # Return both the model and the standard errors
  return(list(model = model, se = se))

}

# -------------------------- EXPORT FUNCTIONS ----------------------------------

#' @title Table showing impact of the interaction between firm characteristics and climate variables on firm outcomes.
#'
#' @description
#' Exports a regression result table featuring interactions between the firm characteristics and climate variables provided.
#'
#' @import stargazer
#'
#' @param data Regression-ready dataset prepared by prep_reg_data.
#' @param firm_outcome Dependent variable passed as a string, see guidance note for options.
#' @param climate_var Independent climate variable passed as a string, options include "Temperature", "TemperatureVolatility", and "HeatDays".
#' @param level_or_difference Either "Level" or "Difference"; indicates whether for the given climate variable, its survey year level or the difference of this level from its long-run mean should be used.
#' @param firm_characteristics Independent firm characteristic/policy variable passed as a string or list of strings, see guidance note for options.
#' @param html_or_tex Either "html" or "tex"; indicates whether the table will be saved in .html or .tex format.
#' @param filename Name of the file to be saved, excluding the file extension.
#' @param output_directory File path for where the table will be saved e.g. "C:/Users/Nolan/Project".
#'
#' @return Regression results table for the variables provided saved in the output_directory.
#'
#' @note Remember to use forward slashes "/" in the folder path.
#'
#' @export
interaction_regs_tables <- function(reg_data, firm_outcome, climate_var, level_or_difference, firm_characteristics, html_or_tex, filename, output_directory){

  # Validate inputs
  if (!is.data.frame(reg_data)) {
    stop("Error: Input must be an R data frame.")
  }

  if (!is.character(firm_outcome) || length(firm_outcome) != 1) {
    stop("Error: 'firm_outcome' must be a single string.")
  }

  if (!is.character(climate_var) || length(climate_var) != 1) {
    stop("Error: 'climate_var' must be a single string.")
  }

  if (!is.character(level_or_difference) || length(level_or_difference) != 1) {
    stop("Error: 'level_or_difference' must be a single string.")
  }

  if (!level_or_difference %in% c("Level", "Difference")) {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  if (!is.character(html_or_tex) || length(html_or_tex) != 1) {
    stop("Error: 'html_or_tex' must be a single string.")
  }

  if (!html_or_tex %in% c("html", "tex")) {
    stop("Error: 'html_or_tex' must be either 'html' or 'tex'.")
  }

  if (!is.character(filename) || length(filename) != 1) {
    stop("Error: 'filename' must be a single string.")
  }

  if (!is.character(output_directory) || length(output_directory) != 1) {
    stop("Error: 'output_directory' must be a single string.")
  }

  # Define climate var - if Level, leave as is; if Difference, append Difference to climate_var name
  climate_var <- ifelse(level_or_difference == "Level", climate_var, paste0(climate_var, "Difference"))

  # Initialize empty lists for models and standard errors
  models <- list()  # Store the models
  ses <- list()     # Store the clustered standard errors

  # Loop through firm characteristics, run interaction_reg, and store model and SE
  for (characteristic in firm_characteristics) {

    # Run the interaction regression for each characteristic
    result <- interaction_reg(reg_data, firm_outcome, climate_var, characteristic)

    # Extract the model and standard errors from the result
    models[[characteristic]] <- result$model
    ses[[characteristic]] <- result$se
  }

  # Adjust output file name based on type
  output_path <- paste0(output_directory, "/", filename, ".", html_or_tex)

  # The note under the regression results table changes depending on level or difference
  if (level_or_difference == "Level"){
    stargazer_note <- "Each firm performance variable is regressed on the given climate variable and its interaction with the given firm characteristic or business environment variable, along with fixed effects (FE) which depend on data coverage. The climate variable is in levels - the local average in the FY of the relevant survey. Estimated effects shown by the coefficients are in terms of one standard deviation increases for the climate variables and continuous firm characteristic/policy variables. We report standard errors clustered at the climate variable level (corresponding to climate data grids) in parentheses."
  } else if (level_or_difference == "Difference") {
    stargazer_note <- "Each firm performance variable is regressed on the given climate variable and its interaction with the given firm characteristic or business environment variable, along with fixed effects (FE) which depend on data coverage. The climate variable is in differences - the local average temperature in the FY of the relevant survey minus the local long-run mean, calculated for the 1980-2008 period. Estimated effects shown by the coefficients are in terms of one standard deviation increases for the climate variables and continuous firm characteristic/policy variables. We report standard errors clustered at the climate variable level (corresponding to climate data grids) in parentheses."
  } else {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  # Use stargazer to combine all models into a results table with clustered SEs
  sink(tempfile())  # Prevent direct output
  on.exit(sink(), add = TRUE)  # Ensure sink is turned off on exit

  stargazer(models, # Combine all models into one table
            type = html_or_tex, # Create either html or tex table
            se = ses,  # Pass the list of standard errors to stargazer
            title = paste0("Interactions Results"), # Define table title
            out = output_path, # Save file to path defined above
            omit.stat = c("f", "ser"),  # Customize which statistics to omit
            align = TRUE,  # Align columns for better readability
            no.space = TRUE,  # Eliminate extra spaces between lines
            notes = stargazer_note, # Add stargazer note defined above
            notes.align = "l") # Align note to left

}

