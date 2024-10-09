# Internal regression function
reg_model <- function(reg_data, outcome, indep_climate_var) {

  # Filter for complete cases in the outcome variable
  reg_data[[outcome]][is.infinite(reg_data[[outcome]])] <- NA
  reg_data_final <- reg_data[complete.cases(reg_data[[outcome]]), ]

  # Initialize the base formula with the climate variable
  formula_components <- c(indep_climate_var)

  # Conditionally add Sector if there is more than one unique value
  if(length(unique(reg_data_final$Sector)) > 1) {
    formula_components <- c(formula_components, "Sector")
  }

  # Conditionally add Country if there is more than one unique value
  if(length(unique(reg_data_final$Country)) > 1) {
    formula_components <- c(formula_components, "Country")
  }

  # Conditionally add Year if there is more than one unique value
  if(length(unique(reg_data_final$Year)) > 1) {
    formula_components <- c(formula_components, "as.factor(Year)")
  }

  # Build the final formula dynamically
  formula <- as.formula(paste(outcome, " ~ ", paste(formula_components, collapse = " + ")))

  # Run the regression
  model <- lm(formula, data = reg_data_final)

  return(model)
}



# Internal summary chart function
reg_chart <- function(reg_data, indep_climate_var, plot_title){

  # Firm outcomes
  firm_outcomes <- c("SalesLog", "SalesPerWorkerLog", "WagesLog", "WorkersLog",
                    "CapitalUtilizationLog", "EnergyIntensityLog",
                    "PowerOutagesBinary", "InvestmentBinary")

  # Empty dataframe for results
  graph_data <- data.frame(
    firm_outcome = character(),
    coefficient = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric()
  )

  # Loop through firm outcomes and run reg_model for each of them, saving the results
  for (outcome in firm_outcomes) {

    # Run model for the relevant outcome
    model <- reg_model(reg_data, outcome, indep_climate_var)

    # Extract coefficient and CI
    results <- data.frame(firm_outcome = outcome,
                          coefficient = coefficients(model)[indep_climate_var],
                          lower_ci = confint(model)[indep_climate_var, 1],
                          upper_ci = confint(model)[indep_climate_var, 2])

    # Append to dataframe
    graph_data <- rbind(graph_data, results)

  }

  # Add statistical significance column
  graph_data$significant <- ifelse(graph_data$lower_ci <= 0 & graph_data$upper_ci >= 0, "No", "Yes")


  # Updated plot with all bars in the same color
  plot <- ggplot(graph_data, aes(x = firm_outcome, y = coefficient)) +
    geom_bar(stat = "identity", aes(fill = significant), color = "black", width = 0.6) + # Conditional color for bars
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") + # Error bars
    scale_fill_manual(values = c("Yes" = "orange", "No" = "grey")) + # Assign grey if CI contains zero, orange otherwise
    geom_hline(yintercept = 0, color = "black", size = 0.8) + # Add a horizontal line at zero
    theme_minimal() + # Minimal theme
    theme(
      axis.title.x = element_blank(), # Remove x-axis title
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Rotate x-axis labels
      axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Bold y-axis title
      axis.text.y = element_text(size = 14), # Increase y-axis text size
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Center and bold title
      legend.position = "none", # Remove legend
      panel.grid = element_blank(), # Remove all gridlines
      axis.line = element_line(color = "black", size = 0.8), # Add black axis lines
      axis.line.x = element_line(color = "black", size = 0.8), # Add x-axis line
      axis.line.y = element_line(color = "black", size = 0.8) # Add y-axis line
    ) +
    labs(y = "Coefficient", # Y-axis label
         title = plot_title) # Title


  return(plot)
}


# Internal results table function
reg_table <- function(reg_data, indep_climate_var, table_title, html_or_tex, directory){

  # Sales (log(d2))
  s <- reg_model(reg_data, "SalesLog", indep_climate_var)
  s_se <- sqrt(diag(vcovHC(s, type = "HC1", cluster = ~reg_data$GridID)))

  # Productivity (log(d2/l1))
  spw <- reg_model(reg_data, "SalesPerWorkerLog", indep_climate_var)
  spw_se <- sqrt(diag(vcovHC(s, type = "HC1", cluster = ~reg_data$GridID)))

  # Wages (log(n2a))
  w <- reg_model(reg_data, "WagesLog", indep_climate_var)
  w_se <- sqrt(diag(vcovHC(w, type = "HC1", cluster = ~reg_data$GridID)))

  # Employment (log(l1))
  e <- reg_model(reg_data, "WorkersLog", indep_climate_var)
  e_se <- sqrt(diag(vcovHC(e, type = "HC1", cluster = ~reg_data$GridID)))

  # Capital utilization (f1)
  capu <- reg_model(reg_data, "CapitalUtilizationLog", indep_climate_var)
  capu_se <- sqrt(diag(vcovHC(capu, type = "HC1", cluster = ~reg_data$GridID)))

  # Energy intensity (n2b / d2)
  ei <- reg_model(reg_data, "EnergyIntensityLog", indep_climate_var)
  ei_se <- sqrt(diag(vcovHC(ei, type = "HC1", cluster = ~reg_data$GridID)))

  # Power outages (c6)
  pwr <- reg_model(reg_data, "PowerOutagesBinary", indep_climate_var)
  pwr_se <- sqrt(diag(vcovHC(pwr, type = "HC1", cluster = ~reg_data$GridID)))

  # Investment in fixed assets (k4)
  inv <- reg_model(reg_data, "InvestmentBinary", indep_climate_var)
  inv_se <- sqrt(diag(vcovHC(inv, type = "HC1", cluster = ~reg_data$GridID)))

  # The note under the regression results table changes depending on level or difference
  if (grepl("Level", table_title)){
    stargazer_note <- c("Each firm performance variable is regressed on the given climate variable, along with fixed effects (FE) which depend on data coverage. The climate variable is in levels - the local average in the FY of the relevant survey. Coefficients show estimated effects of a one standard deviation change in the climate variable. We report standard errors clustered at the climate variable level (corresponding to climate data grids) in parentheses.")
  } else {
    stargazer_note <- c("Each firm performance variable is regressed on the given climate variable, along with fixed effects (FE) which depend on data coverage. The climate variable is in differences - the local average in the FY of the relevant survey minus the local long-run mean, calculated for the 1980-2008 period. Coefficients show estimated effects of a one standard deviation change in the climate variable. We report standard errors clustered at the climate variable level (corresponding to climate data grids) in parentheses.")
  }

  # Use stargazer to combine all models into a results table with clustered SEs
  sink(tempfile()) # Prevent direct output
  on.exit(sink(), add = TRUE) # Ensure sink is turned off on exit
  stargazer(s, spw, w, e, capu, ei, pwr, inv, # Combine all models into one table
            type = html_or_tex, # Create either html or tex table
            se = c(list(s_se), list(spw_se), list(w_se), list(e_se), list(capu_se), list(ei_se), list(pwr_se), list(inv_se)), # Clustered SEs
            out = directory, # Save file to defined path
            title = paste0("Effect of ", table_title, " on Firm Performance"), # Define table title
            dep.var.labels = c("Sales (log)", "Productivity (log)", "Wages (log)",
                               "Employment (log)", "Capital Utilization (log)",
                               "Energy Intensity (log)", "Power Outages",
                               "Investment in Fixed Assets"),
            omit.stat = c("f", "ser"), # Customize which statistics to omit
            align = TRUE, # Align columns for better readability
            no.space = TRUE, # Eliminate extra spaces between lines
            notes = stargazer_note, # Add stargazer note defined above
            notes.align = "l") # Align note to left
}



# -------------------------- EXPORT FUNCTIONS ----------------------------------

#' @title Charts showing impact of climate variables on firm outcomes
#'
#' @description
#' The climate_on_firm_regs_charts() function exports charts showing results from the regressions of firm outcomes on climate variables.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param data Regression-ready dataset prepared by 1_prep_reg_data.
#' @param level_or_difference Either "Level" or "Difference"; indicates whether for the given climate variable, its survey year level or the difference of this level from its long-run mean should be used.
#' @param output_directory Folder path for where charts will be saved.
#'
#' @return One chart for heat days, one for temperature, and one for temperature volatility saved in the output_directory.
#'
#' @note Remember to use forward slashes "/" in the folder path provided.
#'
#' @export
climate_on_firm_regs_charts <- function(data, level_or_difference, output_directory){

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Error: Input must be an R data frame.")
  }

  if (!is.character(level_or_difference) || length(level_or_difference) != 1) {
    stop("Error: 'level_or_difference' must be a single string.")
  }

  if (!level_or_difference %in% c("Level", "Difference")) {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  if (!is.character(output_directory) || length(output_directory) != 1) {
    stop("Error: 'output_directory' must be a single string.")
  }

  # Define the climate variable based on the level_or_difference argument
  heat_days_var <- if (level_or_difference == "Level") {
    "HeatDays"
  } else if (level_or_difference == "Difference") {
    "HeatDaysDifference"
  }

  temp_var <- if (level_or_difference == "Level") {
    "Temperature"
  } else if (level_or_difference == "Difference") {
    "TemperatureDifference"
  }

  tempvol_var <- if (level_or_difference == "Level") {
    "TemperatureVolatility"
  } else if (level_or_difference == "Difference") {
    "TemperatureVolatilityDifference"
  }

  # Create the plots
  hd_plot <- reg_chart(data, heat_days_var, paste0("Number of Heat Days (", level_or_difference, ") - Effect on Firm Outcomes"))
  temp_plot <- reg_chart(data, temp_var, paste0("Temperature (", level_or_difference, ") - Effect on Firm Outcomes"))
  tempvol_plot <- reg_chart(data, tempvol_var, paste0("Temperature Volatility (", level_or_difference, ") - Effect on Firm Outcomes"))

  # Ensure the output directory exists
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  # Define file paths for each plot
  hd_plot_path <- file.path(output_directory, paste0("HeatDays_plot_", level_or_difference, ".png"))
  temp_plot_path <- file.path(output_directory, paste0("Temperature_plot_", level_or_difference, ".png"))
  tempvol_plot_path <- file.path(output_directory, paste0("TemperatureVolatility_plot_", level_or_difference, ".png"))

  # Save the plots
  ggsave(hd_plot_path, plot = hd_plot, width = 12, height = 7, bg = "white")
  ggsave(temp_plot_path, plot = temp_plot, width = 12, height = 7, bg = "white")
  ggsave(tempvol_plot_path, plot = tempvol_plot, width = 12, height = 7, bg = "white")
}

#' @title Tables showing impact of climate variables on firm outcomes
#'
#' @description
#' The climate_on_firm_regs_tables() function exports tables showing results from the regressions of firm outcomes on climate variables.
#'
#' @import dplyr
#' @import sandwich
#' @import stargazer
#'
#' @param data Regression-ready dataset prepared by 1_prep_reg_data.
#' @param level_or_difference Either "Level" or "Difference"; indicates whether for the given climate variable, its survey year level or the difference of this level from its long-run mean should be used.
#' @param html_or_tex Either "html" or "tex"; indicates whether the tables will be saved in .html or .tex format.
#' @param output_directory Folder path for where tables will be saved.
#'
#' @return One table for heat days, one for temperature, and one for temperature volatility saved in the output_directory.
#'
#' @note Remember to use forward slashes "/" in the folder path provided.
#'
#' @export
climate_on_firm_regs_tables <- function(data, level_or_difference, html_or_tex, output_directory){

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Error: Input must be an R data frame.")
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

  if (!is.character(output_directory) || length(output_directory) != 1) {
    stop("Error: 'output_directory' must be a single string.")
  }

  # Define the climate variable based on the level_or_difference argument
  heat_days_var <- if (level_or_difference == "Level") {
    "HeatDays"
  } else if (level_or_difference == "Difference") {
    "HeatDaysDifference"
  }

  temp_var <- if (level_or_difference == "Level") {
    "Temperature"
  } else if (level_or_difference == "Difference") {
    "TemperatureDifference"
  }

  tempvol_var <- if (level_or_difference == "Level") {
    "TemperatureVolatility"
  } else if (level_or_difference == "Difference") {
    "TemperatureVolatilityDifference"
  }

  # Define the climate variable based on the level_or_difference argument
  heat_days_var <- if (level_or_difference == "Level") {
    "HeatDays"
  } else if (level_or_difference == "Difference") {
    "HeatDaysDifference"
  } else {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  temp_var <- if (level_or_difference == "Level") {
    "Temperature"
  } else if (level_or_difference == "Difference") {
    "TemperatureDifference"
  } else {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  tempvol_var <- if (level_or_difference == "Level") {
    "TemperatureVolatility"
  } else if (level_or_difference == "Difference") {
    "TemperatureVolatilityDifference"
  } else {
    stop("Error: 'level_or_difference' must be either 'Level' or 'Difference'.")
  }

  # Create the tables
  hd_table <- reg_table(data, heat_days_var, paste0("Number of Heat Days (", level_or_difference, ")"), html_or_tex, paste0(output_directory, "/HeatDays_table_", level_or_difference, ".", html_or_tex))
  temp_table <- reg_table(data, temp_var, paste0("Temperature (", level_or_difference, ")"), html_or_tex, paste0(output_directory, "/Temperature_table_", level_or_difference, ".", html_or_tex))
  tempvol_table <- reg_table(data, tempvol_var, paste0("Temperature Volatility (", level_or_difference, ")"), html_or_tex, paste0(output_directory, "/TemperatureVolatility_table_", level_or_difference, ".", html_or_tex))
}

