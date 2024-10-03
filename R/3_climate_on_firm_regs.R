# Internal regression function
reg_model <- function(reg_data, outcome, indep_climate_var) {

  # Filter for complete cases in the outcome variable
  reg_data_final <- reg_data[complete.cases(reg_data[[outcome]]), ]

  # Initialize the base formula with the climate variable
  formula_components <- c(indep_climate_var)

  # Conditionally add sector if there is more than one unique value
  if(length(unique(reg_data_final$sector)) > 1) {
    formula_components <- c(formula_components, "sector")
  }

  # Conditionally add countryname if there is more than one unique value
  if(length(unique(reg_data_final$countryname)) > 1) {
    formula_components <- c(formula_components, "countryname")
  }

  # Conditionally add year if there is more than one unique value
  if(length(unique(reg_data_final$year)) > 1) {
    formula_components <- c(formula_components, "year")
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
  firm_outcomes <- c("sales", "sales_per_worker", "wages", "workers",
                    "capital_utilization", "energy_intensity",
                    "power_outages", "invest_fixed_assets_dummy")

  # Empty dataframe for results
  graph_data <- data.frame(
    firm_outcome = character(),
    coefficient = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric()
  )

  for (outcome in firm_outcomes) {

    model <- reg_model(reg_data, outcome, indep_climate_var)

    # Extract coefficient and CI
    results <- data.frame(firm_outcome = outcome,
                          coefficient = coefficients(model)[indep_climate_var],
                          lower_ci = confint(model)[indep_climate_var, 1],
                          upper_ci = confint(model)[indep_climate_var, 2])

    graph_data <- rbind(graph_data, results)

  }

  plot <- ggplot(graph_data, aes(x = firm_outcome, y = coefficient)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45),
          axis.title.y = element_blank(),
          plot.title = element_blank())

  return(plot)
}


# Internal results table function
reg_table <- function(reg_data, indep_climate_var, table_title, directory){

  # Sales (log(d2))
  s <- reg_model(reg_data, "sales_log", indep_climate_var)
  s_se <- sqrt(diag(vcovHC(s, type = "HC1", cluster = ~reg_data$grid_id)))

  # Productivity (log(d2/l1))
  spw <- reg_model(reg_data, "sales_per_worker_log", indep_climate_var)
  spw_se <- sqrt(diag(vcovHC(s, type = "HC1", cluster = ~reg_data$grid_id)))

  # Wages (log(n2a))
  w <- reg_model(reg_data, "wages_log", indep_climate_var)
  w_se <- sqrt(diag(vcovHC(w, type = "HC1", cluster = ~reg_data$grid_id)))

  # Employment (log(l1))
  e <- reg_model(reg_data, "workers_log", indep_climate_var)
  e_se <- sqrt(diag(vcovHC(e, type = "HC1", cluster = ~reg_data$grid_id)))

  # Capital utilization (f1)
  capu <- reg_model(reg_data, "capital_utilization_log", indep_climate_var)
  capu_se <- sqrt(diag(vcovHC(capu, type = "HC1", cluster = ~reg_data$grid_id)))

  # Power outages (c6)
  pwr <- reg_model(reg_data, "power_outages", indep_climate_var)
  pwr_se <- sqrt(diag(vcovHC(pwr, type = "HC1", cluster = ~reg_data$grid_id)))

  # Energy intensity (n2b / d2)
  ei <- reg_model(reg_data, "energy_intensity", indep_climate_var)
  ei_se <- sqrt(diag(vcovHC(ei, type = "HC1", cluster = ~reg_data$grid_id)))

  # Investment in fixed assets (k4)
  inv <- reg_model(reg_data, "invest_fixed_assets_dummy", indep_climate_var)
  inv_se <- sqrt(diag(vcovHC(inv, type = "HC1", cluster = ~reg_data$grid_id)))

  # Generate the regression results table
  sink(tempfile())
  stargazer(s, spw, w, e, capu, pwr, ei, inv,
            type = "html",
            se = c(list(s_se), list(spw_se), list(w_se), list(e_se), list(capu_se), list(pwr_se), list(ei_se), list(inv_se)),
            out = directory,
            title = paste0("Effect of ", table_title, " on Firm Performance"),
            dep.var.labels = c("Sales (log)", "Productivity (log)", "Wages (log)",
                               "Employment (log)", "Capital Utilization (log)",
                               "Power Outages", "Energy Intensity (log)", "Investment in Fixed Assets"),
            omit.stat = c("f", "ser"),
            align = TRUE,
            no.space = TRUE)
  sink()
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
#' @param level_or_deviation Either "level" or "deviation"; indicates whether for the given climate variable, its survey year level or the deviation of this level from its long-run mean should be used.
#' @param output_directory Folder path for where charts will be saved.
#'
#' @return One chart for heat days, one for temperature, and one for temperature volatility saved in the output_directory.
#'
#' @note Remember to use forward slashes "/" in the folder path provided.
#'
#' @export
climate_on_firm_regs_charts <- function(data, level_or_deviation, output_directory){

  # Define the climate variable based on the level_or_deviation argument
  heat_days_var <- ifelse(level_or_deviation == "deviation", "heat_days_deviation", "heat_days")
  temp_var <- ifelse(level_or_deviation == "deviation", "temp_deviation", "temp")
  tempvol_var <- ifelse(level_or_deviation == "deviation", "tempvolatility_deviation", "tempvolatility")

  # Create the plots
  hd_plot <- reg_chart(data, heat_days_var, paste0("Number of Heat Days (", level_or_deviation, ")"))
  temp_plot <- reg_chart(data, temp_var, paste0("Temperature (", level_or_deviation, ")"))
  tempvol_plot <- reg_chart(data, tempvol_var, paste0("Temperature Volatility (", level_or_deviation, ")"))

  # Ensure the output directory exists
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  # Define file paths for each plot
  hd_plot_path <- file.path(output_directory, paste0("heat_days_plot_", level_or_deviation, ".png"))
  temp_plot_path <- file.path(output_directory, paste0("temperature_plot_", level_or_deviation, ".png"))
  tempvol_plot_path <- file.path(output_directory, paste0("temperature_volatility_plot_", level_or_deviation, ".png"))

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
#' @param level_or_deviation Either "level" or "deviation"; indicates whether for the given climate variable, its survey year level or the deviation of this level from its long-run mean should be used.
#' @param output_directory Folder path for where tables will be saved.
#'
#' @return One table for heat days, one for temperature, and one for temperature volatility saved in the output_directory.
#'
#' @note Remember to use forward slashes "/" in the folder path provided.
#'
#' @export
climate_on_firm_regs_tables <- function(data, level_or_deviation, output_directory){

  # Define the climate variable based on the level_or_deviation argument
  heat_days_var <- ifelse(level_or_deviation == "deviation", "heat_days_deviation", "heat_days")
  temp_var <- ifelse(level_or_deviation == "deviation", "temp_deviation", "temp")
  tempvol_var <- ifelse(level_or_deviation == "deviation", "tempvolatility_deviation", "tempvolatility")

  # Create the tables
  hd_table <- reg_table(data, heat_days_var, paste0("Number of Heat Days (", level_or_deviation, ")"), paste0(output_directory, "/heat_days_table_", level_or_deviation, ".html"))
  temp_table <- reg_table(data, temp_var, paste0("Temperature (", level_or_deviation, ")"), paste0(output_directory, "/temp_table_", level_or_deviation, ".html"))
  tempvol_table <- reg_table(data, tempvol_var, paste0("Temperature Volatility (", level_or_deviation, ")"), paste0(output_directory, "/tempvol_table_", level_or_deviation, ".html"))

}

