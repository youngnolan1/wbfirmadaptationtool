# Internal interaction regression function
interaction_reg <- function(reg_data, firm_outcome, climate_var, firm_characteristic){

  # Build the formula: log(sales) = climate_var + firm_characteristic + climate_var:firm_characteristic + sector + region
  formula <- as.formula(paste(firm_outcome, " ~ ", climate_var, " + ", firm_characteristic, " + ", climate_var, ":", firm_characteristic, " + sector + region"))

  # Run the regression
  model <- lm(formula,
              data = reg_data)

  return(model)
}

# -------------------------- EXPORT FUNCTIONS ----------------------------------

#' @title Table showing impact of the interaction between firm characteristics and climate variables on firm outcomes.
#'
#' @description
#' Exports a regression result table featuring interactions between the firm characteristics and climate variables provided.
#'
#' @import stargazer
#'
#' @param data Regression-ready dataset prepared by 1_prep_reg_data.
#' @param firm_outcome Dependent variable passed as a string, options include: "log(sales)", "log(sales_per_worker)", "log(wages)", "log(workers)", "log(capital_utilization)", "log(energy_intensity)", "power_outages", "invest_fixed_asset_dummy".
#' @param climate_var Independent climate variable passed as a string, options include: \cr
#' Levels - "heat_days", "temp", "tempvolatility" \cr
#' Deviations - "heat_days_deviation", "temp_deviation", "tempvolatility_deviation" \cr
#' @param firm_characteristics Independent firm characteristic/policy variable passed as a string or list of strings, options include: \cr
#' Basic characteristics: "region", "young", "small", "market_served" \cr
#' Business environment: "obstacle_accesstofinance", "obstacle_businesslicensing", "obstacle_corruption", "obstacle_traderegulations", "obstacle_laborregulations", "freq_meetings_tax_officials", "pcnt_time_dealing_regulations", "total_annual_informal_payments" \cr
#' Banking: "checking_savings_account", "overdraft_facility", "financial_inst_credit" \cr
#' Infrastructure: "length_power_outage", "pcnt_sales_losses_dueto_power_outages", "freq_water_shortages", "length_water_shortages", "obstacle_electricity", "obstacle_transport", "insufficient_water_supply" \cr
#' Supply chains: "inputs_domestic" \cr
#' Management: "manager_experience_yrs", "top_manager_female" \cr
#' @param output_directory File path for where table will be saved, ending in .html e.g. "C:/Users/Nolan/Project/table.html".
#'
#' @return HTML file containing a regression results table for the variables provided.
#'
#' @note Remember to use forward slashes "/" in the folder path.
#'
#' @export
interaction_reg_table <- function(reg_data, firm_outcome, climate_var, firm_characteristics, output_directory){

  # Initialize an empty list to store the models
  models <- list()

  # Loop through each firm characteristic in the list
  for (characteristic in firm_characteristics) {

    model <- interaction_reg(reg_data, firm_outcome, climate_var, characteristic)

    models[[characteristic]] <- model
  }

  # Use stargazer to combine all models into a results table
  sink(tempfile())
  stargazer(models, type = "text", title = paste0("Regression Results - Interactions (", reg_data$country, ")"), out = output_directory)
  sink()
}

