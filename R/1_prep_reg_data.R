#' @title Process master geo-linked WBES dataset to make it regression-ready.
#'
#' @description
#' The prep_reg_data() function processes the raw master dataset and produces a regression-ready dataset.
#'
#' @import haven
#' @import dplyr
#'
#' @param data_directory A string of the user's directory containing the raw data.
#' @param survey A string structured CountryYear indicating the survey the user wants to analyze.
#'
#' @return The regression-ready dataset as a dataframe (save to object and use as input in other wbfirmadaptation functions).
#'
#' @note "master.dta" and "GPS_Comprehensive_April_03_2024-MASKED.dta" must be saved in project directory, with those exact names.
#'
#' @export
prep_reg_data <- function(data_directory, survey){

  # Read master geo-linked WBES data and subset
  master <- haven::read_dta(paste0(data_directory, "/master.dta")) %>%
    dplyr::filter(country == survey) %>%
    dplyr::mutate(idstd = as.numeric(idstd))

  # Read coords file and subset
  coords <- haven::read_dta(paste0(data_directory, "/GPS_Comprehensive_April_03_2024-MASKED.dta")) %>%
    dplyr::filter(survey == survey) %>%
    dplyr::select(idstd, lat_mask, lon_mask)

  # Merge the two
  master <- dplyr::left_join(master, coords, by = "idstd")

  # Define list of relevant regression vars
  regvars <- c("country", "idstd", "wt", # Identifier and weights
               "d2", "l1", "n2a", "n2b", "f1", "c6", "k4", # Firm performance indicators
               "heat_days", "hd80", "mean_2m_temperature", "mean80", "sd_2m_temperature", "sd80", # Climate variables
               "a2x", "stra_sector", # Region and sector
               "b5", "size", "e1", # Age, size, and market served
               "k30", "j30c", "j30f", "d30b", "l30a", # Business environment variables
               "k6", "k7", "k8", # Banking variables
               "c8", "c9a", "c16", "c17", "c30a", "d30a", "c15", # Infrastructure variables
               "d12a", # Supply chains
               "j4", "j2", "j7b", # Freq meetings tax officials / % time spent complying with regulations / informal payments
               "b7", "b7a"  # Management variables
  )

  # Subset and rename these vars
  reg_master <- master %>%
    dplyr::select(all_of(regvars)) %>%
    rename(sales = d2) %>%
    rename(workers = l1) %>%
    rename(wages = n2a) %>%
    rename(electricity_cost = n2b) %>%
    rename(capital_utilization = f1) %>%
    rename(power_outages = c6) %>%
    rename(invest_fixed_assets_dummy = k4) %>%
    rename(lr_heat_days = hd80) %>%
    rename(temp = mean_2m_temperature) %>%
    rename(lr_temp = mean80) %>%
    rename(tempvolatility = sd_2m_temperature) %>%
    rename(lr_tempvolatility = sd80) %>%
    rename(region = a2x) %>%
    rename(sector = stra_sector) %>%
    rename(year_founded = b5) %>%
    rename(market_served = e1) %>%
    rename(obstacle_accesstofinance = k30) %>%
    rename(obstacle_businesslicensing = j30c) %>%
    rename(obstacle_corruption = j30f) %>%
    rename(obstacle_traderegulations = d30b) %>%
    rename(obstacle_laborregulations = l30a) %>%
    rename(checking_savings_account = k6) %>%
    rename(overdraft_facility = k7) %>%
    rename(financial_inst_credit = k8) %>%
    rename(length_power_outage = c8) %>%
    rename(pcnt_sales_losses_dueto_power_outages = c9a) %>%
    rename(freq_water_shortages = c16) %>%
    rename(length_water_shortages = c17) %>%
    rename(obstacle_electricity = c30a) %>%
    rename(obstacle_transport = d30a) %>%
    rename(insufficient_water_supply = c15) %>%
    rename(inputs_domestic = d12a) %>%
    rename(freq_meetings_tax_officials = j4) %>%
    rename(pcnt_time_dealing_regulations = j2) %>%
    rename(total_annual_informal_payments = j7b) %>%
    rename(manager_experience_yrs = b7) %>%
    rename(top_manager_female = b7a)


  # Define some important variables

  # Sales per worker
  reg_master$sales_per_worker <- reg_master$sales/reg_master$workers

  # Energy intensity
  reg_master$energy_intensity <- reg_master$electricity_cost/reg_master$sales
  reg_master <- reg_master %>%
    mutate(energy_intensity = case_when(
      energy_intensity < 0 ~ NA,
      energy_intensity >= 0 ~ energy_intensity
    ))

  # Heat days deviation
  reg_master$heat_days_deviation <- reg_master$heat_days - reg_master$lr_heat_days

  # Temp deviation
  reg_master$temp_deviation <- reg_master$temp - reg_master$lr_temp

  # Temp volatility deviation
  reg_master$tempvolatility_deviation <- reg_master$tempvolatility - reg_master$lr_tempvolatility

  # Age
  reg_master <- reg_master %>%
    mutate(age = 2022 - year_founded) %>%
    mutate(young = case_when(
      age <= 5 ~ 1,
      age > 5 ~ 0
    ))

  # Size
  reg_master <- reg_master %>%
    mutate(small = case_when(
      size == 1 ~ 1,
      size > 1 ~ 0
    ))

  # Market served
  reg_master <- reg_master %>%
    mutate(market_served = case_when(
      market_served == 1 ~ "Local",
      market_served == 2 ~ "National",
      market_served == 3 ~ "International"
    ))
  reg_master$market_served <- as.factor(reg_master$market_served)

  # Inputs domestic
  reg_master <- reg_master %>%
    mutate(inputs_domestic = case_when(
      inputs_domestic > 50 ~ 1,
      inputs_domestic <= 50 ~ 0
    ))

  # Re-code the binary vars to 0/1
  reg_master <- reg_master %>%
    mutate(overdraft_facility = case_when(
      overdraft_facility == 1 ~ 1,
      overdraft_facility == 2 ~ 0
    )) %>%
    mutate(insufficient_water_supply = case_when(
      insufficient_water_supply == 1 ~ 1,
      insufficient_water_supply == 2 ~ 0
    )) %>%
    mutate(top_manager_female = case_when(
      top_manager_female == 1 ~ 1,
      top_manager_female == 2 ~ 0))

  # Convert obstacle vars to dummies
  obstacle_vars <- grep("^obstacle", names(reg_master), value = TRUE)

  for (var in obstacle_vars) {
    reg_master <- reg_master %>%
      mutate(!!var := case_when(
        .[[var]] == 0 ~ 0,
        .[[var]] == 1 ~ 0,
        .[[var]] == 2 ~ 1,
        .[[var]] == 3 ~ 1,
        .[[var]] == 4 ~ 1
      ))
  }

  # Define NAs properly
  reg_master[reg_master == -9] <- NA
  reg_master[reg_master == -8] <- NA
  reg_master[reg_master == -7] <- NA


  return(reg_master)

}
