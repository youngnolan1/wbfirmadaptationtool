#' @title Process subset of the master geo-linked WBES dataset to make it regression-ready.
#'
#' @description
#' The prep_reg_data() function processes the subset of the raw master dataset (produced by data_subset()) and produces a regression-ready dataset.
#'
#' @import dplyr
#'
#' @param subset_data An R dataframe containing the subset of the raw master dataset to be analyzed.
#'
#' @return The regression-ready dataset as a dataframe (save to object and use as input in other wbfirmadaptation functions).
#'
#' @export
prep_reg_data <- function(subset_data){

  # Validate input
  if (!is.data.frame(subset_data)) {
    stop("Error: Input must be an R data frame.")
  }

  # Define list of relevant regression vars
  regvars <- c("country", "countryname", "year", "idstd", "wt", "region", # Identifier and weights
               "d2", "l1", "n2a", "n2b", "f1", "c6", "k4", # Firm performance indicators
               "heat_days", "hd80", "mean_2m_temperature", "mean80", "sd_2m_temperature", "sd80", # Climate variables
               "a2x", "stra_sector", # Region and sector
               "b5", "size", "e1", # Age, size, and market served
               "k30", "j30c", "j30f", "d30b", "l30a", "sec_ind", # Business environment variables
               "k6", "k7", "k8", "acc_fin", # Banking variables
               "c8", "c9a", "c16", "c17", "c30a", "d30a", "c15", "elec", # Infrastructure variables
               "d12a", "exp_ind", # Value chains
               "j4", "j2", "j7b", "reg_ind", # Freq meetings tax officials / % time spent complying with regulations / informal payments
               "b7", "b7a", "ed_ind"  # Management and labor variables
  )

  # Define zeroes and NAs correctly
  subset_data$a14y[subset_data$a14y == 2301] <- 2013
  subset_data$b6b[subset_data$b6b == -8 | subset_data$b6b == -6] <- NA
  subset_data$b5[subset_data$b5 < 1800] <- NA
  subset_data$e1[subset_data$e1 == -9 | subset_data$e1 == -8 | subset_data$e1 == 4] <- NA
  subset_data$n2b[subset_data$n2b < 0] <- NA
  subset_data$n2a[subset_data$n2a < 0] <- NA
  subset_data$l1[subset_data$l1 < 0] <- NA
  subset_data$n5a[subset_data$n5a < 0] <- NA
  subset_data$n5b[subset_data$n5b < 0] <- NA
  subset_data$d2[subset_data$d2 < 0] <- NA
  subset_data$b6b[subset_data$b6b == -8 | subset_data$b6b == -6] <- NA
  subset_data$i1[subset_data$i1 < 0 | subset_data$i1 == 5] <- NA
  subset_data$i1[subset_data$i1 == 2] <- 0
  subset_data$i3[subset_data$i3 < 0] <- NA
  subset_data$i30[subset_data$i30 == -7] <- 0
  subset_data$i30[subset_data$i30 < 0] <- NA
  subset_data$d3a[subset_data$d3a < 0] <- NA
  subset_data$d3b[subset_data$d3b < 0] <- NA
  subset_data$d3c[subset_data$d3c < 0] <- NA
  subset_data$d30b[subset_data$d30b == -7] <- 0
  subset_data$d30b[subset_data$d30b < 0] <- NA
  subset_data$j10[subset_data$j10 < 0] <- NA
  subset_data$k30[subset_data$k30 == -7] <- 0
  subset_data$k30[subset_data$k30 < 0] <- NA
  subset_data$k3a[subset_data$k3a < 0] <- NA
  subset_data$k3hd[subset_data$k3hd < 0] <- NA
  subset_data$k4[subset_data$k4 < 0] <- NA
  subset_data$k4[subset_data$k4 == 2] <- 0
  subset_data$k6[subset_data$k6 < 0] <- NA
  subset_data$k6[subset_data$k6 == 2] <- 0
  subset_data$k8[subset_data$k8 < 0] <- NA
  subset_data$k8[subset_data$k8 == 2] <- 0
  subset_data$c6[subset_data$c6 < 0] <- NA
  subset_data$c6[subset_data$c6 == 2] <- 0
  subset_data$c30a[subset_data$c30a == -7] <- 0
  subset_data$c30a[subset_data$c30a < 0] <- NA
  subset_data$j3[subset_data$j3 < 0] <- NA
  subset_data$h30[subset_data$h30 == -7] <- 0
  subset_data$h30[subset_data$h30 < 0] <- NA
  subset_data$b7[subset_data$b7 < 0] <- NA
  subset_data$l10[subset_data$l10 < 0] <- NA
  subset_data$l10[subset_data$l10 == 2] <- 0
  subset_data$l30b[subset_data$l30b == -7] <- 0
  subset_data$l30b[subset_data$l30b < 0] <- NA
  subset_data$b1[subset_data$b1 < 0 | subset_data$b1 == 7] <- NA
  subset_data$b2a[subset_data$b2a < 0] <- NA
  subset_data$b2b[subset_data$b2b < 0] <- NA
  subset_data$b2c[subset_data$b2c < 0] <- NA
  subset_data$b2d[subset_data$b2d < 0] <- NA
  subset_data$b3[subset_data$b3 < 0] <- NA
  subset_data$b4[subset_data$b4 < 0] <- NA
  subset_data$b4[subset_data$b4 == 2] <- 0
  subset_data$b4a[subset_data$b4a < 0] <- NA
  subset_data$b7a[subset_data$b7a < 0] <- NA
  subset_data$b7a[subset_data$b7a == 2] <- 0
  subset_data$b8[subset_data$b8 < 0] <- NA
  subset_data$b8[subset_data$b8 == 2] <- 0
  subset_data$c10[subset_data$c10 < 0] <- NA
  subset_data$c10[subset_data$c10 == 2] <- 0
  subset_data$c9a[subset_data$c9a < 0] <- NA
  subset_data$c22b[subset_data$c22b < 0] <- NA
  subset_data$c22b[subset_data$c22b == 2] <- 0
  subset_data$d1a3[subset_data$d1a3 < 0 | subset_data$d1a3 == 5050] <- NA
  subset_data$d6[subset_data$d6 == -7] <- 0
  subset_data$d6[subset_data$d6 < 0] <- NA
  subset_data$d7[subset_data$d7 == -7] <- 0
  subset_data$d7[subset_data$d7 < 0] <- NA
  subset_data$d10[subset_data$d10 < -7] <- NA
  subset_data$d10[subset_data$d10 == -7] <- 0
  subset_data$d11[subset_data$d11 == -7] <- 0
  subset_data$d11[subset_data$d11 < 0] <- NA
  subset_data$d12a[subset_data$d12a < 0] <- NA
  subset_data$d12b[subset_data$d12b < 0] <- NA
  subset_data$d16[subset_data$d16 < 0] <- NA
  subset_data$d30a[subset_data$d30a == -7] <- 0
  subset_data$d30a[subset_data$d30a < 0 | subset_data$d30a == 5] <- NA
  subset_data$e11[subset_data$e11 < 0] <- NA
  subset_data$e30[subset_data$e30 == -7] <- 0
  subset_data$e30[subset_data$e30 < 0] <- NA
  subset_data$f1[subset_data$f1 < 0] <- NA
  subset_data$g30a[subset_data$g30a == -7] <- 0
  subset_data$g30a[subset_data$g30a < 0] <- NA
  subset_data$i2a[subset_data$i2a < 0] <- NA
  subset_data$i3[subset_data$i3 < 0] <- NA
  subset_data$i3[subset_data$i3 == 2] <- 0
  subset_data$i4a[subset_data$i4a < 0] <- NA
  subset_data$i30[subset_data$i30 == -7] <- 0
  subset_data$i30[subset_data$i30 < 0] <- NA
  subset_data$j2[subset_data$j2 < 0] <- NA
  subset_data$j3[subset_data$j3 < 0] <- NA
  subset_data$j3[subset_data$j3 == 2] <- 0
  subset_data$j6[subset_data$j6 < 0] <- NA
  subset_data$j6a[subset_data$j6a < 0 | subset_data$j6a == 79] <- NA
  subset_data$j6a[subset_data$j6a == 2] <- 0
  subset_data$j7a[subset_data$j7a < 0] <- NA
  subset_data$j30a[subset_data$j30a == -7] <- 0
  subset_data$j30a[subset_data$j30a < 0] <- NA
  subset_data$j30b[subset_data$j30b == -7] <- 0
  subset_data$j30b[subset_data$j30b < 0] <- NA
  subset_data$j30c[subset_data$j30c == -7] <- 0
  subset_data$j30c[subset_data$j30c < 0] <- NA
  subset_data$j30e[subset_data$j30e == -7] <- 0
  subset_data$j30e[subset_data$j30e < 0] <- NA
  subset_data$j30f[subset_data$j30f == -7] <- 0
  subset_data$j30f[subset_data$j30f < 0] <- NA
  subset_data$k1c[subset_data$k1c < 0] <- NA
  subset_data$k3a[subset_data$k3a < 0] <- NA
  subset_data$k3bc[subset_data$k3bc < 0] <- NA
  subset_data$k3e[subset_data$k3e < 0] <- NA
  subset_data$k3f[subset_data$k3f < 0] <- NA
  subset_data$k3hd[subset_data$k3hd < 0] <- NA
  subset_data$k16[subset_data$k16 < 0] <- NA
  subset_data$k16[subset_data$k16 == 2] <- 0
  subset_data$k21[subset_data$k21 < 0] <- NA
  subset_data$k21[subset_data$k21 == 2] <- 0
  subset_data$l1[subset_data$l1 < 0] <- NA
  subset_data$l2[subset_data$l2 < 0] <- NA
  subset_data$l30a[subset_data$l30a == -7] <- 0
  subset_data$l30a[subset_data$l30a < 0] <- NA
  subset_data$l9a[subset_data$l9a < 0 | subset_data$l9a > 100] <- NA
  subset_data$l9b[subset_data$l9b < 0] <- NA
  subset_data$l12[subset_data$l12 < 0] <- NA


  # Subset and rename these vars
  reg_master <- subset_data %>%
    dplyr::select(all_of(regvars)) %>%
    rename(Survey = country) %>%
    rename(Country = countryname) %>%
    rename(WorldRegion = region) %>%
    rename(SubNationalRegion = a2x) %>%
    rename(Sector = stra_sector) %>%
    rename(Year = year) %>%
    rename(Sales = d2) %>%
    rename(Workers = l1) %>%
    rename(Wages = n2a) %>%
    rename(ElectricityCost = n2b) %>%
    rename(CapitalUtilization = f1) %>%
    rename(PowerOutagesBinary = c6) %>%
    rename(InvestmentBinary = k4) %>%
    rename(HeatDays = heat_days) %>%
    rename(LongRunHeatDays = hd80) %>%
    rename(Temperature = mean_2m_temperature) %>%
    rename(LongRunTemperature = mean80) %>%
    rename(TemperatureVolatility = sd_2m_temperature) %>%
    rename(LongRunTemperatureVolatility = sd80) %>%
    rename(YearFounded = b5) %>%
    rename(Size = size) %>%
    rename(MarketServed = e1) %>%
    rename(AccessToFinance = k30) %>%
    rename(BusinessLicensing = j30c) %>%
    rename(Corruption = j30f) %>%
    rename(TradeRegulations = d30b) %>%
    rename(LaborRegulations = l30a) %>%
    rename(CheckingOrSavingsAccount = k6) %>%
    rename(OverdraftFacility = k7) %>%
    rename(CreditFromFinancialInstitution = k8) %>%
    rename(PowerOutagesLength = c8) %>%
    rename(PowerOutagesSalesLosses = c9a) %>%
    rename(WaterShortagesFrequency = c16) %>%
    rename(WaterShortagesLength = c17) %>%
    rename(ElectricityObstacle = c30a) %>%
    rename(Transport = d30a) %>%
    rename(InsufficientWaterSupply = c15) %>%
    rename(DomesticInputs = d12a) %>%
    rename(TaxOfficialsMeetings = j4) %>%
    rename(RegulationsTimeSpent = j2) %>%
    rename(InformalPayments = j7b) %>%
    rename(ManagerialExperience = b7) %>%
    rename(FemaleManager = b7a) %>%
    rename(ElectricityIndex = elec) %>%
    rename(AccessToFinanceIndex = acc_fin) %>%
    rename(ExportOrientationIndex = exp_ind) %>%
    rename(SecurityIndex = sec_ind) %>%
    rename(RegulationIndex = reg_ind) %>%
    rename(LaborForceIndex = ed_ind)


  # Define some new variables for regression analysis

  # Heat days difference
  reg_master$HeatDaysDifference <- reg_master$HeatDays - reg_master$LongRunHeatDays

  # Temp Difference
  reg_master$TemperatureDifference <- reg_master$Temperature - reg_master$LongRunTemperature

  # Temp volatility Difference
  reg_master$TemperatureVolatilityDifference <- reg_master$TemperatureVolatility - reg_master$LongRunTemperatureVolatility

  # Sales per worker
  reg_master$SalesPerWorker <- reg_master$Sales/reg_master$Workers

  # Energy intensity
  reg_master$EnergyIntensity <- reg_master$ElectricityCost/reg_master$Sales
  reg_master <- reg_master %>%
    mutate(EnergyIntensity = case_when(
      EnergyIntensity < 0 ~ NA,
      EnergyIntensity >= 0 ~ EnergyIntensity
    ))

  # Age
  reg_master <- reg_master %>%
    mutate(Age = Year - YearFounded) %>%
    mutate(Young = case_when(
      Age <= 5 ~ 1,
      Age > 5 ~ 0
    ))

  # Size
  reg_master <- reg_master %>%
    mutate(Small = case_when(
      Size == 1 ~ 1,
      Size > 1 ~ 0
    ))

  # Market served
  reg_master <- reg_master %>%
    mutate(MarketServed = case_when(
      MarketServed == 1 ~ "Local",
      MarketServed == 2 ~ "National",
      MarketServed == 3 ~ "International"
    ))
  reg_master$MarketServed <- as.factor(reg_master$MarketServed)

  # Inputs domestic
  reg_master <- reg_master %>%
    mutate(DomesticInputs = case_when(
      DomesticInputs > 50 ~ 1,
      DomesticInputs <= 50 ~ 0
    ))

  # Re-code the binary vars to 0/1
  reg_master <- reg_master %>%
    mutate(OverdraftFacility = case_when(
      OverdraftFacility == 1 ~ 1,
      OverdraftFacility == 2 ~ 0
    )) %>%
    mutate(InsufficientWaterSupply = case_when(
      InsufficientWaterSupply == 1 ~ 1,
      InsufficientWaterSupply == 2 ~ 0
    ))

  # Convert obstacle vars to dummies
  obstacle_vars <- c("AccessToFinance", "Corruption", "Transport", "BusinessLicensing",
                     "ElectricityObstacle", "TradeRegulations", "LaborRegulations")

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

  # Define logged dependent vars
  reg_master <- reg_master %>%
    mutate(Sales = case_when(Sales == 0 ~ NA,
                             Sales != 0 ~ Sales)) %>%
    mutate(SalesLog = log(Sales)) %>%
    mutate(SalesPerWorker = case_when(SalesPerWorker == 0 ~ NA,
                                      SalesPerWorker != 0 ~ SalesPerWorker)) %>%
    mutate(SalesPerWorkerLog = log(SalesPerWorker)) %>%
    mutate(Workers = case_when(Workers == 0 ~ NA,
                               Workers != 0 ~ Workers)) %>%
    mutate(WorkersLog = log(Workers)) %>%
    mutate(Wages = case_when(Wages == 0 ~ NA,
                             Wages != 0 ~ Wages)) %>%
    mutate(WagesLog = log(Wages)) %>%
    mutate(CapitalUtilization = case_when(CapitalUtilization == 0 ~ NA,
                                          CapitalUtilization != 0 ~ CapitalUtilization)) %>%
    mutate(CapitalUtilizationLog = log(CapitalUtilization)) %>%
    mutate(EnergyIntensity = case_when(EnergyIntensity == 0 ~ NA,
                                       EnergyIntensity != 0 ~ EnergyIntensity)) %>%
    mutate(EnergyIntensityLog = log(EnergyIntensity))

  # Standardize continuous vars - climate and a few firm characteristics
  climate_vars_standard <- c("HeatDays", "HeatDaysDifference", "Temperature", "TemperatureDifference",
                     "TemperatureVolatility", "TemperatureVolatilityDifference")

  firm_vars_standard <- c("PowerOutagesLength", "PowerOutagesSalesLosses", "WaterShortagesFrequency",
                     "WaterShortagesLength", "TaxOfficialsMeetings", "RegulationsTimeSpent",
                     "InformalPayments", "ManagerialExperience")

  reg_master[climate_vars_standard] <- lapply(reg_master[climate_vars_standard], function(x) x / sd(x, na.rm = TRUE))

  reg_master[firm_vars_standard] <- lapply(reg_master[firm_vars_standard], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))

  # Create grid_ids for unique values of geo-codes
  reg_master <- reg_master %>%
    mutate(GridID = dense_rank(TemperatureDifference))

  return(reg_master)

}
