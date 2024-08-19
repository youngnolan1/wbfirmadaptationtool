# WB Firm Adaptation Tool - example analysis

# Load package
library(wbfirmadaptation)

# MANDATORY FIRST STEP: Prep data - edit data_directory and survey arguments
reg_data <- wbfirmadaptation::prep_reg_data("path/to/WB/geolinked/master/data", "Survey")


# Climate on firm regressions - edit level_or_deviation and output_directory

# Export summary charts for heat days, temperature, and temperature volatility
wbfirmadaptation::climate_on_firm_regs_charts(reg_data, "level", "path/where/output/saved")

# Export regression tables
wbfirmadaptation::climate_on_firm_regs_tables(reg_data, "level", "path/where/output/saved")

# Climate interacted with firm characteristics regressions - edit firm_outcome, climate_var, firm_characteristics, and output_directory
wbfirmadaptation::interaction_reg_table(reg_data, "log(sales)", "heat_days",
                                        c("young", "small", "obstacle_accesstofinance"),
                                        "path/where/output/saved/table_name.html")

