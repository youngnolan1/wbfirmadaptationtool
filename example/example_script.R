# WB Firm Adaptation Tool - example analysis

# Load package
library(wbfirmadaptation)

# Prep data - edit data_directory and survey arguments
reg_data <- wbfirmadaptation::prep_reg_data("path/to/WB/geolinked/master/data", "Survey")

# Climate on firm - edit level_or_dviation and output_directory
wbfirmadaptation::climate_on_firm_regs_charts(reg_data, "level", "path/where/output/saved")
wbfirmadaptation::climate_on_firm_regs_tables(reg_data, "level", "path/where/output/saved")

# Interactions - edit firm_outcome, climate_var, firm_characteristics, and output_directory
wbfirmadaptation::interaction_reg_table(reg_data, "log(sales)", "heat_days",
                                        c("young", "small", "obstacle_accesstofinance"),
                                        "path/where/output/saved/table_name.html")

