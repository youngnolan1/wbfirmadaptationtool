library(dplyr)
library(haven)
library(sandwich)
library(ggplot2)
library(stargazer)

# Script 1
mexico_survey <- data_subset("C:/Users/young/OneDrive/Escritorio/wbfirmadaptation_snapshots/master.dta",
                             "Mexico2023",
                             "survey")

mexico_country <- data_subset("C:/Users/young/OneDrive/Escritorio/wbfirmadaptation_snapshots/master.dta",
                             "Mexico",
                             "country")

mena_region <- data_subset("C:/Users/young/OneDrive/Escritorio/wbfirmadaptation_snapshots/master.dta",
                             "MEA",
                             "region")

# Script 2
mexico_survey <- prep_reg_data(mexico_survey)

mexico_country <- prep_reg_data(mexico_country)

mena_region <- prep_reg_data(mena_region)

# Script 3
climate_on_firm_regs_charts(mexico_survey, "deviation", "C:/Users/young/OneDrive/Documents/test")
climate_on_firm_regs_charts(mexico_country, "deviation", "C:/Users/young/OneDrive/Documents/test")
climate_on_firm_regs_charts(mena_region, "deviation", "C:/Users/young/OneDrive/Documents/test")

climate_on_firm_regs_tables(mexico_survey, "deviation", "html", "C:/Users/young/OneDrive/Documents/test")
climate_on_firm_regs_tables(mexico_country, "deviation", "html", "C:/Users/young/OneDrive/Documents/test")
climate_on_firm_regs_tables(mena_region, "level", "html", "C:/Users/young/OneDrive/Documents/test")

# Script 4
interaction_reg_table(mexico_survey, "SalesLog", "HeatDays", "Deviation", c("Young", "Small"), "html",
                      "TestTable", "C:/Users/young/OneDrive/Documents/test")
