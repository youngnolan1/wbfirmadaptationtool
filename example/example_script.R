# WB Firm Adaptation Tool - example analysis

# Load package
library(wbfirmadaptationtool)

# Subset data and prep it for regressions
mexico23_data <- subset_data("path/to/master/dataset/datafilename.dta", # Path where WBES geo-linked master .dta file is saved
                    "Mexico2023", # Survey, country, or region you want to analyze
                    "survey") # "survey", "country", or "region" depending on your case

mexico23_reg_data <- prep_reg_data(mexico23_data)


# Climate on firm regressions

# Export summary charts for heat days, temperature, and temperature volatility
wbfirmadaptationtool::climate_on_firm_regs_charts(mexico23_reg_data, # Output of prep_reg_data() function
                                                  "Level", # Defining climate variable as "Level" or "Difference"
                                                  "path/where/output/saved") # Path where the chart will be saved

# Export regression tables
wbfirmadaptationtool::climate_on_firm_regs_tables(mexico23_reg_data, # Output of prep_reg_data() function
                                                  "Level", # Defining climate variable as "Level" or "Difference"
                                                  "html", # Table should be saved in "html" or "tex" format
                                                  "path/where/output/saved") # Path where the table will be saved


# Climate interacted with firm characteristics regressions
wbfirmadaptationtool::interaction_reg_table(mexico23_reg_data, # Output of prep_reg_data() function
                                            "SalesLog", # Firm performance dependent variable - see guidance note for full list of options
                                            "Temperature", # Climate variable - "Temperature", "TemperatureVolatility", or "HeatDays"
                                            "Difference",  # Defining climate variable as "Level" or "Difference"
                                            c("Young", "Small"), # Firm characteristics to interact with - see guidance note for full list of options
                                            "html", # Table should be saved in "html" or "tex" format
                                            "TableFileName", # File name used to save the table
                                            "path/where/output/saved") # Path where the table will be saved

# Exhaustive analysis - do all of the above (plus more interactions) in just one command
wbfirmadaptationtool::exhaustive_analysis("path/to/master/dataset/datafilename.dta", # Path where WBES geo-linked master .dta file is saved
                                          "Mexico2023",# Survey, country, or region you want to analyze
                                          "survey", # "survey", "country", or "region" depending on your case
                                          "html", # Tables should be saved in "html" or "tex" format
                                          "path/where/output/saved") # Path where the charts and tables will be saved
