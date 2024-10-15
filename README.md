The R package wbfirmadaptationtool automates the regression section of the WB Investment Climate Unit’s “Firms and Climate Adaptation Policy Toolkit”. Using the WBES geo-linked master dataset, the user can select any survey (e.g. Mexico2023), country (e.g. Mexico) or region (e.g. LAC) and the package will prepare the data for analysis, perform regressions looking at the impact of climate on firm performance directly, and run regressions featuring interactions with whichever firm characteristics/policy variables are supplied. It automatically exports results tables and charts for the regressions the user decides to run. 

Follow these steps to start using the package:
  1. Save the WBES geo-linked master dataset (in .dta format) in your project directory
  2. Install the "devtools" R package - run install.packages("devtools")
  3. Install the WB Firm Adaptation Tool package - run devtools::install_github("youngnolan1/wbfirmadaptationtool")
  4. Finally, make sure to load this package each time before using it as you would any other R package - run library(wbfirmadaptationtool) 

Please see the example.R script in the example folder above for an outline of how to use the package's functions. You can download the script to your device and then edit the function inputs (which survey to look at, which variables to include in the regressions, etc) to suit your needs.

This guidance note contains more information on the data, coverage, functions, variables which can be used, and empirical specifications: LINK TO FINALIZED GUIDANCE NOTE PDF HERE
