Follow these steps to start using the package:
1. Save "master.dta" (WBES geo-linked master dataset) in your project directory.
2. Install the "devtools" R package - run install.packages("devtools")
3. You can now install the WB Firm Adaptation Tool package - run devtools::install_github("youngnolan1/wbfirmadaptationtool")
4. Finally, make sure to load this package each time before using it as you would any other R package - run library(wbfirmadaptationtool)

Please see the example_analysis.R script in the example folder above for an outline of how to use the package's functions. You can download the script to your device and then edit the function inputs (which survey to look at, which variables to include in the regressions, etc) to suit your needs.

Surveys you can analyze:

  [1] "Afghanistan2014"              "Albania2013"                 
  [3] "Albania2019"                  "Argentina2010"               
  [5] "Argentina2017"                "Armenia2013"                 
  [7] "Armenia2020"                  "Austria2021"                 
  [9] "Azerbaijan2013"               "Azerbaijan2019"              
 [11] "Burundi2014"                  "Belgium2020"                 
 [13] "Benin2016"                    "Bangladesh2013"              
 [15] "Bangladesh2022"               "Bulgaria2013"                
 [17] "Bulgaria2019"                 "Bulgaria2023"                
 [19] "Bosnia and Herzegovina2013"   "Bosnia and Herzegovina2019"  
 [21] "Bosnia and Herzegovina2023"   "Belarus2013"                 
 [23] "Belarus2018"                  "Bolivia2010"                 
 [25] "Bolivia2017"                  "Barbados2023"                
 [27] "Bhutan2015"                   "Botswana2023"                
 [29] "Central African Republic2011" "Central African Republic2023"
 [31] "Chile2010"                    "China2012"                   
 [33] "Côte d'Ivoire2016"            "Côte d'Ivoire2023"           
 [35] "Cameroon2016"                 "DRC2013"                     
 [37] "Colombia2010"                 "Colombia2017"                
 [39] "Colombia2023"                 "Costa Rica2010"              
 [41] "Costa Rica2023"               "Cyprus2019"                  
 [43] "Czechia2013"                  "Czechia2019"                 
 [45] "Germany2021"                  "Djibouti2013"                
 [47] "Denmark2020"                  "DominicanRepublic2016"       
 [49] "Ecuador2010"                  "Ecuador2017"                 
 [51] "Egypt2013"                    "Egypt2016"                   
 [53] "Egypt2020"                    "Spain2021"                   
 [55] "Estonia2013"                  "Estonia2019"                 
 [57] "Estonia2023"                  "Ethiopia2011"                
 [59] "Ethiopia2015"                 "Finland2020"                 
 [61] "France2021"                   "Georgia2013"                 
 [63] "Georgia2019"                  "Georgia2023"                 
 [65] "Ghana2013"                    "Ghana2023"                   
 [67] "Guinea2016"                   "Gambia2018"                  
 [69] "Gambia2023"                   "Greece2018"                  
 [71] "Greece2023"                   "Guatemala2010"               
 [73] "Guatemala2017"                "Hong Kong SAR China2023"     
 [75] "Honduras2010"                 "Honduras2016"                
 [77] "Croatia2013"                  "Croatia2019"                 
 [79] "Croatia2023"                  "Hungary2013"                 
 [81] "Hungary2019"                  "Hungary2023"                 
 [83] "Indonesia2015"                "Indonesia2023"               
 [85] "India2014"                    "India2022"                   
 [87] "Ireland2020"                  "Iraq2011"                    
 [89] "Iraq2022"                     "Israel2013"                  
 [91] "Italy2019"                    "Jordan2013"                  
 [93] "Jordan2019"                   "Kazakhstan2013"              
 [95] "Kazakhstan2019"               "Kenya2013"                   
 [97] "Kenya2018"                    "Kyrgyz Republic2013"         
 [99] "Kyrgyz Republic2019"          "Kyrgyz Republic2023"         
[101] "Cambodia2016"                 "Cambodia2023"                
[103] "LaoPDR2016"                   "LaoPDR2018"                  
[105] "Lebanon2013"                  "Lebanon2019"                 
[107] "Liberia2017"                  "Lesotho2016"                 
[109] "Lesotho2023"                  "Lithuania2013"               
[111] "Lithuania2019"                "Luxembourg2020"              
[113] "Latvia2013"                   "Latvia2019"                  
[115] "Morocco2013"                  "Morocco2019"                 
[117] "Morocco2023"                  "Moldova2013"                 
[119] "Moldova2019"                  "Madagascar2013"              
[121] "Madagascar2022"               "Mexico2010"                  
[123] "Mexico2023"                   "North Macedonia2013"         
[125] "North Macedonia2019"          "North Macedonia2023"         
[127] "Mali2016"                     "Malta2019"                   
[129] "Myanmar2014"                  "Myanmar2016"                 
[131] "Montenegro2013"               "Montenegro2019"              
[133] "Montenegro2023"               "Mongolia2013"                
[135] "Mongolia2019"                 "Mozambique2018"              
[137] "Mauritania2014"               "Mauritius2023"               
[139] "Malawi2014"                   "Malaysia2015"                
[141] "Malaysia2019"                 "Namibia2014"                 
[143] "Niger2017"                    "Nigeria2014"                 
[145] "Nicaragua2010"                "Nicaragua2016"               
[147] "Netherlands2020"              "Nepal2013"                   
[149] "Nepal2023"                    "New Zealand2023"             
[151] "Pakistan2013"                 "Pakistan2022"                
[153] "Panama2010"                   "Peru2010"                    
[155] "Peru2017"                     "Peru2023"                    
[157] "Philippines2015"              "Philippines2023"             
[159] "PapuaNewGuinea2015"           "Poland2013"                  
[161] "Poland2019"                   "Portugal2019"                
[163] "Portugal2023"                 "Paraguay2010"                
[165] "Paraguay2017"                 "Paraguay2023"                
[167] "West Bank And Gaza2013"       "West Bank And Gaza2019"      
[169] "West Bank And Gaza2023"       "Romania2013"                 
[171] "Romania2019"                  "Romania2023"                 
[173] "Russia2012"                   "Russia2019"                  
[175] "Rwanda2011"                   "Rwanda2019"                  
[177] "Rwanda2023"                   "Saudi Arabia2022"            
[179] "Sudan2014"                    "Senegal2014"                 
[181] "Singapore2023"                "Solomon Islands2015"         
[183] "Sierra Leone2017"             "Sierra Leone2023"            
[185] "ElSalvador2010"               "ElSalvador2016"              
[187] "ElSalvador2023"               "Serbia2013"                  
[189] "Serbia2019"                   "Southsudan2014"              
[191] "Suriname2018"                 "Slovak Republic2013"         
[193] "Slovak Republic2019"          "Slovenia2013"                
[195] "Slovenia2019"                 "Sweden2014"                  
[197] "Sweden2020"                   "Eswatini2016"                
[199] "Seychelles2023"               "Chad2018"                    
[201] "Chad2023"                     "Togo2016"                    
[203] "Togo2023"                     "Thailand2016"                
[205] "Tajikistan2013"               "Tajikistan2019"              
[207] "Timor-Leste2015"              "Timor-Leste2021"             
[209] "Tunisia2013"                  "Tunisia2020"                 
[211] "Türkiye2013"                  "Türkiye2019"                 
[213] "Tanzania2013"                 "Tanzania2023"                
[215] "Uganda2013"                   "Ukraine2013"                 
[217] "Ukraine2019"                  "Uruguay2010"                 
[219] "Uruguay2017"                  "Uzbekistan2013"              
[221] "Uzbekistan2019"               "Venezuela2010"               
[223] "Viet Nam2015"                 "Viet Nam2023"                
[225] "Samoa2023"                    "Kosovo2013"                  
[227] "Kosovo2019"                   "Yemen2010"                   
[229] "Yemen2013"                    "SouthAfrica2020"             
[231] "Zambia2013"                   "Zambia2019"                  
[233] "Zimbabwe2011"                 "Zimbabwe2016" 
