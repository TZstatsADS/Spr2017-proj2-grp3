# Project: NYC Open Data
### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.


#### Table advertising_year_data
This table contains spending on tobacco advertising from 1970 to 2014.
Source: [Federal Trade Commission Cigarette Report for 2014](https://www.ftc.gov/system/files/documents/reports/federal-trade-commission-cigarette-report-2014-federal-trade-commission-smokeless-tobacco-report/ftc_cigarette_report_2014.pdf)
- spendings are splitted per media.  
- spendings are in thousands of dollars.   
- N/A means that the expenditures for this category are included in the “Other” category to avoid potential disclosure of individual company data.  

#### CDI (U.S. Chronic Disease Indicators)
CDC's Division of Population Health provides cross-cutting set of 124 indicators that were developed by consensus.
Source: [U.S. Chronic Disease Indicators](https://catalog.data.gov/dataset/u-s-chronic-disease-indicators-cdi-e50c9)

#### Population
This table contains U.S. population by states from 2010 to 2015
Source: [U.S. Population](http://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/totals/)
