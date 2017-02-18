# Project: NYC Open Data
### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.


#### Table advertising_year_data
This table contains spending on tobacco advertising from 1970 to 2014.  
- spendings are splitted per media.  
- spendings are in thousands of dollars.   
- N/A means that the expenditures for this category are included in the “Other” category to avoid potential disclosure of individual company data.  
Source: [Federal Trade Commission Cigarette Report for 2014](https://www.ftc.gov/system/files/documents/reports/federal-trade-commission-cigarette-report-2014-federal-trade-commission-smokeless-tobacco-report/ftc_cigarette_report_2014.pdf)
