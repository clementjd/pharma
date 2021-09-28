#------------------------------------------------------------------------------------------
#
#  Script Name: Bloomberg RX Data Import and Wrangle
#
#  Author:    Jeff Clement
#             cleme514@umn.edu
#  
#  Update:    20 Aug 2020
#
#  Purpose:   This script builds a couple functions to take the wide BI PHRM RX data from 
#             Bloomberg (which is provided by Symphony Integrated Dataverse) and output a 
#             long, tidy dataframe.
#
#  License:   CC-BY 4.0
#
#------------------------------------------------------------------------------------------
#
#  Usage:     Run this script.  It will load functions called "Import_Bloomberg_TRX" and
#             "WriteCSV_Bloomberg_TRX"
#             
#
#             -----Import_Bloomberg_TRX-----
#
#             Input:  The filename to an excel file containing the TRX output for a molecule
#                     from Bloomberg
#                     
#                     These files normally look like this:
#       
#                         DRUG_NAME                      44012    43982     43951
#                         MYDAYIS                        20373    20740     21536
#                         ADDERALL                        5930     5777      5895
#                         ADDERALL XR                   387068   383811    401516
#                         DEXTROAMPHETAMINE-AMPHET ER   800199   793704    812330
#                         ...                              ...      ...       ...
#                         DEXTROAMPHETAMINE-AMPHETAMI  1792654  1755148   1783921
#
#                     The first column contains the drug names within the category.
#                     The *top row* contains dates (usually monthly) in Excel's numeric
#                       date format. ("44012" is 30 June 2020)
#                     The fields contain the number of RX dispensed for each drug/month.
#
#             Output: A long "tidy" tibble (dataframe) with three variables:
#                         Drug (character) :       The name of the drug
#                         MonthYear (date) :       The month-year w/ last day of month
#                         TRX (numeric)    :       The number of scripts dispensed
#
#             Vignette: Take a file ("adderall.xlsx") in the working directory and load/
#                       store it as "dataframe"
#
#                       >   df <- Import_Bloomberg_TRX("adderall.xlsx")
#
#                       Data Preview:                                                               #                
#                       # A tibble: 6 x 3
#                       DRUG_NAME MonthYear    TRX
#                       <chr>     <date>     <dbl>
#                       1 MYDAYIS   2020-06-30 20737
#                       2 MYDAYIS   2020-05-31 20740
#                       3 MYDAYIS   2020-04-30 21536
#                       4 MYDAYIS   2020-03-31 23455
#                       5 MYDAYIS   2020-02-29 22205
#                       6 MYDAYIS   2020-01-31 23229
#                       
#                       
#                       Drugs imported: ADDERALL, ADDERALL XR, DEXTROAMPHETAMINE-AMPHET ER,
#                                       DEXTROAMPHETAMINE-AMPHETAMINE, MYDAYIS
#                       
#                       Dates:          2006-02-28 to 2020-06-30
#
#             -----WriteCSV_Bloomberg_TRX-----
#
#             This function calls the Import_Bloomberg_TRX but does not save it to work with in R,
#             it just writes a CSV file for analysis elsewhere (e.g. in STATA)
#
#             Input:  The filename to an excel file containing the TRX output for a molecule
#                     from Bloomberg; a target_filename where you want a tidy CSV stored
#
#             Output: A CSV file at the specified target
#
#
#------------------------------------------------------------------------------------------

### Load packages
library(tidyverse)
library(readxl)
library(lubridate)

### Function to load and pivot data as a tidy dataframe with properly formatted date
Import_Bloomberg_TRX <- function(filename){
  
  # Load data for TRX as a tibble and pipe it forward...
  drug_data <- read_excel(filename) %>%
    # ...to pivot it from wide to long...
    pivot_longer(-DRUG_NAME, names_to = "MonthYear", values_to = "TRX") %>%
    # ...and convert the date from an Excel numeric to a proper R Date...
    mutate(MonthYear = as.Date(as.numeric(.$MonthYear), origin = "1899-12-30")) %>%
    # ...and rename "DRUG_NAME" to the nicer-looking "Drug"
    rename(Drug = DRUG_NAME)
  
  # Provide feedback to the user
  # Print the first few rows of data
  message("Data Preview:\n")
  print(head(drug_data))
  
  # Describe the data that was loaded
  
  message("\n\n",
          "Drugs imported: ", 
          unique(drug_data$DRUG_NAME) %>% 
            sort() %>% 
            unlist() %>% 
            paste(collapse = ', ') %>%
            strwrap(),
          "\n\n",
          "Dates:          ", min(drug_data$MonthYear), " to ", max(drug_data$MonthYear),
          "\n")
  
  return(drug_data)
  
} 

### Function to take in the data and save it as a CSV
WriteCSV_Bloomberg_TRX <- function(filename, target_filename){
  # Use the Import_Bloomberg_TRX function, save output as long CSV
  Import_Bloomberg_TRX(filename) %>% write_csv(target_filename)
  
  message("File saved as ", target_filename)
}

