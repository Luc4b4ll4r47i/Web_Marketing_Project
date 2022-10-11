#### OPTIONS ####

options(scipen=999) 
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
## add other libraries

#### DIRECTORIES ####
working_dir = "DMktg_DSLab_R_Scripts"
data_dir = "DMktg_DSLab_Data_1"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  , 'D00_som.R'
  , 'D01_rfm.R'
  , 'D02_churn.R'
  , 'D03_mba.R'
  )

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}
