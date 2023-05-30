#Master script to calculate education shares of labor force
#This was created for technical assistance purposes for the Immigration Research Initiative

library(tidyverse)
library(epiextractr)
library(here)
library(labelled)


source('code/01_cps_analysis.R', echo = TRUE)

#Load and clean BLS education data, projections data, and bls SOC crosswalk
source('code/02_clean_data.R', TRUE)

#Merge and analyze education and employment projection matrices
source('code/03_employment_matrix_merge.R', TRUE)
