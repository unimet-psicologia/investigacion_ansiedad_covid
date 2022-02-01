### Yarfraz Analysis 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(visdat)


# data --------------------------------------------------------------------

df <- read_delim("data/dataset_ie_ansiedad_ccs_vzla_202105.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Data scanning -----------------------------------------------------------

colnames(df)

### some names will be changed 

str(df)

### all factor cols are character when they must be numbers


# Data cleaning -----------------------------------------------------------


