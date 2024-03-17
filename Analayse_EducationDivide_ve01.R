# Title: Education Divide 
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Set the directory
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Loading packages
library(tidyverse)
library(memoise)
library(haven)

# Load functions
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load data
merged_data_shp <-
  readRDS("SHP/Data_Aggregated_1999_2022/cached_education_divide.rds")
education_divide <- 
  read_csv("/Users/brunoalvesdecarvalho/Desktop/ad_hoc_soc_new_ed_divide.csv", 
           col_types = list("i","f","i","f","f","f","d")) 


# Exploratory Data Analysis -----------------------------------------------

education_divide %>% group_by(ed_level_02, decade_of_birth) %>% summarise(sum = sum(prct_of_total, na.rm = T)) %>% ggplot(aes(x = decade_of_birth, y = sum, fill = ed_level_02)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0, 1, 0.05)) + geom_hline(yintercept = 0.30) + geom_hline(yintercept = 0.5)
 







