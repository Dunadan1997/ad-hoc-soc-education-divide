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
  readRDS("Cached_Data/cached_education_divide_shp.rds")

merged_data_ess <- 
  readRDS("Cached_Data/cached_education_divide_ess.rds")

education_divide <- 
  read_csv("OFS/ad-hoc-soc-new-ed-divide-education-divide-ve02.csv", 
           col_types = list("i","f","i","f","f","f","f","d")) 


# Transform Data ----------------------------------------------------------

merged_data_shp$edulvl_fct_01 <-
  factor(
    merged_data_shp$`isced$$`, 
    levels = c(0,10,20,31,32,33,41,51,52,60), 
    labels = c(
      "Not completed primary (compulsory) education", 
      "Primary or first stage of basic education", 
      "Lower secondary or Second stage of basic education", 
      "Upper secondary education (preparation for tertiary education)", 
      "Upper secondary education (preparation for further prof. education)", 
      "Upper secondary education (entrance into the labor market)", 
      "Post-secondary education non tertiary (preparation for an institution for higher education)", 
      "First stage of tertiary education (general education)", 
      "First stage of tertiary education (professional education)", 
      "Second stage of tertiary education"
      )
    )

merged_data_shp$edulvl_fct_02 <-
  fct_collapse(merged_data_shp$edulvl_fct_01,
               primary = c("Not completed primary (compulsory) education", 
                           "Primary or first stage of basic education"),
               secondary = c("Lower secondary or Second stage of basic education", 
                             "Upper secondary education (preparation for tertiary education)", 
                             "Upper secondary education (preparation for further prof. education)", 
                             "Upper secondary education (entrance into the labor market)", 
                             "Post-secondary education non tertiary (preparation for an institution for higher education)"),
               tertiary_prfssnl = c("First stage of tertiary education (professional education)"),
               tertiary_unvrty = c("First stage of tertiary education (general education)",
                                   "Second stage of tertiary education")
               )

merged_data_shp$edulvl_fct_02 <-
  fct_relevel(
    merged_data_shp$edulvl_fct_02, 
    c("primary", "secondary", "tertiary_prfssnl", "tertiary_unvrty")
    )

# Exploratory Data Analysis -----------------------------------------------

# The threshold for educational divide was reached in the 1990s/2000s, when 25% to 30% of the voting population was university educated
education_divide %>% 
  group_by(ed_level_02, decade_of_birth, gender) %>% 
  summarise(sum = sum(prct_of_total, na.rm = T)) %>% 
  ggplot(aes(x = decade_of_birth, y = sum, fill = ed_level_02)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ gender) + 
  scale_y_continuous(breaks = seq(0, 1, 0.05)) + 
  geom_hline(yintercept = 0.30) + 
  geom_hline(yintercept = 0.5)
 







