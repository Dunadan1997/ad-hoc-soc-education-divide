# Title: Education Divide 
# Author: Bruno Alves de Carvalho
# Status: ongoing

# Set the directory to the data warehouse
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Load packages
library(tidyverse)
library(memoise)
library(haven)

# Load functions
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load data
aggregated_data_shp <- 
  readRDS("SHP/Data_Aggregated_1999_2022/cached_data_shp.rds")

# Select variables
selected_vars_shp <- 
  rep(
    list(
      c(
        # ids
        "idpers",
        "idhous$$",
        
        # socio-demographics
        "year",
        "age$$",
        "generation",
        "sex$$",
        "iptotni", # total income
        "iwyni", # working income
        "canton$$",
        "com2_$$",
        
        # level of education
        "isced$$",
        "educat$$",
        
        # political factors
        "p$$p10", # political position, scale 1 (left) to 10 (right)
        "p$$p11", # member of political party
        "p$$p19", # party of choice if elections tomorrow
        "p$$p46", # political position of father
        "p$$p47", # political position of mother
        "p$$p67", # party identification
        
        # political views
        "p$$p14", # opinion on joining the EU (scale of 1 to 3)
        "p$$p15", # opinion on chances for foreigners (scale 1 to 3)
        "p$$p12", # opinion on Swiss army
        "p$$p02", # satisfaction with democracy (scale 0 to 10, completely satisfied)
        "p$$p03", # feeling about political influence (scale 0 to 10, a very strong influence)
        "p$$p04", # trust in federal government (scale 0 to 10, full confidence)
        "c$$p01", # handling of coronacrisis, scale 0 to 10 (completely agree)
        "c$$p02", # restriction of rights, scale 1 to 3
        "p$$p100", # feelings toward LGBTIQ, homosexuals, scale 0 to 10 (favorable)
        "p$$p101", # feelings toward LGBTIQ, bisexual, scale 0 to 10 (favorable)
        "p$$p102" # feelings toward LGBTIQ, transgender, scale 0 to 10 (favorable)
      )
    ), 
    length(1999:2022)
  )

select_vars_ess <-
  c(
    # socio-demographic factors
    "edlvdch",
    "eisced",
    "agea",
    "yrbrn",
    "gndr",
    "cntry",
    "year",
    
    # political views
    "prtvthch", # party voted for in last national election
    "lrscale", # placement on left right scale (scale 0 to 10, right)
    "psppsgva", # political system allows people to have a say in what government does
    "stfdem", # how satisfied with the way democracy works in country (scale 0 to 10, extremely satisfied)
    "euftf", # European unification go further or gone too far (scale 0 to 10, unification go further)
    "imueclt", # country's cultural life undermined or enriched by immigrants (scale 0 to 10, cultural life enriched)
    "votedir", # citizens have the final say on political issues by voting directly in referendums (scale 0 to 1)
    "rghmgpr", # the rights of minority groups are protected
    "viepol", # the views of ordinary people prevail over the views of the political elite
    "wpestop", # the will of the people cannot be stopped
    "chpldmi", # best for democracy: government changes politics in response to what most people think 
    "stpldmi", # important for democracy: government sticks to policies regardless of what most people think
    "impdema", # how important things are for democracy: free and fair elections
    "impdemb", # how important things are for democracy: people > elite
    "impdemc", # how important things are for democracy: direct democracy
    "impdemd", # how important things are for democracy: welfare govt
    "impdeme", # how important things are for democracy: rule of law
    
    # weights
    "pspwght", # post-stratification weight including design weight
    "pweight" # populate size weight
  )

# Merge data
merged_data_shp <-
  shp_merge_data(aggregated_data_shp, selected_vars_shp)

aggregated_data_ess <- 
  aggregate_data_ess()

merged_data_ess <- 
  merge_data_ess(aggregated_data_ess)

# Cache data
saveRDS(merged_data_shp, "Cached_Data/cached_education_divide_shp.rds")
saveRDS(merged_data_ess, "Cached_Data/cached_education_divide_ess.rds")



