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

# Transform SHP Data
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


merged_data_shp$gndr_fct <- 
  factor(
    merged_data_shp$`sex$$`,
    levels = c(1,2),
    labels = c("man", "womqn")
  )

merged_data_shp$opinion_eu <- 
  factor(
    merged_data_shp$`p$$p14`,
    levels = c(1,2,3),
    labels = c(
      "in favour of joining the EU",
      "neither",
      "in favour of staying outside the EU"
      )
  )

merged_data_shp$opinion_army <-
  factor(
    merged_data_shp$`p$$p12`,
    levels = c(1,2,3),
    labels = c(
      "strong army",
      "neither",
      "no army"
    )
  )

merged_data_shp$opinion_foreigners <-
  factor(
    merged_data_shp$`p$$p15`,
    levels = c(1,2,3),
    labels = c(
      "in favour of equality of opportunities",
      "neither",
      "in favour of better opportunities for Swiss citizens"
    )
  )

merged_data_shp$sat_democracy <-
  ifelse(merged_data_shp$`p$$p02` < 0, NA, merged_data_shp$`p$$p02`)

merged_data_shp$trust_govt <-
  ifelse(merged_data_shp$`p$$p04` < 0, NA, merged_data_shp$`p$$p04`)

merged_data_shp$pol_influence <-
  ifelse(merged_data_shp$`p$$p03` < 0, NA, merged_data_shp$`p$$p03`)

merged_data_shp$pol_ideology <-
  ifelse(merged_data_shp$`p$$p10` < 0, NA, merged_data_shp$`p$$p10`)

# Transform ESS Data
merged_data_ess$edulvl_fct_01 <- 
  factor(
    merged_data_ess$eisced,
    levels = c(0:7, 55),
    labels = c(
      "Not possible to harmonise into ES-ISCED",
      "ES-ISCED I , less than lower secondary", 
      "ES-ISCED II, lower secondary",
      "ES-ISCED IIIb, lower tier upper secondary",
      "ES-ISCED IIIa, upper tier upper secondary",
      "ES-ISCED IV, advanced vocational, sub-degree",
      "ES-ISCED V1, lower tertiary education, BA level",
      "ES-ISCED V2, higher tertiary education, >= MA level",
      "Other"
      )
    )

merged_data_ess$edulvl_fct_02 <- 
  fct_collapse(
    merged_data_ess$edulvl_fct_01,
    primary = "ES-ISCED I , less than lower secondary",
    secondary = c(
      "ES-ISCED II, lower secondary", 
      "ES-ISCED IIIb, lower tier upper secondary",
      "ES-ISCED IIIa, upper tier upper secondary",
      "ES-ISCED IV, advanced vocational, sub-degree"
      ),
    tertiary = c(
      "ES-ISCED V1, lower tertiary education, BA level", 
      "ES-ISCED V2, higher tertiary education, >= MA level"
      ),
    other_level = NA
  )

merged_data_ess$gndr_fct <-
  factor(
    merged_data_ess$gndr,
    levels = c(1,2),
    labels = c("man", "woman")
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

# Gap in Swiss values?

# Opinion of joining EU follows similar trend overtime, but is markedly lower among the non-university educated
merged_data_shp %>% 
  filter(!is.na(opinion_eu) & !is.na(edulvl_fct_02)) %>% 
  group_by(year, edulvl_fct_02, opinion_eu) %>% 
  summarise(n = n()) %>% 
  group_by(year, edulvl_fct_02) %>% 
  mutate(prct = n / sum(n)) %>% 
  ggplot(aes(year, prct, group = edulvl_fct_02, color = edulvl_fct_02)) + 
  geom_line() + 
  geom_smooth() +
  facet_wrap(~ opinion_eu)

# Gap in democratic views?
 







