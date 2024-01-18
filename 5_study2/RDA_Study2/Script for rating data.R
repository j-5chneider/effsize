# Script for rating data (open answer format)
library(tidyverse)

# for random row oder
set.seed("250582")

# Import item data from .csv
study2_data_imported <-
    read_csv(here::here("data/teachers_study2_update01.csv")) 

# Remove not finishing particpants
study2_data_ended <-
    study2_data_imported |> 
    filter(!is.na(ended))

# Select items and order randomly
study2_data_ended |> 
    select(session, 
           uxaxs_1x, uxaxs_1y,
           uyaxs_1x, uyaxs_1y) |> 
    sample_frac(1) |> 
    mutate(rating_juergen = NA,
           rating_kristina = NA,
           should_be_rated_by = 
               c(rep("Kristina & Juergen", ceiling(0.1*n())), # 10% double coding
                 rep(c(rep("Kristina", 9), # 9 times K
                 rep("Juergen", 9), # 9 times J
                 rep("Kristina & Juergen", 2) # 2 together
                 ), 100)[1:floor(0.9*n())]
           )) |> View()
    openxlsx::write.xlsx("data/data_for_rating.xlsx")
