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

### Interraterreliability after 20 Rating ######################################
rating_1_rater_1 <- 
        readxl::read_excel(here::here("data/data_for_rating_rater1.xlsx"))
rating_1_rater_2 <- 
        readxl::read_excel(here::here("data/data_for_rating_rater2.xlsx"))

library(irr)
## uxaxs_1x
kalpha_uxaxs_1x <-
    kripp.alpha(matrix(
        c(rating_1_rater_1$uxaxs_1x_rating_juergen[1:37],
          rating_1_rater_2$uxaxs_1x_rating_kristina[1:37]),
        nrow = 2,
        byrow = T), "nominal")

## uxaxs_1x
kalpha_uxaxs_1y <-
    kripp.alpha(matrix(
        c(rating_1_rater_1$uxaxs_1y_rating_juergen[1:37],
          rating_1_rater_2$uxaxs_1y_rating_kristina[1:37]),
        nrow = 2,
        byrow = T), "nominal")

## uyaxs_1x
kalpha_uyaxs_1x <-
    kripp.alpha(matrix(
        c(rating_1_rater_1$rating_juergen_uyaxs_1x[1:37],
          rating_1_rater_2$rating_kristina_uyaxs_1x[1:37]),
        nrow = 2,
        byrow = T), "nominal")

## uyaxs_1x
kalpha_uyaxs_1y <-
    kripp.alpha(matrix(
        c(rating_1_rater_1$rating_juergen_uyaxs_1y[1:37],
          rating_1_rater_2$rating_kristina_uyaxs_1y[1:37]),
        nrow = 2,
        byrow = T), "nominal")
