library(formr)
library(tidyverse)

formr_connect(email = "",
              password = "")

# Download data
study2_data <- formr_results("TueDiBASE_study2")


# ALL RESPONDENTS
nrow(study2_data)



# RESPONDENTS WHO ENDED
nrow(study2_data |>
    dplyr::filter(!is.na(ended)))



# RESPONDENTS WHO ENDED AND DID NOT PASS THE ATTENTION CHECK
nrow(study2_data |>
         dplyr::filter(!is.na(ended)) |>
         dplyr::filter(!is.na(attention_check)))

    # their answers to the attention check were...
    study2_data |>  
        dplyr::filter(!is.na(ended)) |>
        dplyr::filter(!is.na(attention_check)) |>
        pull(attention_check) |>
        table()



############# PARTICIPANTS WE ARE INTERESTED IN ################################
# RESPONDENTS WHO ENDED AND PASSED THE ATTENTION CHECK
nrow(study2_data |>
    dplyr::filter(!is.na(ended)) |>
    dplyr::filter(is.na(attention_check)))







