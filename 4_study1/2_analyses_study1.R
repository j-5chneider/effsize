#library(hrbrthemes)
library(ggdist)
library(corrgram)
library(formr)
library(tidyverse)
library(skimr)
library(car)


formr_connect(email = "",
              password = "")

tudibase_study1 <- formr_results("TueDiBASE_study1a")


plot_info <- tudibase_study1 %>%
    dplyr::filter((is.na(attention_check) | attention_check == "") & 
                  !is.na(ended) & 
                  prpid != "56fd94d5291bb50008ea5805") %>% # statistician (messaged us via prolific)
    dplyr::select(-c(session:expired, attention_check, conse, 
                     mcstu:value)) %>%
    pivot_longer(2:195, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::filter(str_detect(variables, "plot")) %>%
    tidyr::separate(col = values, into = c("type", "axis", "effsize"), 
                    sep = "_", remove = F) %>%
    dplyr::mutate(plot = variables,
                  type = paste(type, axis, sep = "_")) %>%
    dplyr::select(-variables, -axis)

item_values <- tudibase_study1 %>%
    dplyr::filter((is.na(attention_check) | attention_check == "") & 
                      !is.na(ended) & 
                      prpid != "56fd94d5291bb50008ea5805") %>% # statistician (messaged us via prolific)
    dplyr::select(-c(session:expired, attention_check, conse, 
                     mcstu:value, topic:itemo)) %>%
    pivot_longer(2:169, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::mutate(variables = case_when(      # recode variable names that have
        variables == "sensi_6" ~ "sensi_06",  # accidentally been labeled
        variables == "acccl_6" ~ "acccl_06",   # without zero
        variables == "accu3_6" ~ "accu3_06",
        variables == "accov_6" ~ "accov_06",
        variables == "diffi_6" ~ "diffi_06",
        variables == "infor_6" ~ "infor_06",
        variables == "value_6" ~ "value_06",
        TRUE ~ variables 
    )) %>%
    dplyr::mutate(#plot = gsub("_", "", str_sub(variables, -2, -1)),
                  plot = paste0("plotx_", str_sub(variables, -2, -1)),
                  variables = str_sub(variables, 1, -4)) %>%
    pivot_wider(id_cols = c(prpid, plot), names_from = "variables", 
                values_from = "values")



tudibase_study1_w <- full_join(plot_info, item_values, 
                               by = c("prpid", "plot")) %>%
    dplyr::select(-values) %>%
    dplyr::mutate(acccl = as.numeric(acccl),
                  accu3 = as.numeric(accu3),
                  accov = as.numeric(accov),
                  diffi = as.numeric(diffi),
                  infor = as.numeric(infor),
                  value = as.numeric(value),
                  effsize = as.numeric(effsize),
                  # acccl_betrag = abs(acccl),
                  # accu3_betrag = abs(accu3 - 50),
                  # effsize_betrag = abs(effsize)
                  effsize_cl = case_when( # there is no negative Cliff's Delta, so we have to compute two transformations
                      effsize > 0 ~   (((2*pnorm(effsize/2))-1)/pnorm(effsize/2)), # transform the actual effect size Cohen's d to Cliff's Delta
                      effsize < 0 ~ (- (((2*pnorm(abs(effsize)/2))-1)/pnorm(abs(effsize)/2))) # transform the actual effect size Cohen's d to Cliff's Delta and make it negative as in the item
                  ),
                  effsize_u3 = pnorm(effsize), # transform the actual effect size Cohen's d to Cohen's U3
                  effsize_ov = 2 * pnorm(-abs(effsize) / 2), # transform the actual effect size Cohen's d to overlap
                  acccl_eff = (acccl - effsize_cl)/2, # actual accuracy of rating relative to depicted effectsize on plot
                  accu3_eff = (accu3/100) - effsize_u3, # actual accuracy of rating relative to depicted effectsize on plot
                  accov_eff = (accov/100) - effsize_ov, # actual accuracy of rating relative to depicted effectsize on plot
                  diffi_normed = ((diffi - 1)  / 3) - 1, # transform item to -1 to 1
                  infor_normed = ((infor - 1)  / 3) - 1, # transform item to -1 to 1
                  value_normed = ((value - 1)  / 3) - 1) # transform item to -1 to 1





tudibase_study1_wl <- tudibase_study1_w %>%
    dplyr::select(-c(acccl:effsize_ov)) %>%
    pivot_longer(6:11, names_to = "variables", values_to = "values")
    


# ### GROUP MEAN CENTERING #######################################################
# # (to consider clustered data https://doi.org/10.1186/2196-0739-1-7)
# group_means <- tudibase_item_pilot_w %>%
#     dplyr::group_by(ID) %>%
#     dplyr::summarise(across(accuracycliff:effsize_betrag, .fns = mean, na.rm = T, 
#                             .names = "{.col}_groupmean")) %>%
#     right_join(tudibase_item_pilot_w, by = "ID")
# 
# tudibase_item_pilot_w_c <- group_means %>%
#     dplyr::mutate(accuracycliff_centered = accuracycliff - accuracycliff_groupmean,
#                   accuracyu3a_centered = accuracyu3a - accuracyu3a_groupmean,
#                   accuracyu3b_centered = accuracyu3b - accuracyu3b_groupmean,
#                   accuracyoverlap_centered = accuracyoverlap - accuracyoverlap_groupmean,
#                   difficulty_centered = difficulty - difficulty_groupmean,
#                   informative_centered = informative - informative_groupmean,
#                   value_centered = value - value_groupmean,
#                   implement_centered = implement - implement_groupmean,
#                   accuracycliff_betrag_centered = accuracycliff_betrag - accuracycliff_betrag_groupmean,
#                   accuracyu3a_betrag_centered = accuracyu3a_betrag - accuracyu3a_betrag_groupmean,
#                   accuracyu3b_betrag_centered = accuracyu3b_betrag - accuracyu3b_betrag_groupmean,
#                   effsize_betrag_centered = effsize_betrag - effsize_betrag_groupmean)


######### DESCRIPTIVE PLOTS ####################################################
ggplot(tudibase_study1_wl, aes(x=variables, y=values, color = type)) + 
    stat_summary(fun = mean, position = position_dodge(width=.5)) + 
    stat_summary(fun.data = mean_se, geom = "linerange", position = position_dodge(width=.5)) +
    scale_color_viridis_d() +
    theme_light()


# tudibase_study1_w_sensi <- tudibase_study1_w %>%
#     dplyr::filter((effsize > 0 & ))




######### CUMULATIVE LINK MODEL ################################################
library(ordinal)

tudibase_study1_w_sensi <- tudibase_study1_w %>%
    mutate(sensi = factor(sensi, levels = c("sensitivity_tablet_livelesson_simulation_videowithsub_superior",
                                            "sensitivity_tablet_livelesson_simulation_videowithsub_equal",
                                            "sensitivity_tablet_livelesson_simulation_videowithsub_inferior")))


ggplot(tudibase_study1_w_sensi, 
       aes(x=type, fill = sensi)) +
    geom_bar(position = "dodge") +
    scale_fill_viridis_d() +
    theme_light()
    

# clm(as.factor(sensi) ~ effsize, data = tudibase_study1_w_sensi)

results_clm <- data.frame(threshold = as.character(),
                          value = as.numeric(),
                          type = as.character())

for (plottype in unique(tudibase_study1_w_sensi$type)) {
    tmp_results <- clm(as.factor(sensi) ~ effsize, 
                       data = tudibase_study1_w_sensi%>%dplyr::filter(type==plottype))
    
    results_clm <- results_clm %>%
        add_row(threshold = "superior_equal",
                value = tmp_results$coefficients[[1]],
                type = plottype) %>%
        add_row(threshold = "equal_inferior",
                value = tmp_results$coefficients[[2]],
                type = plottype)
}


ggplot(results_clm, aes(x=value, y=type, color = threshold)) +
    geom_point()
