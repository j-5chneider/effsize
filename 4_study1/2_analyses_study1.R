library(ggdist)
library(corrgram)
library(formr)
library(tidyverse)
library(skimr)
library(car)
library(MASS)
library(ordinal)

## connect to formr (not needed if imported from data folder)
# formr_connect(email = "",
#               password = "")


#### EXPORT AND WRANGLE SURVEY ANSWERS #########################################
## import data from formr (not needed if imported from data folder)
# study1 <- formr_results("TueDiBASE_study1a")

# import from data folder
study1 <- rio::import("data/teachers_study1.Rdata")

# wrangle information on the plot type, ES, ...
plot_info <- study1 %>%
    pivot_longer(2:195, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::filter(str_detect(variables, "plot")) %>% # we only need the rows with info on plots
    tidyr::separate(col = values, into = c("type", "axis", "effsize"), # separate the info into three columns
                    sep = "_", remove = F) %>%
    dplyr::mutate(plot = variables,       # rename variables for later join
                  type = paste(type, axis, sep = "_")) %>%
    dplyr::select(-variables, -axis)

# wrangle answers to items on each page
item_values <- study1 %>%
    dplyr::select(-c(topic:itemo)) %>%
    pivot_longer(2:169, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::mutate(variables = case_when(      # recode variable names that have
        variables == "sensi_6" ~ "sensi_06",  # accidentally been labeled
        variables == "acccl_6" ~ "acccl_06",  # without zero
        variables == "accu3_6" ~ "accu3_06",
        variables == "accov_6" ~ "accov_06",
        variables == "diffi_6" ~ "diffi_06",
        variables == "infor_6" ~ "infor_06",
        variables == "value_6" ~ "value_06",
        TRUE ~ variables 
    )) %>%
    dplyr::mutate(plot = paste0("plotx_", str_sub(variables, -2, -1)), # create variable for later join
                  variables = str_sub(variables, 1, -4)) %>%    # rename variable names to get a data set 
                                                                # with one line per participant per page
    pivot_wider(id_cols = c(session, plot), names_from = "variables", 
                values_from = "values")


# join the two data sets
study1_w <- full_join(plot_info, item_values, 
                               by = c("session", "plot")) %>% # by participant and page (plot)
    dplyr::select(-values) %>%
    dplyr::mutate(acccl = as.numeric(acccl), # some var need to be defined as
                  accu3 = as.numeric(accu3), # numeric again
                  accov = as.numeric(accov),
                  diffi = as.numeric(diffi),
                  infor = as.numeric(infor),
                  value = as.numeric(value),
                  effsize = as.numeric(effsize),
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





######### DESCRIPTIVE PLOTS ####################################################
# reshape data set for plot
study1_wl <- study1_w %>%
    dplyr::select(-c(acccl:effsize_ov)) %>%
    pivot_longer(6:11, names_to = "variables", values_to = "values")

ggplot(study1_wl, aes(x=variables, y=values, color = type)) + 
    stat_summary(fun = mean, position = position_dodge(width=.5)) + 
    stat_summary(fun.data = mean_se, geom = "linerange", position = position_dodge(width=.5)) +
    scale_color_viridis_d() +
    theme_light()

# filter only cases that estimated the effect in the correct direction
# study1_w_sensi <- study1_w %>%
#     dplyr::filter((effsize > 0 & ))




######### CUMULATIVE LINK MODEL ################################################
study1_w_sensi <- study1_w %>%
    mutate(sensi = factor(sensi, levels = c("sensitivity_tablet_livelesson_simulation_videowithsub_superior",
                                            "sensitivity_tablet_livelesson_simulation_videowithsub_equal",
                                            "sensitivity_tablet_livelesson_simulation_videowithsub_inferior")))


ggplot(study1_w_sensi%>%dplyr::filter(!is.na(sensi)), 
       aes(x=type, fill = sensi)) +
    geom_bar(position = "dodge") +
    scale_fill_viridis_d() +
    theme_light()

ggplot(study1_w_sensi%>%dplyr::filter(!is.na(sensi)), 
       aes(x=effsize, fill = sensi)) +
    geom_density(adjust = 2, alpha = .5) +
    scale_fill_viridis_d() +
    theme_light() +
    facet_wrap(~ type)
    
## make loop over all plot types: compute CLM
# create data frame to save results
results_clm <- data.frame(threshold = as.character(),
                          value = as.numeric(),
                          type = as.character())

for (plottype in unique(study1_w_sensi$type)) { # for every plot type
    tmp_results <- clm(as.factor(sensi) ~ effsize, # compute a clm with effsize as predictor
                       data = study1_w_sensi%>%dplyr::filter(type==plottype))
    
    results_clm <- results_clm %>%  # save results in two rows
        add_row(threshold = "superior_equal", # results for threshold superior to equal
                value = tmp_results$coefficients[[1]],
                type = plottype) %>%
        add_row(threshold = "equal_inferior", # results for threshold equal to inferior
                value = tmp_results$coefficients[[2]],
                type = plottype)
}

# plot results with thresholds
#### @Samuel: Sind die thresholds überhaupt vergleichbar zwischen den CLMs???
ggplot(results_clm, aes(x=value, y=type, color = threshold)) +
    geom_point(size = 3) +
    scale_color_viridis_d() +
    theme_light()



######### TIMESTAMP DATA ######################################################
# import data
study1_timestamp <- rio::import("data/teachers_study1_detailed.csv")


# wrangle data
study1_timestamp <- study1_timestamp %>%
    mutate(plot = paste0("plotx_", str_sub(item_name, -2, -1))) %>%  # create var with plot number
    dplyr::filter(str_detect(item_name, "sensi|acccl|accu3|accov")) %>% # we only need vars sensitivity or accuracy
    mutate(item_name = str_sub(item_name, 1, 5)) %>%  # delete the page number in item name
    dplyr::select(session, item_name, answer, answered_relative, plot) %>% 
    pivot_wider(id_cols = c(session, plot), names_from = item_name, # make structure similar to existing data set
                values_from = answered_relative) %>%
    mutate(plot = ifelse(plot == "plotx__6", "plotx_06", plot))%>% # recode wrong item labelling
    rowwise() %>%
    mutate(effic = min(sensi, acccl, accu3, accov, na.rm=T)) %>% # what was the the time of the first item to be clicked?
    dplyr::select(session, plot, effic)


# join data and time stamps
study1_w_timestamp <- left_join(study1_w, study1_timestamp, by=c("session", "plot"))



### generate data set so that the six plots from the same type are ordered
### one after the other (and not 1-24)
study1_w_timestamp <- study1_w_timestamp %>%
    group_by(session, type) %>%
    mutate(plot_nr_within = 1:6) %>%
    ungroup()
    

ggplot(study1_w_timestamp, aes(x=plot_nr_within, y=effic)) +
    stat_summary(fun.data=median_q1q3) + # show 25 to 75% quartile
    stat_summary(fun = median) + # using median because of outliers
    theme_light() +
    facet_wrap(~type, nrow = 2)




### filter only cases that are in 2 SD range of median to exclude unplausible outliers
# compute median and sd 
median_and_sd <- study1_w_timestamp %>%
    summarize(median = median(effic, na.rm=T),
              sd     = sd(effic, na.rm=T))

# filter away if value > 2 sd from median
study1_w_timestamp_filtered <- study1_w_timestamp %>%
    dplyr::filter(effic <= (median_and_sd$median + (2*median_and_sd$sd)))

ggplot(study1_w_timestamp_filtered, aes(x=plot_nr_within, y=effic)) +
    geom_smooth(method = loess, na.rm=T) +
    stat_summary(fun = mean) + # CAUTION! USING MEAN
    theme_light() +
    facet_wrap(~type, nrow = 2)


### regressions with exponential decay for each plot type

fit_exp_gard_x <- lm(log(effic) ~ plot_nr_within, 
                     data = study1_w_timestamp%>%dplyr::filter(type=="gardneraltman_xaxis"))
summary(fit_exp_gard_x)
exp(fit_exp_gard_x$coefficients) # convert coefficients back to original scale

fit_exp_half_x <- lm(log(effic) ~ plot_nr_within, 
                     data = study1_w_timestamp%>%dplyr::filter(type=="halfeye_xaxis"))
summary(fit_exp_half_x)
exp(fit_exp_half_x$coefficients) # convert coefficients back to original scale

fit_exp_half_y <- lm(log(effic) ~ plot_nr_within, 
                     data = study1_w_timestamp%>%dplyr::filter(type=="halfeye_yaxis"))
summary(fit_exp_half_y)
exp(fit_exp_half_y$coefficients) # convert coefficients back to original scale

fit_exp_rain_y <- lm(log(effic) ~ plot_nr_within, 
                     data = study1_w_timestamp%>%dplyr::filter(type=="raincloud_yaxis"))
summary(fit_exp_rain_y)
exp(fit_exp_rain_y$coefficients) # convert coefficients back to original scale


### generalized regressions with negative logistic fitting for each plot type
fit_exp_gard_x <- glm.nb(effic ~ plot_nr_within, 
                     data = study1_w_timestamp%>%dplyr::filter(type=="gardneraltman_xaxis"))
summary(fit_exp_gard_x)

tmp <- cbind(study1_w_timestamp%>%dplyr::filter(type=="gardneraltman_xaxis"), 
             fit_exp_gard_x$fitted.values)

ggplot(tmp, aes(x=plot_nr_within, y=fit_exp_gard_x$fitted.values)) +
    geom_point()
