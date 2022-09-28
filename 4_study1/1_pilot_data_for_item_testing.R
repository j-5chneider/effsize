library(hrbrthemes)
library(ggdist)
library(corrgram)
library(formr)
library(tidyverse)
library(skimr)
library(car)


tudibase_item_pilot <- rio::import("data/pilot_item_testing.Rdata") %>%
    dplyr::filter(created > "2022-09-27 18:00:00" & !is.na(ended)) # filter only subjects from second pilot


plot_info <- tudibase_item_pilot %>%
    dplyr::filter(is.na(attention_check) & !is.na(ended)) %>%
    dplyr::select(-c(session:expired, attention_check, informed_consent, 
                     mc_study:implement)) %>%
    pivot_longer(2:243, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::filter(str_detect(variables, "plot")) %>%
    tidyr::separate(col = values, into = c("type", "axis", "effsize"), 
                    sep = "_", remove = F) %>%
    dplyr::mutate(plot = variables,
                  type = paste(type, axis, sep = "_")) %>%
    dplyr::select(-variables, -axis)

item_values <- tudibase_item_pilot %>%
    dplyr::filter(is.na(attention_check) & !is.na(ended)) %>%
    dplyr::select(-c(session:expired, attention_check, informed_consent, 
                     mc_study:implement, topic, plot1:plot24, item_order)) %>%
    pivot_longer(2:217, names_to = "variables", values_to = "values", 
                 values_transform = as.character) %>%
    dplyr::mutate(variables = case_when(
        variables == "accuracy_u3_1a" ~ "accuracy_u3a_1",
        variables == "accuracy_u3_1b" ~ "accuracy_u3b_1",
        variables == "accuracy_u3_2a" ~ "accuracy_u3a_2",
        variables == "accuracy_u3_2b" ~ "accuracy_u3b_2",
        variables == "accuracy_u3_3a" ~ "accuracy_u3a_3",
        variables == "accuracy_u3_3b" ~ "accuracy_u3b_3",
        variables == "accuracy_u3_4a" ~ "accuracy_u3a_4",
        variables == "accuracy_u3_4b" ~ "accuracy_u3b_4",
        variables == "accuracy_u3_5a" ~ "accuracy_u3a_5",
        variables == "accuracy_u3_5b" ~ "accuracy_u3b_5",
        variables == "accuracy_u3_6a" ~ "accuracy_u3a_6",
        variables == "accuracy_u3_6b" ~ "accuracy_u3b_6",
        variables == "accuracy_u3_7a" ~ "accuracy_u3a_7",
        variables == "accuracy_u3_7b" ~ "accuracy_u3b_7",
        variables == "accuracy_u3_8a" ~ "accuracy_u3a_8",
        variables == "accuracy_u3_8b" ~ "accuracy_u3b_8",
        variables == "accuracy_u3_9a" ~ "accuracy_u3a_9",
        variables == "accuracy_u3_9b" ~ "accuracy_u3b_9",
        variables == "accuracy_u3_10a" ~ "accuracy_u3a_10",
        variables == "accuracy_u3_10b" ~ "accuracy_u3b_10",
        variables == "accuracy_u3_11a" ~ "accuracy_u3a_11",
        variables == "accuracy_u3_11b" ~ "accuracy_u3b_11",
        variables == "accuracy_u3_12a" ~ "accuracy_u3a_12",
        variables == "accuracy_u3_12b" ~ "accuracy_u3b_12",
        variables == "accuracy_u3_13a" ~ "accuracy_u3a_13",
        variables == "accuracy_u3_13b" ~ "accuracy_u3b_13",
        variables == "accuracy_u3_14a" ~ "accuracy_u3a_14",
        variables == "accuracy_u3_14b" ~ "accuracy_u3b_14",
        variables == "accuracy_u3_15a" ~ "accuracy_u3a_15",
        variables == "accuracy_u3_15b" ~ "accuracy_u3b_15",
        variables == "accuracy_u3_16a" ~ "accuracy_u3a_16",
        variables == "accuracy_u3_16b" ~ "accuracy_u3b_16",
        variables == "accuracy_u3_17a" ~ "accuracy_u3a_17",
        variables == "accuracy_u3_17b" ~ "accuracy_u3b_17",
        variables == "accuracy_u3_18a" ~ "accuracy_u3a_18",
        variables == "accuracy_u3_18b" ~ "accuracy_u3b_18",
        variables == "accuracy_u3_19a" ~ "accuracy_u3a_19",
        variables == "accuracy_u3_19b" ~ "accuracy_u3b_19",
        variables == "accuracy_u3_20a" ~ "accuracy_u3a_20",
        variables == "accuracy_u3_20b" ~ "accuracy_u3b_20",
        variables == "accuracy_u3_21a" ~ "accuracy_u3a_21",
        variables == "accuracy_u3_21b" ~ "accuracy_u3b_21",
        variables == "accuracy_u3_22a" ~ "accuracy_u3a_22",
        variables == "accuracy_u3_22b" ~ "accuracy_u3b_22",
        variables == "accuracy_u3_23a" ~ "accuracy_u3a_23",
        variables == "accuracy_u3_23b" ~ "accuracy_u3b_23",
        variables == "accuracy_u3_24a" ~ "accuracy_u3a_24",
        variables == "accuracy_u3_24b" ~ "accuracy_u3b_24",
        TRUE ~ variables 
    )) %>%
    dplyr::mutate(plot = gsub("_", "", str_sub(variables, -2, -1)),
                  plot = paste0("plot", plot),
                  variables = gsub("_", "", str_sub(variables, 1, -3))) %>%
    pivot_wider(id_cols = c(prolific_pid, plot), names_from = "variables", 
                values_from = "values")



tudibase_item_pilot_w <- full_join(plot_info, item_values, 
                                   by = c("prolific_pid", "plot")) %>%
    dplyr::select(-values) %>%
    dplyr::mutate(accuracycliff = as.numeric(accuracycliff),
                  accuracyu3a = as.numeric(accuracyu3a),
                  accuracyu3b = as.numeric(accuracyu3b),
                  accuracyoverlap = as.numeric(accuracyoverlap),
                  difficulty = as.numeric(difficulty),
                  informative = as.numeric(informative),
                  value = as.numeric(value),
                  implement = as.numeric(implement),
                  effsize = as.numeric(effsize),
                  accuracycliff_betrag = abs(accuracycliff),
                  accuracyu3a_betrag = abs(accuracyu3a - 50),
                  accuracyu3b_betrag = abs(accuracyu3b - 50),
                  effsize_betrag = abs(effsize))


### GROUP MEAN CENTERING #######################################################
# (to consider clustered data https://doi.org/10.1186/2196-0739-1-7)
group_means <- tudibase_item_pilot_w %>%
    dplyr::group_by(prolific_pid) %>%
    dplyr::summarise(across(accuracycliff:effsize_betrag, .fns = mean, na.rm = T, 
                            .names = "{.col}_groupmean")) %>%
    right_join(tudibase_item_pilot_w, by = "prolific_pid")

tudibase_item_pilot_w_c <- group_means %>%
    dplyr::mutate(accuracycliff_centered = accuracycliff - accuracycliff_groupmean,
                  accuracyu3a_centered = accuracyu3a - accuracyu3a_groupmean,
                  accuracyu3b_centered = accuracyu3b - accuracyu3b_groupmean,
                  accuracyoverlap_centered = accuracyoverlap - accuracyoverlap_groupmean,
                  difficulty_centered = difficulty - difficulty_groupmean,
                  informative_centered = informative - informative_groupmean,
                  value_centered = value - value_groupmean,
                  implement_centered = implement - implement_groupmean,
                  accuracycliff_betrag_centered = accuracycliff_betrag - accuracycliff_betrag_groupmean,
                  accuracyu3a_betrag_centered = accuracyu3a_betrag - accuracyu3a_betrag_groupmean,
                  accuracyu3b_betrag_centered = accuracyu3b_betrag - accuracyu3b_betrag_groupmean,
                  effsize_betrag_centered = effsize_betrag - effsize_betrag_groupmean)


######### DESCRIPTIVE PLOTS ####################################################
ggplot(tudibase_item_pilot_w_c, aes(y=accuracycliff)) + 
    ggdist::stat_halfeye(
        justification = -.1
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip()


ggplot(tudibase_item_pilot_w_c, aes(y=accuracyu3a)) + 
    ggdist::stat_halfeye(
        justification = -.1
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme_ipsum() +
    coord_flip()


ggplot(tudibase_item_pilot_w_c, aes(y=accuracyu3b)) + 
    ggdist::stat_halfeye(
        justification = -.1
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme_ipsum() +
    coord_flip()


ggplot(tudibase_item_pilot_w_c, aes(y=accuracyoverlap)) + 
    ggdist::stat_halfeye(
        justification = -.1
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip()


ggplot(tudibase_item_pilot_w_c, aes(y=difficulty)) + 
    ggdist::stat_halfeye(
        justification = -.1,
        adjust = 2,
        alpha = .5
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip() +
    facet_wrap( ~ as.factor(type), ncol = 1)


ggplot(tudibase_item_pilot_w_c, aes(y=informative)) + 
    ggdist::stat_halfeye(
        justification = -.1,
        adjust = 2
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip()

ggplot(tudibase_item_pilot_w_c, aes(y=informative)) + 
    ggdist::stat_halfeye(
        justification = -.1,
        adjust = 2,
        alpha = .5
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip() +
    facet_wrap( ~ as.factor(type), ncol = 1)


ggplot(tudibase_item_pilot_w_c, aes(y=value)) + 
    ggdist::stat_halfeye(
        justification = -.1,
        adjust = 2,
        alpha = .5
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip() +
    facet_wrap( ~ as.factor(type), ncol = 1)


ggplot(tudibase_item_pilot_w_c, aes(y=implement)) + 
    ggdist::stat_halfeye(
        justification = -.1,
        adjust = 2,
        alpha = .5
    ) + 
    geom_point(aes(x=0),
               size = 1.3,
               alpha = .3,
               position = position_jitter(
                   seed = 1, height = .005
               )
    ) + 
    theme_ipsum() +
    coord_flip() +
    facet_wrap( ~ as.factor(type), ncol = 1)


ggplot(tudibase_item_pilot_w_c%>%dplyr::filter(!is.na(sensitivity)), 
       aes(x=type, fill = sensitivity)) +
    geom_bar(position = "dodge")




########### CORR ALL ITEMS #####################################################
## CODINGS OF ITEMS:         ##
# high values in effsize:
#   - reading on paper superior
#   - video recorded lesson  superior
#   - real lab  superior
#   - without subtitles  superior
# 
# 
# high values in accuracycliff:
#   - reading on paper superior
#   - video recorded lesson  superior
#   - real lab  superior
#   - without subtitles  superior
# 
# 
# high values in accuracyU3a:
#   - reading on tablet superior
#   - live lesson  superior
#   - computer simulation  superior
#   - with subtitles  superior
# 
# high values in accuracyU3b:
#   - reading on tablet superior
#   - live lesson  superior
#   - computer simulation  superior
#   - with subtitles  superior



# correlation centered items
# (to consider clustered data https://doi.org/10.1186/2196-0739-1-7)
corrgram(tudibase_item_pilot_w_c%>%dplyr::select(
    effsize, 
    accuracycliff_centered:accuracyu3b_centered, 
    difficulty_centered:implement_centered),
    lower.panel = "panel.pie",
    upper.panel = "panel.cor",
    cor.method = "spearman") # spearman as we don't assume normally 
# distrubuted data, as perfectly accurate
# estimations should be mostly uniform


# Correlations with overlap (has to be absolute value of accuracy items)
corrgram(tudibase_item_pilot_w_c%>%dplyr::select(
    effsize_betrag_centered,
    accuracyoverlap_centered,
    accuracycliff_betrag_centered:accuracyu3b_betrag_centered),
    lower.panel = "panel.pie",
    upper.panel = "panel.cor",
    cor.method = "spearman")
