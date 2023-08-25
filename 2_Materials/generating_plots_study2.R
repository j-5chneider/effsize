############################################################################# #
### Script for producing and saving the plots                   ############# #
### Conditions: Signaling differencs (Cohen's U3) & overlap     ###############
###                                                             ############# #
### Original script from Samuel Merk                            ############# #
############################################################################# #


library(bayestestR) 
library(tidyverse)
library(hrbrthemes)
library(ggdist)
# library(dabestr)
library(ggh4x)
# library(egg)
# library(grid)

## basic characteristics of the data ##########################################
# generate list of group names
group_names <- list(c("reading\non tablet", "reading\non paper"),
                    c("live\nlesson", "video recorded\nlesson"),
                    c("computer\nsimulation", "real\nlaboratory"),
                    c("with\nsubtitles", "without\nsubtitles"))

# Effect sizes. Based on thresholds of Cohen's d (Cohen, 1988)
es <- c(-.80, -.65, -.50, -.35, -.20, .20, .35, .50, .65, .80)


set.seed(823876) # seed for replicability


## HALFEYE PLOT - GROUPS ON Y-AXIS ############################################
                         # making a loop over
for (i1 in group_names) {# all group names (vignettes)
    for (i2 in es) {     # and all effect sizes
        # for nearly perfectly distributed empirical data
        data <- tibble(group1 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # lower than 50
                                                          50-((15*i2)/2), 
                                                          15), 
                                      0),
                       group2 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # higher than 50
                                                          50+((15*i2)/2), 
                                                          15), 
                                      0)) %>% 
            pivot_longer(1:2, names_to = "group", values_to = "testscore")
        
        # Halfeye - Groups as nominal y-axis
        ggplot(data, aes(x=testscore, y=group)) + 
                 stat_halfeye() + 
                 xlim(c(0, 100)) + 
                 scale_y_discrete(labels = i1) +
                 theme_ipsum() +
                 xlab("knowledge test score")  +
                 force_panelsizes(rows = unit(4, "in"),
                                  cols = unit(4, "in"))
        
        # save this plot
        ggsave(
            file = paste0("2_Materials/2_plots_study2_exported/yaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}

## HALFEYE PLOT - GROUPS ON X-AXIS ############################################
                         # making a loop over
for (i1 in group_names) {# all group names (vignettes)
    for (i2 in es) {     # and all effect sizes
        # for nearly perfectly distributed empirical data
        data <- tibble(group1 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # lower than 50
                                                          50-((15*i2)/2), 
                                                          15), 
                                      0),
                       group2 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # higher than 50
                                                          50+((15*i2)/2), 
                                                          15), 
                                      0)) %>% 
            pivot_longer(1:2, names_to = "group", values_to = "testscore")
        
        # Halfeye - Groups as nominal x-axis
        ggplot(data, aes(x=testscore, y=group)) + 
                stat_halfeye() + 
                xlim(c(0, 100)) + 
                scale_y_discrete(labels = i1) +
                theme_ipsum() +
                xlab("knowledge test score") +
                coord_flip()  +
                force_panelsizes(rows = unit(4, "in"),
                                 cols = unit(4, "in"))
        
        # save this plot
        ggsave(
            file = paste0("2_Materials/2_plots_study2_exported/xaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}



############################################################################# #
### Script for producing and saving the plots                   ############# #
### Condition: Benchmarks                                       ###############
###                                                             ############# #
### Original script from Samuel Merk                            ############# #
############################################################################# #


## basic characteristics of the data ##########################################
# generate list of group names
group_names <- list(c("reading on paper\nand highlighting text", "reading\non paper"),
                    c("online\nlesson", "video recorded\nlesson"),
                    c("virtual reality\nlaboratory", "real\nlaboratory"),
                    c("subtitles with\nhighlighted keywords", "without\nsubtitles"))

# Effect sizes. Based on thresholds of Cohen's d (Cohen, 1988)
es <- c(-.2, .2)


set.seed(823876) # seed for replicability


## HALFEYE PLOT - GROUPS ON Y-AXIS ############################################
# making a loop over
for (i1 in group_names) {# all group names (vignettes)
    for (i2 in es) {     # and all effect sizes
        # for nearly perfectly distributed empirical data
        data <- tibble::tibble(group1 = round(distribution_normal(309, 
                                                                  # mean = half an es 
                                                                  # lower than 50
                                                                  80-((15*i2)/2), 
                                                                  15), 
                                              0),
                               group2 = round(distribution_normal(309, 
                                                                  # mean = half an es 
                                                                  # higher than 50
                                                                  80+((15*i2)/2), 
                                                                  15), 
                                              0)) %>% 
            tidyr::pivot_longer(1:2, names_to = "group", values_to = "testscore")
        
        # Halfeye - Groups as nominal y-axis
        ggplot2::ggplot(data, ggplot2::aes(x=testscore, y=group)) + 
            stat_halfeye() + 
            ggplot2::xlim(c(30, 130)) + 
            ggplot2::scale_y_discrete(labels = i1) +
            hrbrthemes::theme_ipsum() +
            ggplot2::xlab("knowledge test score")  +
            ggh4x::force_panelsizes(rows = unit(4, "in"),
                                    cols = unit(4, "in")) +
            theme(axis.text = element_text(family = 'Arial'),
                  axis.title.x = element_text(family = 'Arial'),
                  axis.title.y = element_text(family = 'Arial'),
                  panel.grid = element_line(linetype=2))
        
        # save this plot
        ggplot2::ggsave(
            file = paste0("2_Materials/2_plots_study2_exported/yaxis_", 
                          stringr::str_remove(gsub(" ", "", i1[1]), "\n"), 
                          stringr::str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, "_benchmarks", ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}


## HALFEYE PLOT - GROUPS ON X-AXIS ############################################
# making a loop over
for (i1 in group_names) {# all group names (vignettes)
    for (i2 in es) {     # and all effect sizes
        # for nearly perfectly distributed empirical data
        data <- tibble::tibble(group1 = round(distribution_normal(309, 
                                                                  # mean = half an es 
                                                                  # lower than 50
                                                                  80-((15*i2)/2), 
                                                                  15), 
                                              0),
                               group2 = round(distribution_normal(309, 
                                                                  # mean = half an es 
                                                                  # higher than 50
                                                                  80+((15*i2)/2), 
                                                                  15), 
                                              0)) %>% 
            tidyr::pivot_longer(1:2, names_to = "group", values_to = "testscore")
        
        # Halfeye - Groups as nominal y-axis
        ggplot2::ggplot(data, ggplot2::aes(x=testscore, y=group)) + 
            stat_halfeye() + 
            ggplot2::xlim(c(30, 130)) + 
            ggplot2::scale_y_discrete(labels = i1) +
            hrbrthemes::theme_ipsum() +
            ggplot2::xlab("knowledge test score")  +
            coord_flip()  +
            ggh4x::force_panelsizes(rows = unit(4, "in"),
                                    cols = unit(4, "in")) +
            theme(axis.text = element_text(family = 'Arial'),
                  axis.title.x = element_text(family = 'Arial'),
                  axis.title.y = element_text(family = 'Arial'),
                  panel.grid = element_line(linetype=2))
        
        # save this plot
        ggplot2::ggsave(
            file = paste0("2_Materials/2_plots_study2_exported/xaxis_", 
                          stringr::str_remove(gsub(" ", "", i1[1]), "\n"), 
                          stringr::str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, "_benchmarks", ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}
