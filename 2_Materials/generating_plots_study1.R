############################################################################# #
### Script for producing and saving the plots                   ############# #
### Original script from Samuel Merk                            ############# #
############################################################################# #


library(bayestestR) 
library(tidyverse)
library(hrbrthemes)
library(ggdist)
library(dabestr)
library(ggh4x)
library(egg)
library(grid)

## basic characteristics of the data ##########################################
# generate list of group names
group_names <- list(c("reading\non tablet", "reading\non paper"),
                    c("live\nlesson", "video recorded\nlesson"),
                    c("computer\nsimulation", "real\nlaboratory"),
                    c("with\nsubtitles", "without\nsubtitles"))

# Effect sizes. Based on thresholds of Cohen's d (Cohen, 1988)
es <- c(-.8, -.5, -.2, .2, .5, .8)


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
            file = paste0("2_Materials/1_plots_study1/halfeye_yaxis_", 
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
            file = paste0("2_Materials/1_plots_study1/halfeye_xaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}



## RAINCLOUD PLOT - GROUPS ON Y-AXIS ##########################################
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
        
        # Raincloud Plot - Groups as nominal y-axis
        ggplot(data, aes(x=group, y=testscore)) + 
            ggdist::stat_halfeye(
                adjust = .5, 
                width = .6, 
                .width = 0, 
                justification = -.3, 
                point_colour = NA) + 
            geom_point(
                size = 1.3,
                alpha = .3,
                position = position_jitter(
                    seed = 1, width = .1
                )
            ) + 
            ylim(c(0, 100)) + 
            scale_x_discrete(labels = i1) +
            theme_ipsum() +
            ylab("knowledge test score") +
            coord_flip() +
            force_panelsizes(rows = unit(4, "in"),
                             cols = unit(4, "in"))
        
        # save this plot
        ggsave(
            file = paste0("2_Materials/1_plots_study1/raincloud_yaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}




## GARDNER-ALTMAN PLOT - GROUPS ON X-AXIS #####################################
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
            pivot_longer(1:2, names_to = "group", values_to = "testscore") %>%
            # change group1 and group2 to group names
            mutate(group = case_when(
                                group == "group1" ~ i1[1],
                                group == "group2" ~ i1[2]),
                   # replace \n with space to align plots later on
                   group = gsub("\n", " ", group))
        
        # Gardner-Altman Plot - Groups on x-axis
        unpaired_mean_diff <-
            dabest(
                data,
                group,
                testscore,
                idx = c(gsub("\n", " ", i1[1]), gsub("\n", " ", i1[2])),
                paired = FALSE
            ) %>%
            mean_diff()
        
        # plot results
        p <- plot(unpaired_mean_diff)
        p <- set_panel_size(p,
                            width  = unit(4, "in"),
                            height = unit(4, "in"))
        grid.newpage()
        grid.draw(p1)
        
        
        # save this plot
        ggsave(
            file = paste0("2_Materials/1_plots_study1/gardneraltman_xaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}
