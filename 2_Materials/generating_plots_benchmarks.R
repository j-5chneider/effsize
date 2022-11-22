############################################################################# #
### Script for producing and saving the benchmark plots         ############# #
### Original script from Samuel Merk                            ############# #
############################################################################# #


library(bayestestR) 
library(tidyverse)
library(hrbrthemes)
library(ggdist)
# library(dabestr)
library(ggh4x)
# library(egg)
library(grid)

## basic characteristics of the data ##########################################
# generate list of group names
group_names <- list(c("reading\non tablet", "reading\non paper"),
                    c("live\nlesson", "video recorded\nlesson"),
                    c("computer\nsimulation", "real\nlaboratory"),
                    c("with\nsubtitles", "without\nsubtitles"))

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
            file = paste0("2_Materials/plots/halfeye_yaxis_", 
                          stringr::str_remove(gsub(" ", "", i1[1]), "\n"), 
                          stringr::str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, "_benchmarks", ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}
