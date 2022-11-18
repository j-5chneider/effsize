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
        data <- tibble(group1 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # lower than 50
                                                          55-((15*i2)/2), 
                                                          15), 
                                      0),
                       group2 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # higher than 50
                                                          55+((15*i2)/2), 
                                                          15), 
                                      0)) %>% 
            pivot_longer(1:2, names_to = "group", values_to = "testscore")
        
        # Halfeye - Groups as nominal y-axis
        ggplot(data, aes(x=testscore, y=group)) + 
            stat_halfeye() + 
            xlim(c(5, 105)) + 
            scale_y_discrete(labels = i1) +
            theme_ipsum() +
            xlab("knowledge test score")  +
            force_panelsizes(rows = unit(4, "in"),
                             cols = unit(4, "in"))
        
        # save this plot
        ggsave(
            file = paste0("2_Materials/plots/halfeye_yaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_", i2, "_benchmarks", ".svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }
}
