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

############################################################################# #
########  CHECK COMPREHENSION OF DISPERSION       #############################
############################################################################# #

## basic characteristics of the data ##########################################
# generate list of group names
group_names <- list(c("reading\non tablet", "reading\non paper"),
                    c("live\nlesson", "video recorded\nlesson"),
                    c("computer\nsimulation", "real\nlaboratory"),
                    c("with\nsubtitles", "without\nsubtitles"))

# Effect sizes = 0 as we focus on dispersion
es <- 0


set.seed(823876) # seed for replicability


## HALFEYE PLOT - GROUPS ON Y-AXIS ############################################
for (i1 in group_names) {# all group names (vignettes)
        # for nearly perfectly distributed empirical data
        data <- tibble(group1 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # lower than 50
                                                          50-((15*es)/2), 
                                                          5), 
                                      0),
                       group2 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # higher than 50
                                                          50+((15*es)/2), 
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
            file = paste0("2_Materials/7_treatment-checks/halfeye_yaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_treatmentCheck-dispersion.svg"),
            width = 6,
            height = 6,
            device = "svg"
            )
        }

## HALFEYE PLOT - GROUPS ON X-AXIS ############################################
                         # making a loop over
    for (i1 in group_names) {     # and all effect sizes
        # for nearly perfectly distributed empirical data
        data <- tibble(group1 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # lower than 50
                                                          50-((15*es)/2), 
                                                          5), 
                                      0),
                       group2 = round(distribution_normal(309, 
                                                          # mean = half an es 
                                                          # higher than 50
                                                          50+((15*es)/2), 
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
            file = paste0("2_Materials/7_treatment-checks/halfeye_xaxis_", 
                          str_remove(gsub(" ", "", i1[1]), "\n"), 
                          str_remove(gsub(" ", "", i1[2]), "\n"), 
                          "_treatmentCheck-dispersion.svg"),
            width = 6,
            height = 6,
            device = "svg"
        )
    }



############################################################################# #
########  CHECK AXIS COMPREHENSION                #############################
############################################################################# #

## HALFEYE PLOT - GROUPS ON Y-AXIS ############################################
# for nearly perfectly distributed empirical data
data <- tibble(testscore = round(distribution_normal(309, 
                                                     # mean = half an es 
                                                     # lower than 50
                                                     50-((15*es)/2), 
                                                     15), 
                                 0),
               group = as.factor(1))

# Halfeye - Groups as nominal y-axis
ggplot(data, aes(x=testscore, y=group)) + 
    stat_halfeye() + 
    xlim(c(0, 100)) + 
    scale_y_discrete(labels = "") +
    theme_ipsum() +
    xlab("knowledge test score") +
    ylab("") +
    force_panelsizes(rows = unit(4, "in"),
                     cols = unit(4, "in"))

# save this plot
ggsave(
    file = paste0("2_Materials/7_treatment-checks/halfeye_yaxis_", 
                  "_treatmentCheck-axis.svg"),
    width = 6,
    height = 6,
    device = "svg"
)


## HALFEYE PLOT - GROUPS ON X-AXIS ############################################
# for nearly perfectly distributed empirical data
data <- tibble(testscore = round(distribution_normal(309, 
                                                     # mean = half an es 
                                                     # lower than 50
                                                     50-((15*es)/2), 
                                                     15), 
                                 0),
               group = as.factor(1))

# Halfeye - Groups as nominal x-axis
ggplot(data, aes(x=testscore, y=group)) + 
    stat_halfeye() + 
    xlim(c(0, 100)) + 
    scale_y_discrete(labels = "") +
    theme_ipsum() +
    xlab("knowledge test score") +
    ylab("") +
    coord_flip()  +
    force_panelsizes(rows = unit(4, "in"),
                     cols = unit(4, "in"))

# save this plot
ggsave(
    file = paste0("2_Materials/7_treatment-checks/halfeye_xaxis_", 
                  "_treatmentCheck-axis.svg"),
    width = 6,
    height = 6,
    device = "svg"
)



############################################################################# #
########                  #############################
############################################################################# #