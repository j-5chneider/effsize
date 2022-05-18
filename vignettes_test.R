tmp <- rio::import("C:/Users/Juergen Schneider/Downloads/Results_1003.csv")

library(tidyverse)
# library(EnvStats)
glimpse(tmp)

tmp <- tmp %>%
    mutate(across(4:9, ~ scale(.x)[,1])) %>%
    dplyr::select(3:9) %>%
    pivot_longer(2:7, names_to = "Feature_Name", values_to = "Value")

ggplot(tmp, aes(x=as.factor(Feature_Name), y=Value, color = Text_title, group = Text_title)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 25, hjust=1))

# varTest(scale(tmp$`Mean Sentence Length in Letters`, scale = F)[,1], 
#         alternative = "greater",
#         sigma.squared = 0.000000000000000000000000000000000000001)
# 
# ggplot(tmp, aes(x=Text_title, y=`Mean Token Length in Letters`)) +
#   geom_point()
# 
# ggplot(tmp, aes(x=Text_title, y=`Lexical Richness: Type Token Ratio (Root TTR)`)) +
#   geom_point()
# 
# ggplot(tmp, aes(x=Text_title, y=`Lexical Variation Feature: Modifier`)) +
#   geom_point()
# 
# ggplot(tmp, aes(x=Text_title, y=`Syntactic Complexity Feature: Dependent clause ratio`)) +
#   geom_point()
# 
# ggplot(tmp, aes(x=Text_title, y=`Syntactic Complexity Feature: Complex Nominals per Clause`)) +
#   geom_point()
