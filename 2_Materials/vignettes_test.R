tmp <- rio::import("data/features_of_vignettes_wide_en.csv")

library(tidyverse)
library(ggpubr)

glimpse(tmp)

plot1 <- tmp %>%
    dplyr::select(3:8) %>%
    pivot_longer(2:6, names_to = "Feature_Name", values_to = "Value") %>%
    mutate(Feature_Name = factor(Feature_Name, levels = c("Mean Sentence Length in Tokens",
                                                          "Lexical Richness: Type Token Ratio (Root TTR)",
                                                          "Mean Token Length in Syllables",
                                                          "Syntactic Complexity Feature: Complex Nominals per Clause",
                                                          "Lexical Variation Feature: Modifier")))


plot2 <- tmp %>%
    mutate(across(4:8, ~ scale(.x)[,1])) %>%
    dplyr::select(3:8) %>%
    pivot_longer(2:6, names_to = "Feature_Name", values_to = "Value") %>%
    mutate(Feature_Name = factor(Feature_Name, levels = c("Mean Sentence Length in Tokens",
                                                          "Lexical Richness: Type Token Ratio (Root TTR)",
                                                          "Mean Token Length in Syllables",
                                                          "Syntactic Complexity Feature: Complex Nominals per Clause",
                                                          "Lexical Variation Feature: Modifier")))

p1 <- ggplot(plot1, aes(x=as.factor(Feature_Name), y=Value, color = Text_title, group = Text_title)) +
          geom_point() +
          geom_line() +
          scale_x_discrete(labels =  c("Mean Sentence \nLength in Tokens",
                                       "Lexical Richness: Type \nToken Ratio (Root TTR)",
                                       "Mean Token Length in Syllables",
                                       "Syntactic Complexity Feature: \nComplex Nominals per Clause",
                                       "Lexical Variation Feature: Modifier")) +
          theme(axis.text.x = element_text(angle = 25, hjust=1))

p2 <- ggplot(plot2, aes(x=as.factor(Feature_Name), y=Value, color = Text_title, group = Text_title)) +
          geom_point() +
          geom_line() +
          scale_x_discrete(labels =  c("Mean Sentence \nLength in Tokens",
                                       "Lexical Richness: Type \nToken Ratio (Root TTR)",
                                       "Mean Token Length in Syllables",
                                       "Syntactic Complexity Feature: \nComplex Nominals per Clause",
                                       "Lexical Variation Feature: Modifier")) +
          theme(axis.text.x = element_text(angle = 25, hjust=1))

ggarrange(p1, p2,
          ncol = 2, 
          labels = c("A", "B")) 

ggarrange(p1, p2,
          ncol = 1, 
          nrow = 2
          ) 


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
