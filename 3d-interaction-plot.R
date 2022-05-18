library(tidyverse)
library(plotly)


############### GENERATING DATA ######
df1x <- c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8)   # simulate from the normal distribution
df1y <- df1x^2 +  (2*df1x)/3                 # make it squared +/- a little bit

# generate the multivariate normal distribution
df1 <- data.frame(v1 = df1x, 
                  v2 = df1y,
                  v3 = "positive Effect")

names(df1) <- c("trueES", "sensitivity", "anticipatedES")




##### SIMULATE DATA FOR NO EFFECT
df2x <- c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8)   # simulate from the normal distribution
df2y <- df2x^2                              # make it squared +/- a little bit

# generate the multivariate normal distribution
df2 <- data.frame(v1 = df2x, 
                  v2 = df2y,
                  v3 = "no Effect")

names(df2) <- c("trueES", "sensitivity", "anticipatedES")




##### SIMULATE DATA FOR NEGATIVE EFFECT
df3x <- c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8)   # simulate from the normal distribution
df3y <- df3x^2 -  (2*df3x)/3                 # make it squared +/- a little bit

# generate the multivariate normal distribution
df3 <- data.frame(v1 = df3x, 
                  v2 = df3y,
                  v3 = "negative Effect")

names(df3) <- c("trueES", "sensitivity", "anticipatedES")




##### MERGE DATA AND PLOT
plot_df <- rbind(df1, df2, df3)


plot_ly(
    data = plot_df,
    x = ~ trueES, 
    y = ~ anticipatedES, 
    z = ~ sensitivity,
    color = ~ anticipatedES,
    type = "scatter3d",
    mode = "markers") %>%
    add_trace(x = ~ trueES, 
              y = ~ anticipatedES, 
              z = ~ sensitivity, 
              color = ~ anticipatedES, 
              line = list(width = 6))




######################################################### #
############### SURFACE PLOT ##############################
######################################################### #

# plot_df$trueES2 <- plot_df$trueES^2
# plot_df$pred <- as.numeric(NA)
# 
# for (anticipatedESi in c("positive Effect", "no Effect", "negative Effect")) {
#     fit <- lm(sensitivity ~ trueES + trueES2 , data=plot_df[plot_df$anticipatedES == anticipatedESi,])
#     
#     for (trueESi in c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8)) {
#         
#         plot_df$pred[plot_df$anticipatedES == anticipatedESi] <- predict(fit)
#     }
# }
# 
# 
# 
# 
# plot_ly(
#     data = plot_df,
#     x = ~ trueES, 
#     y = ~ anticipatedES, 
#     z = ~ predict,
#     color = ~ anticipatedES,
#     type = "scatter3d",
#     mode = "markers") %>%
#     add_trace(x = ~ trueES, 
#               y = ~ anticipatedES, 
#               z = ~ sensitivity, 
#               color = ~ anticipatedES, 
#               line = list(width = 6))


plot_ly() %>% 
    add_trace(data = plot_df,  
              x= ~trueES, y= ~anticipatedES, z= ~sensitivity, 
              type="mesh3d"#,
              # intensity = c(0, 0.33, 0.66, 1),
              # color = c(0, 0.33, 0.66, 1),
              # colors = colorRamp(c("red", "green", "blue"))
              ) 
