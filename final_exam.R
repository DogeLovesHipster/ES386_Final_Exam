# Sergio Mendoza
# ES 386 Final Practicum
# 3/20/23
# emails: mendozas1@sou.edu || shadow41402@gmail.com
# Version 1.0
# Data can be found in Git repo

library(tidyverse)
library(visdat)
library(skimr)
library(dplyr)
library(ggstatsplot)
library(ggplot2)

# my data is named "tidePools"
# year (INT)
# plot_ID = unique (INT)
# study_site = 1-3 Sites (CHR)
# disturbance = Cleared, Control, Test (CHR)
# Start_spp_div = initial species diversity of each plot (INT)
# End_spp_div = Final species diversity of each plot (INT)
# percent_recovery = 10 initial, 8 end, 80% recovery (NUM/FLOAT)
# max_ISST = maximum intertidal sea surface temperature (INT)

# cleared = Completely cleared plots (5 plots)
# control = Control variables (5 plots)

tidePools <- read.csv("E:/ES 386 Labs/final_exam/tidepool.csv")
view(tidePools)

# Analyze Data Commands
str(tidePools)
glimpse(tidePools)
vis_dat(tidePools)

# Question 1
# dplyr and ggplot

# dplyr command to organize data and manipulate it and use for graphs
avg_recovery <- tidePools %>%
  group_by(disturbance) %>%
  summarize(avg_percent_recovery = mean(percent_recovery))

# ggplot for graphical visualizations (start and end tide pools)
ggplot(tidePools, aes(x = Start_spp_div, y = End_spp_div)) +
  geom_point()

# ggplot for graphical visualizations (avg_recovery of cleared and control)
ggplot(avg_recovery, aes(x = disturbance, y = avg_percent_recovery)) +
    geom_col(fill = "#0099f9")

# Question 2
# ANOVA, TukeyHSD, ggplot for boxplots

# Anova test with summary results
aov_model <- aov(percent_recovery ~ study_site + disturbance + study_site*disturbance, data=tidePools)
summary(aov_model)

# TukeyHSD to interpret p-values of adjacent sites
tukey_result <- TukeyHSD(aov_model, "study_site")
tukey_result

# ggplot for graphical visualization (control vs cleared recovery relationship)
ggplot(tidePools, aes(x=study_site, y=percent_recovery, fill=disturbance)) +
  geom_boxplot() +
  labs(x="Study Site", y="Percent Recovery", fill="Disturbance")

# Question 3
# Pearson Correlation test and ggplot for vertical histogram

# Pearson correlation test
cor.test(tidePools$max_ISST, tidePools$percent_recovery, method = "pearson")

# ggplot for graphical visualization (verical scatterplot for sea surface temp)
ggplot(tidePools, aes(x = max_ISST, y = percent_recovery)) +
  geom_point() +
  labs(x = "Maximum Intertidal Sea Surface Temperature", y = "Percent Recovery")
