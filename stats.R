# Krill stats
setwd("~/placement/alfred-wegener/Bettina Meyer/data")
source("prerequisites.R")
# So what statistical tests can I do to this data? I have lipid content (continuous), and months (categorical). I can compare the means of each Jul/Aug, to see if they differ significantly - that would be a one-way ANOVA.

# 1-WAY ANOVA testing if there is a significant difference between Aug each year ####
# Assumptions: the observations are independent, the data are normally distributed (shapiro test), and the data has homogeneity of variance (levene's test)
shapiro.test(seasonalLipids$Lipids) # p-val < 0.05, non-normal
leveneTest(Lipids~Year,seasonalLipids) # p-val NAN, that's surely not a good thing
seasonalLipids$logLipids = log10(seasonalLipids$Lipids) # Transform the data
shapiro.test(seasonalLipids$logLipids) # p-val > 0.05, normal
leveneTest(logLipids~Year,seasonalLipids) # p-val <<< 0.05, non-homogeneous

# The variables still don't meet all the assumptions. That's unfortunate, but not the end of the world. I can do a non-parametric ANOVA! I think that's a Kruskal-Wallis test

# Kruskal-Wallis test ####
kruskal.test(Lipids~Year,seasonalLipids) # p-val > 0.05. No significant difference between mean lipid content of years
kruskal.test(Total~Year, seasonalLipids)