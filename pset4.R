library(ggplot2)
MaunaLoa <- read.csv("./co2_maunaloa.csv")
attach(MaunaLoa)
names(MaunaLoa)
year <- MaunaLoa$YEAR
co2 <- MaunaLoa$CO2
plot(year,co2,"l")
abline(lm(co2~year))
title(main="Mauna Loa Carbon Dioxide (ppm)")
time <- year-1958


#Problem 1: Fitting Trends Over the Entire Data Series
#(a)
lmco2Time <- lm(co2~time)
lmco2Year <- lm(co2~year)
