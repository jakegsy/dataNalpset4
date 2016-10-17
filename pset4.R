library(ggplot2)
MaunaLoa <- read.csv("./co2_maunaloa.csv")
attach(MaunaLoa)
names(MaunaLoa)
year <- MaunaLoa$YEAR
co2 <- MaunaLoa$CO2
time <- year-1958


#Problem 1: Fitting Trends Over the Entire Data Series
#(a)
plot(time,co2,"l")
lmco2Time <- lm(co2~time)
abline(lmco2Time)
title(main="Mauna Loa Carbon Dioxide (ppm)")
summary(lmco2Time)


#(b)
# From the data, co2 = 1.520 * time + 3.070e2
# Therefore at startpoint, or 1958, co2 = 1.520 * 0 + 3.070e+02 = 307
#           at endpoint, or 2016, co2 = 1.520 * 58 + 3.070e+02 = 395.16
co2[1]
co2[length(co2)]

#(c)
summary(lmco2Time)

#(d)
pred_co2 <- predict(lmco2Time)
plot(time,co2,"l")
lines(time,pred_co2)
