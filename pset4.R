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
# 31at endpoint, or 2016, co2 = 1.520 * 58 + 3.070e+02 = 395.16
co2[1]
co2[length(co2)]

#(c)
summary(lmco2Time)

#(d)
pred_co2 <- predict(lmco2Time)
plot(time,co2,"l")
lines(time,pred_co2)

#(e)
time2 <- time^2
lm2co2Time <- lm(co2~time+time2)
summary(lm2co2Time)
# From the data, co2 = 7.879e-01 * time + 1.245e-02 * time2 + 3.142e+02
# Therefore, at startpoint, or 1958, co2 = 7.879e-01 * 0 + 1.245e-02 * 0 + 3.142e+02 = 314.2
# at endpoint, or 2016, co2 = 7.8979e-01 * 58 + 1.245e-02 * 58 * 58 + 3.142e+02 = 401.8896

#(f)
pred2_co2 <- predict(lm2co2Time)
startYearDiff = pred2_co2[13] - pred2_co2[1]
endYearDiff = pred2_co2[702] - pred2_co2[690]

#(g)
summary(lm2co2Time)

#(h)
plot(time,co2,"l")
lines(time,pred2_co2)
title(main="Mauna Loa Carbon Dioxide (ppm)")

#(i)
time3 <- time^3
lm3_co2 <- lm(co2~time+time2+time3)
summary(lm3_co2)

#Problem 2: Fitting Trends and Cycles Over the Entire Data Series
pi2 <- 2*pi
ann_s <- sin(pi2*time)
ann_c <- cos(pi2*time)
lm2a_co2 <- lm(co2 ~ time + time2 + ann_s + ann_c)
summary(lm2a_co2)
#(a)
coef_s <- coef(summary(lm2a_co2))["ann_s", "Estimate"]
coef_c <- coef(summary(lm2a_co2))["ann_c", "Estimate"]
coef_ann <- sqrt(coef_s^2 + coef_c^2)
dcoef_s <- coef(summary(lm2a_co2))["ann_s", "Std. Error"]
dcoef_c <- coef(summary(lm2a_co2))["ann_c", "Std. Error"]
dcoef_ann <- sqrt(dcoef_s^2 + dcoef_c^2)


#(b)
phaseD = atan2(coef_s,coef_c)/(2*pi)

#(c)
resid <- co2 - pred2_co2
pred_co2A <- predict(lm2a_co2)
resid2 <- co2 - pred_co2A
plot(resid,resid2, 
     xlab="Quadratic model residuals", 
     ylab="Quadratic and annual model residuals",
     "l")
title(main = "Residuals of quadratic with quadratic and annual cycle")

#(d)
pi4 <- 4*pi
ann2_s <- sin(pi4*time)
ann2_c <- cos(pi4*time)
lm2a2_co2 <- lm(co2 ~ time + time2 + ann_s + ann_c + ann2_s + ann2_c)
coef2_s <- coef(summary(lm2a2_co2))["ann2_s", "Estimate"]
coef2_c <- coef(summary(lm2a2_co2))["ann2_c", "Estimate"]
coef_semiAnn <- sqrt(coef2_s^2 + coef2_c^2)
dcoef2_s <- coef(summary(lm2a2_co2))["ann2_s", "Std. Error"]
dcoef2_c <- coef(summary(lm2a2_co2))["ann2_c", "Std. Error"]
dcoef_semiAnn <- sqrt(dcoef2_s^2 + dcoef2_c^2)
pred_co2SA <- predict(lm2a2_co2)


#(e)
plot(time,pred_co2SA,"l")

