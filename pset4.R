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
plot(time,pred_co2A,"l")
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
plot(time,pred_co2SA,"l")

#(e)
resid_sa <- co2 - pred_co2SA
SDresid_sa <- sd(resid_sa)
plot(time,resid_sa,
     xlab="Time in years",
     ylab="Residual from quadratic+annual+semi-annual cycle model",
     "l")

#Question 3
#(a)
ann1 <- 1:7 
ann1_err <- 1:7 
yr <- 1:7 
for (j in c(1,98,195,292,389,486,583)){
  k=k+1 
  timed <- time[j:(j+119)] 
  time2d <- timed^2 
  co2d <- co2[j:(j+119)] 
  ann_sd <- ann_s[j:(j+119)] 
  ann_cd <- ann_c[j:(j+119)] 
  ann2_sd <- ann2_s[j:(j+119)] 
  ann2_cd <- ann2_c[j:(j+119)]
  lmlmlm <- lm(co2d~timed+time2d+ann_sd+ann_cd+ann2_sd+ann2_cd)
  summary(lmlmlm)
  anns<-coef(summary(lmlmlm))["ann_sd","Estimate"]
  annc<-coef(summary(lmlmlm))["ann_cd","Estimate"]
  ann1[k] <- sqrt(anns^2+annc^2) 
  yr[k] <- year[j+60]
  anns <- coef(summary(lmlmlm))["ann_sd","Std. Error"]
  annc <- coef(summary(lmlmlm))["ann_cd","Std. Error"]
  ann1_err[k] <- sqrt(anns^2+annc^2)
}

qplot(yr,ann1)+
geom_errorbar(aes(x=yr,ymin=ann1-ann1_err,ymax=ann1+ann1_err),width=2)

#(b)
ann_phase <- 1:7
ann_pherr <- 1:7
k=0
yr <- 1:7
for (j in c(1,98,195,292,389,486,583)){
  k=k+1 
  timed <- time[j:(j+119)] 
  time2d <- timed^2 
  co2d <- co2[j:(j+119)] 
  ann_sd <- ann_s[j:(j+119)] 
  ann_cd <- ann_c[j:(j+119)] 
  ann2_sd <- ann2_s[j:(j+119)] 
  ann2_cd <- ann2_c[j:(j+119)]
  lmlmlm <- lm(co2d~timed+time2d+ann_sd+ann_cd+ann2_sd+ann2_cd)
  summary(lmlmlm)
  anns<-coef(summary(lmlmlm))["ann_sd","Estimate"]
  annc<-coef(summary(lmlmlm))["ann_cd","Estimate"]
  ann_amp <- sqrt(anns^2 + annc^2)
  ann_phase[k] <- 180*atan2(anns,annc)/pi
  yr[k] <- year[j+60]
  anns <- coef(summary(lmlmlm))["ann_sd","Std. Error"]
  annc <- coef(summary(lmlmlm))["ann_cd","Std. Error"]
  ann_pherr[k] <- (180/pi)*sqrt(anns^2+annc^2)/ann_amp
}
qplot(yr,ann_phase)+
  geom_errorbar(aes(x=yr,ymin=ann_phase-ann_pherr,ymax=ann_phase+ann_pherr),width=2)

#(c)

sann1 <- 1:7 
sann1_err <- 1:7 
yr <- 1:7 
k=0
for (j in c(1,98,195,292,389,486,583)){
  k=k+1 
  timed <- time[j:(j+119)] 
  time2d <- timed^2 
  co2d <- co2[j:(j+119)] 
  ann_sd <- ann_s[j:(j+119)] 
  ann_cd <- ann_c[j:(j+119)] 
  ann2_sd <- ann2_s[j:(j+119)] 
  ann2_cd <- ann2_c[j:(j+119)]
  lmlmlm <- lm(co2d~timed+time2d+ann_sd+ann_cd+ann2_sd+ann2_cd)
  summary(lmlmlm)
  sanns<-coef(summary(lmlmlm))["ann2_sd","Estimate"]
  sannc<-coef(summary(lmlmlm))["ann2_cd","Estimate"]
  sann1[k] <- sqrt(sanns^2+sannc^2) 
  yr[k] <- year[j+60]
  sanns <- coef(summary(lmlmlm))["ann2_sd","Std. Error"]
  sannc <- coef(summary(lmlmlm))["ann2_cd","Std. Error"]
  sann1_err[k] <- sqrt(sanns^2+sannc^2)
}

qplot(yr,sann1)+
  geom_errorbar(aes(x=yr,ymin=sann1-sann1_err,ymax=sann1+sann1_err),width=2)

#Question 4
#(a)
lmlogExp <- lm(log(co2)~time)
pred_logExp <- predict(lmlogExp)
plot(time,log(co2),"l")
lines(time,pred_logExp,col="red")
#(b)
pred_exp <- exp(pred_logExp)
plot(time,co2,"l")
lines(time,pred_exp,col="red")

#(c)
st <- coef(nls(log(co2) ~ log(a+b*exp(c*time)) ,start=list(a=0,b=5.73,c=0.004)))

astart = st[1]
bstart = st[2]
cstart = st[3]
nlm_co2 <- nls(co2 ~ a + b*exp(c*time),start=list(a=astart,b=bstart,c=cstart))

predexp <- predict(nlm_co2)
plot(year,co2,"l")
title(main="Mauna Loa Carbon Dioxide(ppm)")
lines(year,predexp,col="red")
#(d)
sdfrom = (280-256.9)/2.542
extrapolate = 256.9+56.61*exp(0.01627*-170)
