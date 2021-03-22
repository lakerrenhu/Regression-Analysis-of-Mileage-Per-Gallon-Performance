Auto = read.csv("E:/Desk/STA5104/Assignment2oral/autompg-dataset/auto-mpg.csv", header=T, na.strings="?")
dim(Auto)
[1] 398   9
Auto1 = na.omit(Auto)   # 6 samples are deleted
dim(Auto1)
[1] 392   9
summary(Auto1)          # statistical descriptions


library(dplyr)
Auto2=select(Auto1,-car.name)

#Correlation within numerical variables
pairs(Auto2)
cor(Auto2)

#variable selection 
###### best subset selection 
library(leaps)
regfit.full=regsubsets(mpg~., data=Auto2,nvmax=7,method="backward")
reg.summary= summary(regfit.full)
names(reg.summary)
# best model according to cp,BIC, adjusted Rsq
which.min(reg.summary$cp)  #6
which.min(reg.summary$bic)  #3
which.max(reg.summary$adjr2)  #6
# plot cp,BIC and adjusted Rsq
par(mfrow=c(1,3))
# cp
plot(reg.summary$cp,xlab="subset size",ylab="cp",type="l")
points(6,reg.summary$cp[6],col="red",cex=2,pch=20)
# BIC
plot(reg.summary$bic,xlab="subset size",ylab="BIC",type="l")
points(3,reg.summary$bic[3],col="red",cex=2,pch=20)
# adjr2
plot(reg.summary$adjr2,xlab="subset size",ylab="Adjusted R2",type="l")
points(6,reg.summary$adjr2[6],col="red",cex=2,pch=20)
# coefficient of 14 variable model
coef(regfit.full,6)
  (Intercept)     cylinders  displacement    horsepower        weight 
-15.563492306  -0.506685137   0.019269286  -0.023895029  -0.006218311 
   model.year        origin 
  0.747515952   1.428241885 
coef(regfit.full,3)
  (Intercept)        weight    model.year        origin 
-18.045850149  -0.005994118   0.757126111   1.150390789 

# fit the model
lm.fit1 = lm(mpg~.-acceleration, data=Auto2)
summary(lm.fit1)

lm.fit2=  lm(mpg~weight+model.year+origin, data=Auto2)
summary(lm.fit2)
# VIF 
library(car) 
vif(lm.fit1)
 cylinders displacement   horsepower       weight   model.year       origin 
   10.710150    21.608513     6.147752     8.324047     1.237304     1.772234 
vif(lm.fit2)
    weight  model.year     origin 
  1.625522   1.105651   1.520292 

#if without selection for features or variables
lm.fit0=lm(mpg~.-car.name, data=Auto1)
summary(lm.fit0)
vif(lm.fit0)
#log(mpg) fit
lm.fit_log=lm(log(mpg)~.-car.name, data=Auto1)
summary(lm.fit_log)
vif(lm.fit_log)

# the model with weight, model.year and origin
lim.fitbest=lm(mpg~model.year+origin, data=Auto2)
summary(lim.fitbest)
