##------------- SOME PRELIMINARY EXPLORATION ----------------
try1 = lm(TARGDOL~CNDOL1+CNMON1+CNMONL+CONLARG+CNTMLIF+CNTRLIF, data = quant_learn)
summary(try1)

try2 = lm(TARGDOL~CNDOL1+CNMONL+CONLARG+CNTMLIF+CNTRLIF+relevel(SEX, 'F'), data = quant_learn[2:18139, ])
summary(try2)

count = summary(quant_test$STATCODE)
testMean = aggregate(quant_test[c('TARGDOL', 'CONLARG', 'CNMON1')], list(quant_test$STATCODE), mean)
testMean$state = paste0('US-', testMean$Group.1)
testMean$donorBase = count[count>0]

# mapping 
GeoStates3 <- gvisGeoChart(testMean[c(1:11, 13:53), ], "state", "TARGDOL", options=list(region="US", displayMode="regions", resolution="provinces", width=600, height=400))
plot(GeoStates3)

##------------- MODEL (I): For People Who Donated Only Once -------------
require(car)
once = quant_learn[quant_learn$CNTMLIF==1, ]
quant_test_once = subset(quant_test, CNTMLIF==1)

###----- [i.] Multivariate Linear Models -----
# on full set of training data
fit_lin = lm(TARGDOL~CNDOL1+log(CNDOL1)+CNMON1+SEX+region, data=once)
summary(fit_lin)
step(fit_lin)
plot(fit_lin, which = 1)
ha = boxCox(once_lin, lambda = seq(-1, 1, .1))
text(.5, -55500, expression(lambda[opt] == 1/9))

# on filtered set of training data
once_lin = lm(TARGDOL~log(CNDOL1)+CNMON1+SEX+incZone+region,data=once,subset=c(6:16, 18:8990))
summary(once_lin)
step(once_lin)
plot(once_lin, which = 2)
avPlots(once_lin, 'log(CNDOL1)')
crPlots(once_lin)

# Box-Cox transform TARGDOL and then regress
lam = 1/9
once$tar = (once$TARGDOL^lam - 1)/lam
fit_bc = lm(tar~CNDOL1+log(CNDOL1)+CNMON1+SEX+region, data=once)
summary(fit_bc)
plot(fit_bc, which = 1)
crPlots(fit_bc)

# calculate Cook's dist. & remove influential obs.
d1 = cooks.distance(fit_bc)
conciseOnce = once[which(d1<.005), ]    # left out 7
fit_bcCon = lm(tar~CNDOL1+log(CNDOL1)+CNMON1+SEX+region, data=conciseOnce)
summary(fit_bcCon)
plot(fit_bcCon, which = 1)
crPlots(fit_bcCon)

# compare Fitted vs. Actual for models fit_bc & fit_bcCon
# fit_bc:
val_bc = (fit_bc$fitted.values * lam + 1)^(1/lam)
plot(once$TARGDOL, val_bc, xlim=c(0, 100), ylim = c(0,100))
abline(a=0, b=1)
mean((once$TARGDOL - val_bc)^2)   # ~ mse on once

# fit_bcCon:
val_bcCon = (fit_bcCon$fitted.values * lam + 1)^(1/lam)
plot(conciseOnce$TARGDOL, val_bcCon, xlim=c(0, 100), ylim = c(0,100))
abline(a=0, b=1)
mean((conciseOnce$TARGDOL - val_bcCon)^2)   # ~ mse on conciseOnce
pred = (predict(fit_bcCon, newdata = once) * lam + 1)^(1/lam)
mean((once$TARGDOL - pred)^2)
plot(pred, once$TARGDOL-pred)

###----- [ii.] Try GLM with Poisson Family -----
once$tar2 = round(once$TARGDOL)
fit_pois = glm(tar2~log(CNDOL1)+CNMON1+SEX+region, data=once[which(d1<.04),], family = poisson)
summary(fit_pois)
plot(fit_pois, which = 1)
d2 = cooks.distance(fit_pois)
mean((once$TARGDOL - predict(fit_pois, type = 'response'))^2)   # ~ mse
plot(once$TARGDOL, predict(fit_pois, newdata = once, type = 'response'), xlim = c(0,100), ylim = c(0,100))
crPlots(fit_pois)
plot(once$TARGDOL, once$TARGDOL-predict(fit_pois, newdata = once, type = 'response'))
     
###----- [iii.] Test On Validation Set -----

# for fit_bc --- Error = 22.4173 or 4.7347
pred_bc = (predict(fit_bc, newdata = test_once) *lam + 1)^(1/lam)
mean((test_once$TARGDOL - pred_bc)^2) 

# for fit_bcCon --- Error = 22.3260
pred_bcCon = (predict(fit_bcCon, newdata = test_once) *lam + 1)^(1/lam)
mean((test_once$TARGDOL - pred_bcCon)^2)

# for fit_pois --- Error = 21.32912 or 4.6183
mean((quant_test_once$TARGDOL-predict(once_pois, newdata = quant_test_once, type='response'))^2)


##------------- MODEL (II): For People Who Donated Exactly Twice -------------
twice = quant_learn[quant_learn$CNTMLIF==2, ]
quant_test_twice = subset(quant_test, CNTMLIF==2)

###----- [i.] Multivariate Linear Models -----
twi_lin = lm(TARGDOL~CNDOL1+I(CNDOL2^2)+SEX+region+solTime+cnc1+cnc2+CNMON1+I(CNMON1^2)+CNMON2+incZone, data = twice)
summary(twi_lin)
step(twi_lin)
twi_lin = lm(TARGDOL~CNDOL1:cnc1+CNDOL2:cnc2+I(CONLARG^2)+region+solTime, data = twice, subset = c(2, 4:60, 62:1470))
plot(twi_lin, which = 4)
ha = boxCox(twi_lin, lambda = seq(-1, 1, .1))

d2 = cooks.distance(twi_lin)
conc2 = twice[which(d2<1), ]    # filtering out

# a more parsimonious model
twi_linless = lm(TARGDOL~CNDOL1:cnc1+I(CNDOL2^2)+CNMON1+I(CNMON1^2), data = conc2)
summary(twi_linless)
plot(twi_linless, which=1)

###----- [ii.] GLM with the Poisson family -----
twice$tar = round(twice$TARGDOL)
twi_pois = glm(tar~CNDOL1:cnc1+CNDOL2:cnc2+CNMON1+CNMON2+SEX+region+incZone+solTime+sol2, family = poisson, data = twice, subset = c(3:316, 318:1470))
summary(twi_pois)
plot(twi_pois, which = 1)
avPlots(twi_pois)

###----- [iii.] Test On Validation Set -----

# for twi_lin --- Error = $4.0061
sqrt(mean((quant_test_twice$TARGDOL-predict(twi_lin, newdata = quant_test_twice))^2))


##------------- MODEL (III): For Loyal Donors (Past Contribution Times >= 3) -------------
loyal = quant_learn[quant_learn$CNTMLIF>=3, ]
loyal = transform(loyal, AVG=CNTRLIF/CNTMLIF)
loyal = transform(loyal, avg = (CNDOL1+CNDOL2+CNDOL3)/3)
loyal = transform(loyal, vip = ifelse(CONLARG>100, 1, 0))

###----- [i.] Principle Components Regression (didn't use in the end) -----
trial = princomp(~CNDOL1+CNDOL2+avg, data = loyal)
summary(trial)
trial$loadings

# add PC1 & PC2 to the dataset
loyal = transform(loyal, PC1=.724*CNDOL1+.62*CNDOL2+.3*avg, PC2=.69*CNDOL1-.66*CNDOL2-.3*avg)

# regress with PCs as predictors
loy_lin = lm(TARGDOL~PC1+PC2+CONLARG+cnc1+cnc2+sol2+cnc3, data = loyal[2:7679, ])
summary(loy_lin)
plot(loy_lin, which = 3)
crPlots(loy_lin)

###----- [ii.] Linear Model w/ Interaction Terms -----
concLoyal = c(2:3, 5:7, 9:15, 17:670, 672:2644, 2646:4856, 4858:7679)
loy_int = lm(TARGDOL~AVG+sqrt(CONLARG)+vip+sol2+CNMON1+CNDOL1:cnc1+CNDOL2:cnc2+CNDOL3:cnc3, data=loyal, subset = concLoyal)
summary(loy_int)
step(loy_int)
plot(loy_int, which = 1)
d3 = cooks.distance(loy_int)
concLoyal = concLoyal[d3<.3, ]
avPlots(loy_int)

summary(lm(TARGDOL~CNDOL1:cnc1+CNDOL2:cnc2+CNDOL3:cnc3, data =loyal, subset = concLoyal ))

# refit using Box-Cox transformed response (didn't use in the end)
ha = boxCox(loy_int, lambda = seq(-1,1,.1))  # lambda = .2121
lam = .2121
loyal$tar = (loyal$TARGDOL^lam - 1)/lam
loy_bc = lm(tar~log(AVG)+I(CONLARG^(1+4*lam))+CNDOL1:cnc1+CNDOL2:cnc2+CNDOL3:cnc3+SEX+sol2, data = loyal, subset = c(2:3, 5:2644, 2646:7679))
summary(loy_bc)
plot(loy_bc, which = 1)

###----- [iii.] Regularized Regressions: RIDGE/LASSO -----
mat = model.matrix(loy_int)

# finding the best penalizing coefficient -- lambda
MSEs <- NULL
for (i in 1:100){
  cv <- cv.glmnet(mat, concLoyal$TARGDOL, alpha=0, lambda = seq(0,1,.01))
  MSEs <- cbind(MSEs, cv$cvm)
}
rownames(MSEs) <- cv$lambda
lamb = as.numeric(names(which.min(rowMeans(MSEs)))) #  best lambda=0.03

ridg = glmnet(mat, concLoyal$TARGDOL, alpha=0, lambda = lamb)
coef(ridg)
ridg$dev.ratio


###----- [iv.] Test On Validation Set -----
test_loyal = subset(quant_test, CNTMLIF>=3)
test_loyal = transform(test_loyal, AVG = CNTRLIF/CNTMLIF)
test_loyal = transform(test_loyal, vip = ifelse(CONLARG>100, 1, 0))

# for loy_int --- MSE = 20.3464
mean((test_loyal$TARGDOL - predict(loy_int, newdata = test_loyal))^2)

# for ridge --- MSE = 20.3097
newmat = model.matrix(lm(TARGDOL~AVG+sqrt(CONLARG)+vip+sol2+CNMON1+CNDOL1:cnc1+CNDOL2:cnc2+CNDOL3:cnc3, data = test_loyal))
pred_ridg = predict(ridg, newx=newmat, type = 'response')
mean((test_loyal$TARGDOL - pred_ridg)^2)


