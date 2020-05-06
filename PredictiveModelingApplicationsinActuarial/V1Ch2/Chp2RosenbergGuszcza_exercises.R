######################################################################
#	program:	Chp2RosenbergGuszcza_exercises.R
#	purpose:	Exercises for Chapter 2 "Overview of Linear Models"
#
######################################################################
library(MASS)	# "Modern Applied Statistics in S"
library(psych)	# for scatterplot matrices pairs.panels() function


####################################
#	Exercise 1:  F-test 
####################################
rm(list=ls())		### clear workspace

dat <- read.csv("MEPS.Diabetes.csv")
dat$MENTHEALTH <- factor(dat$MENTHEALTH)
train   <- dat[dat$PANEL==13,]


r1 <- lm(LOGEXPEND ~ LOGINCOME, train) 
rho  <- cor(train$LOGEXPEND, train$LOGINCOME) 
sd.y <- sd(train$LOGEXPEND)
sd.x <- sd(train$LOGINCOME)
summary(r1)
rho * sd.y / sd.x; coef(r1)[2]	# see equation 7
rho^2; summary(r1)$r.squared


r2 <- lm(LOGEXPEND ~ LOGINCOME + MENTHEALTH, train) 
summary(r2)
anova(r1, r2, test = "F")
AIC(r1, r2)
###	The F and AIC tests suggest that adding MENTHEALTH
###	improves the model.

###	Note that the coefficient for LOGINCOME decreases when 
###	MENTHEALTH is added.
coef(r1)
coef(r2)

###	A boxplot helps illustrate why this happens:
###	People with better mental health status 
###	tend to have higher incomes. 
txt <- "Relationship Between Income and Mental Health"
boxplot(LOGINCOME ~ MENTHEALTH, train, col='aliceblue', main=txt)
abline(h=median(train$LOGINCOME), col="blue", lwd=3)



####################################
#	Exercise 2:  Residual analysis
#		Partial Correlation
####################################
###	Continuing using the model fit in Exercise 1.


plot(resid(r2), rstandard(r2)) 
cor(resid(r2), rstandard(r2)) 
# note:  in this exercise analyzing either un-standardized or
# standardized residuals will not lead to materially different conclusions


###	plot standardized model residuals against AGE
e <- resid(r2) 
plot(e~train$AGE, xlab="AGE", pch=20, cex=.8, col='grey')
title("Residuals versus Age", line=.5, cex.main=1)
abline(h=0,lwd=1)
lines(lowess(train$AGE,e), lwd=3, col="blue") 
###	The residuals are correlated with AGE.
###	This suggests that AGE should be added to the model.

###	Create "Added Variable Plot" for AGE
e.age <- resid( update(r2, AGE~.) )	
plot(e~e.age, pch=20, cex=.8, col='grey')
title("Added Variable Plot for AGE", line=.5, cex.main=1)
abline(h=0,lwd=1)
lines(lowess(e.age,e), lwd=3) 


###	calculate partial correlation coefficient
cor(e, e.age)


###	calculate partial correlation in terms of t-stat
r3 <- update(r2, .~. + AGE)
summary(r3)
t.age <- summary(r3)$coef[6,3]
df.r3 <- df.residual(r3)
rho.age <- t.age / (t.age^2 + df.r3)^.5
rho.age
cor(e, e.age)
### 	The two methods of computing partial correlation
###	yield identical results.




####################################
#	Exercise 3:  "reverse test"
#		the final model
####################################
rm(list=ls())		### clear workspace

###	read in data and fit model cs3
dat <- read.csv("MEPS.Diabetes.csv")
dat$MENTHEALTH <- factor(dat$MENTHEALTH)
train   <- dat[dat$PANEL==13,]
holdout <- dat[dat$PANEL==14,]

cs3 <- lm(LOGEXPEND ~ AGE + AGE:STROKE	
	+ BMI + RACE + HISPANIC 
	+ EMPLOYED + UNINSURED + MENTHEALTH 
	+ EMPHYSEMA + STROKE + CORONARY + CHOLEST
	+ CANCER + ASTHMA + HIGHBP
	, data=train)
summary(cs3)

###	Refit cs3 on the holdout data
cs3.updated <- update(cs3, data=holdout)


###	Compare parameters of cs3 and cs3.updated
b1 <- summary(cs3)$coef[,c(1,3)]
b2 <- summary(cs3.updated)$coef[,c(1,3)]
beta <- data.frame(b1,b2)
beta[, c(1,3)] <- round(beta[, c(1,3)], 3)
beta[, c(2,4)] <- round(beta[, c(2,4)], 1)
names(beta) <- c("beta", "t", "beta.refit", "t.refit")
beta
###	Broadly speaking, the coefficients from the model refit on the 
###	test data are consistent with those of the original model cs3.


###	Now compare actual, predicted values of cs3.updated on the train data.
yhat.cs3.holdout <- predict(cs3.updated, train)
scores <- data.frame(train$LOGEXPEND, yhat.cs3.holdout)
names(scores) <- c("LOGEXPEND", "Predicted")
pairs.panels(scores)
###	The results are broadly consistent with 
###	cs3 tested on the holdout data.


####################################
#	Exercise 4:  swap 
#	COMORB.CNT for 5 of the 
#	comorbidity indicators 
#	in the final model
####################################
###	Continuing from Exercise 3:

# verify definition of COMORB.CNT
attach(train)
cnt <- EMPHYSEMA+CHOLEST+CANCER+ASTHMA+HIGHBP+STROKE+CORONARY
table(COMORB.CNT,cnt)
detach(train)

summary(cs3)
###	Observation: Five of the seven comorbidity indicators all have
###	coefficients in the 0.20-0.26 range.
###	Let's try dropping them and replacing them with COMORB.CNT

cs4 <- update(cs3, .~. - EMPHYSEMA-CHOLEST-CANCER-ASTHMA-HIGHBP+COMORB.CNT)
AIC(cs3); AIC(cs4)
###	AIC goes down, suggesting that cs4 is a better model.
###	cs3 and cs4 are not nested models, so we do not perform an F test.

summary(cs4)
###	The coefficient for COMORB.CNT is approximately 0.24, broadly consistent 
###	with the coefficients of the dropped comorbidity indicators.

coef(cs3);coef(cs4)
###	Other than STROKE and CORONARY, the coefficients of the other 
###	variables have changed only slightly.


###	Confirm that models cs3 and cs4 make highly consistent predictions
LOGEXPEND <- holdout$LOGEXPEND
yhat.cs3 <- predict(cs3, holdout)
yhat.cs4 <- predict(cs4, holdout)
cor(yhat.cs3,yhat.cs4)
pairs.panels(data.frame(LOGEXPEND,yhat.cs3,yhat.cs4))
summary(yhat.cs3 / yhat.cs4 - 1)



####################################
#	Exercise 5:  p-values and 
#		variable selection
####################################
rm(list=ls())		### clear workspace

###	Generate 101 standard normal random variables {Y, X1, ..., X100}.
set.seed(650)
nsim <- 1000
ddat <- NULL
for (i in 1:101)  ddat <- cbind(ddat,rnorm(nsim))
ddat <- as.data.frame(ddat)
names(ddat)[1] <- "Y"
names(ddat)[2:101] <- paste("X", 1:100, sep="")
summary(ddat)

###	Regress Y on X1-X100.
###	How many variables do you expect to be "significant" at the 0.05 level?
r1 <- lm(Y~., ddat)

summary(r1)
###  How do the results compare with what you expected?


###	Bonus material:
###	The above was based on only one simulation.
###	Let's repeat it 100 times and analyze the results.
set.seed(652)
cnt <- NULL
for (i in 1:100){
	ddat <- NULL
	for (i in 1:101)  ddat <- cbind(ddat,rnorm(nsim))
	ddat <- as.data.frame(ddat)
	names(ddat)[1] <- "Y"
	names(ddat)[2:101] <- paste("X", 1:100, sep="")
	reg.temp <- lm(Y~., ddat)
	p.temp <- summary(reg.temp)$coef[,4]
	cnt <- c(cnt, sum(p.temp <= 0.05))
}
hist(cnt, col='aliceblue')
abline(v=median(cnt), col="red", lwd=3)
###	On average, 5 of the 100 randomly generated, uncorrelated variables
###	show up as "significant" at the 0.05 level.



####################################
#	Exercise 6:  missing value 
#		simulation exercise 
####################################
rm(list=ls())		### clear workspace

library(mvtnorm)
nsim <- 1000
doSim <- function(rho){
	x <- rmvnorm(nsim, mean=c(0,0), sigma=matrix(c(1,rho,rho,1), ncol=2))
	e <- rnorm(nsim)
	x1 <- x[,1]; x2 <- x[,2]
	y <- 3 + 5*x1 + 7*x2 + e
	ddat <<- data.frame(y,x1,x2)
}

rho <- .7
set.seed(654)		### setting seed ensures identical results when code is re-run.
doSim(rho)
pairs.panels(ddat)

summary(lm(y~x1+x2,ddat))		
### Coefficients match the "true" values used to simulate data ({3,5,7}).

###	Introduce MCAR values of x2: missing 25% of time.
miss <- sample(c(0,0,0,1), nsim, replace=T)
table(miss)
ddat$x2[miss==1] <- NA
pairs.panels(ddat)
summary(ddat)
summary(lm(y~x1+x2,ddat))	
### Note residual standard error degrees of freedom has decreased.
### Also standard errors of parameter estimates increased.
### However, coefficients still consistent with the "true" values {3,5,7}.	


###	Recode missing values of x2 to the mean value.
ddat$x2.imp <- ddat$x2
ddat$x2.imp[is.na(ddat$x2.imp)] <- mean(ddat$x2.imp, na.rm=T)
summary(ddat)
summary(lm(y ~ x1 + x2.imp, ddat))	
### When rho > 0, the parameter estimates are biased.

###	Set up a missing value indicator for x2 and add to model.
ddat$x2.m <- I(is.na(ddat$x2))*1
summary(lm(y ~ x1 + x2.imp + x2.m, ddat))	
### Adding the missing value indicator doesn't remove the bias.

###	What accounts for the bias?
###	Let's create a scatterplot matrix to investigate.
pairs.panels(ddat)
### 	The scatterplot matrix indicates that the mean value imputation 
### 	has changed the correlation structure of the data.

### 	Now include an interaction term between x1 and x2.m
summary(lm(y ~ x1*x2.m + x2.imp, ddat))	
### 	Adding the interaction term removes the bias.


###	Now repeat this exercise with rho=0.
rho <- 0
set.seed(654)
doSim(rho)
pairs.panels(ddat)		### note 0 correlation between x1 and x2
miss <- sample(c(0,0,0,1), nsim, replace=T)
table(miss)
ddat$x2[miss==1] <- NA
pairs.panels(ddat)
summary(ddat)
summary(lm(y~x1+x2, ddat))	### as before, no bias

### Now set up the missing indicator and mean-impute as before.
ddat$x2.imp <- ddat$x2
ddat$x2.imp[is.na(ddat$x2.imp)] <- mean(ddat$x2.imp, na.rm=T)
ddat$x2.m <- I(is.na(ddat$x2))*1
summary(ddat)
summary(lm(y ~ x1 + x2.imp + x2.m, ddat))	
### In the rho=0 case, this procedure does not result in biased estimates.
pairs.panels(ddat)
