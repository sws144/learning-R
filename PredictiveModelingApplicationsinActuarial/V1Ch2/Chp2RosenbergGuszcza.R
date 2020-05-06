######################################################################
#	program:	Chp2RosenbergGuszcza.R
#	purpose:	code underlying Chapter 2 "Overview of Linear Models"
#
#	Part 1:  Section 2 examples
#	Part 2:  Section 3 Case Study
#
######################################################################
library(MASS)	# "Modern Applied Statistics in S"
library(psych)	# contains pairs.panels() function
library(car) 	# contains vif function
library(MPV)  	# contains PRESS function



######################################################################
#	Part 1:	Section 2 examples
######################################################################
rm(list=ls())
dat <- read.csv("MEPS.Diabetes.csv")
dat$MALE <- I(dat$FEMALE==0)*1
train   <- dat[dat$PANEL==13,]
test    <- dat[dat$PANEL!=13,]



#
# 	Example 1:  basic Body Mass Index (null model)
#
nrow(train)
mean(train$BMI)
m0 <- lm(BMI~1, data=train)
m0

#
# 	Example 2:  one-variable regression for BMI
#
m1 <- lm(BMI~AGE, train)
summary(m1)


par(mfrow=c(1,1), oma = c(1,1,1,1))
plot(BMI~AGE, data=train, pch=16, cex=.3
	, las=1, cex.axis=1.1, cex.lab=1.1)
title("Scatterplot of BMI vs AGE", cex.main=1.2, line=.5)
abline(h=mean(train$BMI), lty=2)
abline(v=mean(train$AGE), lty=2)
abline(m1, lwd=3)



#
# 	Example 3: linear model with sex
#
m2 <- lm(BMI ~ MALE, train)
coef(m2)
coef(lm(BMI ~ FEMALE, train))
coef(lm(BMI ~ MALE + FEMALE -1, train))	#no intercept
mean(train$BMI[train$MALE==1])
mean(train$BMI[train$FEMALE==1])

coef(lm(BMI ~ MALE + FEMALE, train))	#perfect multicollinearity


#
# 	Example 4:  multi-variable regression for BMI
#
form <- formula(BMI ~ AGE + MALE + BLACK)
m3 <- lm(form, train)
summary(m3)


par(oma=c(6,6,1,1), mgp=c(2,.7,0), tck=-.01, mar=c(3,4,2,1))
split.screen(c(2,1))
split.screen(c(1,2),1)
screen(3)
boxplot(BMI~MALE, train, las=1, varwidth=TRUE
	, names=c("Female","Male"), cex.axis=1.2)
#abline(h=mean(train$BMI), lwd=2, lty=2)
mtext("BMI", side=2, line=2.5, cex=1.2)

screen(4)
boxplot(BMI~BLACK, train, las=1, varwidth=TRUE
	, names=c("Other","Black"), cex.axis=1.2)

screen(2)
lab <- c("F.Other", "M.Other", "F.Black", "M.Black")
boxplot(BMI~MALE*BLACK, train, las=1, varwidth=TRUE
	, names=lab, cex.axis=1.2)
mtext("BMI", side=2, line=2.5, cex=1.2)
close.screen(all=TRUE)



#
# example 5-6:  estimating parameters of one-variable linear model
#
center <- function(x)  x - mean(x)
attach(train)
b1 <- sum( center(BMI)*center(AGE) ) / sum( center(AGE)^2 ) ; b1
cor(BMI,AGE) * sd(BMI) / sd(AGE)
b0 <- mean(BMI) - b1*mean(AGE); b0
detach(train)
summary(m1)

yhat <- b0 + b1*train$AGE	#compute fitted values
summary(yhat - m1$fitted)	#check
summary(yhat - predict(m1))	#check


#
# example 7:  null model output
#
summary(lm(BMI~1, train))
mean(train$BMI)
sd(train$BMI)


#
# example 8:  output of multivariable linear model
#
summary(m3)
form <- formula(BMI ~ AGE + MALE + BLACK)
X <- model.matrix(form, train)
y <- train$BMI
beta <- solve(t(X) %*% X) %*% t(X) %*% y
cbind(coef(m3), beta)


#
# example 9:  partitioning sum of squares:  multivariable linear model
#
# fitted values of multivariable linear model
yhat <- X %*% as.vector(beta)
summary(yhat - predict(m3))	# check calculation

# Partitioning the Sum of Squares
# ANOVA table
SST <- sum((y - mean(y))^2)
SSR <- sum((yhat - mean(yhat))^2)
SSE <- sum((y - yhat)^2)
SSR; SSE; SST
SSR / (length(beta)-1)
SSE / (nrow(X) - length(beta))
SST / (nrow(X) - 1)
sd(train$BMI)^2

# R squared
SST ; SSR + SSE
1 - SSE/SST
cor(train$BMI, predict(m3))^2
summary(m3)$r.squared


s2 <- SSE / (nrow(train) - length(beta))		# s2 is MSE
s2; summary(m3)$sigma^2					# check


#
# example 10:  significance of AGE in Linear Model
#
summary(m1)
t.stat <- summary(m1)$coef[2,1] / summary(m1)$coef[2,2]
t.stat; summary(m1)$coef[2,3]		# check
p.val <- 2 * pt(t.stat, df=nrow(train)-2)
p.val; summary(m1)$coef[2,4]   	# check
t.stat^2 ; summary(m1)$fstatistic	# check
p.val; 1-pf(t.stat^2, 1, 1099)	# t-stat and F-stat have same p-value


#
# example 11:  nested model test of RACE
#
form <- formula(BMI ~ AGE + MALE + RACE)
m4 <- lm(form, train)
m5 <- update(m4, .~. - RACE)

SSE.reduced <- sum((train$BMI - predict(m5))^2)
SSE.full    <- sum((train$BMI - predict(m4))^2)
MSE.full <- SSE.full / (nrow(train) - length(coef(m4)))
n <- nrow(train)
k <- length(coef(m5)) - 1		# number of variables in reduced model
q <- length(coef(m4)) - (k+1)		# number of additional variables in full model
F.stat <- (SSE.reduced - SSE.full) / (q * MSE.full)
q
SSE.reduced - SSE.full
F.stat
1 - pf(F.stat, q, n-(k+q+1))		# p-value < .0001 ==> we reject H0
anova(m5, m4, test="F")			# check
anova(m4)					# see table 4
anova(m5)					# see table 4


#
# example 12	Nested model is the null model
#
R2 <- cor(train$BMI, train$AGE)^2
F.stat <- R2 / ( (1-R2)/(1101-2) )
R2; F.stat
anova(m1)

# alternate calculation of F-stat in the case of simple linear model
summary(m1)
t.stat <- summary(m1)[[4]][2,3]
t.stat^2 ; F.stat	


#
# example 13	VIF for model BMI~AGE+MALE+BLACK model
#
R2.age   <- summary(update(m3, AGE~.-AGE))$r.squared
R2.male  <- summary(update(m3, MALE~.-MALE))$r.squared
R2.black <- summary(update(m3, BLACK~.-BLACK))$r.squared
(1 - R2.age)^-1
(1 - R2.male)^-1
(1 - R2.black)^-1
vif(m3)			# check


#
# example 14	expected value and variance of regression estimators
#
# std errors of regression estimators
yhat <- predict(m3)
y <- train$BMI
beta <- coef(m3)
SSE <- sum( (y-yhat)^2 )
s2 <- SSE / (nrow(train) - length(beta))		# s2 is MSE
s2; summary(m3)$sigma^2					# check
var.beta <- s2 * solve(t(X) %*% X)
var.beta
diag(var.beta) ^ .5
summary(m3)[[4]][,2]					# check


s.m1 <- summary(m1)$sigma
s.m3 <- summary(m3)$sigma
s.m1 ^ 2
s.m3 ^ 2


## illustrate relationship between s.e.(beta.male) and VIF.male
summary(m3)
VIF.male <- (1 - R2.male)^-1
s.m3 * VIF.male^.5 / (sd(train$MALE)*(nrow(train)-1)^.5)
summary(m3)$coef[3,2]


#
# example 15-6	R2, adjusted R2, AIC
#
m6 <- lm(BMI~AGE+MALE+RACE+UNINSURED, train)
mods <- list(m2, m1, m5, m3, m4, m6)
mods


# functions to compute various model adequacy measures
r2  <- function(x)  round(summary(x)$r.squared, 5)
r2a <- function(x)  round(summary(x)$adj.r.squared, 5)
SSPE <- function(mod){
	yhat <- predict(mod, test)
	y <- test$BMI
	sspe <- sum( (y-yhat)^2 )
	return(sspe)
}

# check
SSPE(m6)
sum( (predict(m6, test) - test$BMI)^ 2 )

compare <- NULL
for(i in 1:length(mods)){
	mod <- mods[[i]]
	temp <- c(r2(mod), r2a(mod)
		, round(AIC(mod),3), SSPE(mod), PRESS(mod))
	compare <- rbind(compare, temp)
}
rownames(compare) <- 1 : nrow(compare)
colnames(compare) <- c("R2", "adj R2", "AIC", "SSPE", "PRESS")
compare


# illustration:  Compute R2 and adjusted R2 for model m3
SSE <- sum( (predict(m3)-train$BMI)^2)
SST <- sum( (train$BMI - mean(train$BMI))^2)
1 - SSE/SST; summary(m3)$r.squared
n <- nrow(train)
k <- length(coef(m3)) - 1
R2.adj <- 1 - (SSE/(n-k-1)) / (SST/(n-1))
R2.adj 
summary(m3)$adj.r.squared

# illustration:  compute AIC for model m3
k <- length(coef(m3)) - 1		# number of variables in the model
p <- length(coef(m3)) + 1		# number of parameters in the model
s <- summary(m3)$sigma
AIC(m3)
-2*logLik(m3)[1] + 2*p
n*log(2*pi) + n*log(s^2) + n + 3 + k



#
#	Example 17 - high leverage
#
form7 <- BMI ~ AGE + MALE + RACE + INCOME
m7 <- lm(form7, train)
X.7 <- model.matrix(form7, train)

form8 <- BMI ~ AGE + MALE + RACE + LOGINCOME
m8 <- lm(form8, train)
X.8 <- model.matrix(form8, train)

yhat.7 <- predict(m7)
yhat.8 <- predict(m8)
sr.7 <- rstandard(m7)
sr.8 <- rstandard(m8)
HAT <- function(X)  X %*% solve(t(X) %*% X) %*% t(X)
h.7 <- diag(HAT(X.7))		# leverage
h.8 <- diag(HAT(X.8))		# leverage
sum(h.8); length(coef(m8))	# check
cd.7 <- cooks.distance(m7)	# cooks distance
cd.8 <- cooks.distance(m8)	# cooks distance


# produce leverage plot for INCOME and log(INCOME) models
par(mfrow=c(1,2))
h <- h.7
plot(h, type='h', xlab="Observation", ylab=""
	, ylim=c(0,.045), cex.lab=1.3, cex.axis=1.3)
abline(h = 3*mean(h), lty=3)
title("Income", line=.5, cex.main=1.3)

hi <- h > 3*mean(h)
summary(train[hi,])
train$INCOME[hi]		
summary(train$INCOME)
mean(train$INCOME > 190000)
# INCOME for all 14 high leverage points is > $190000
# motivates the use of Log(Income) instead

h <- h.8
plot(h, type='h', xlab="Observation", ylab=""
	, ylim=c(0,.045), cex.lab=1.3, cex.axis=1.3)
abline(h = 3*mean(h), lty=3)
title("Log(Income)", line=.5, cex.main=1.3)
mtext(side=2, "Leverage", outer=TRUE, line=-1.5, cex=1.5)
hi <- h > 3*mean(h)
sum(hi)
train[hi,]



#
#	Example 18 - outliers
#
sum(abs(rstandard(m7)) > 2)
sum(abs(rstandard(m8)) > 2)
table( abs(sr.7)>2, h.7>3*mean(h.7))
table( abs(sr.8)>2, h.8>3*mean(h.8))



# produce standardized residual plot for INCOME and log(INCOME) models
par(mfrow=c(1,2))
plot(sr.7, type="h", ylab="", xlab="Observation", cex.lab=1.3, cex.axis=1.3, las=1)
abline(h=0)
abline(h=c(-2,2), lty=3)
title("Income", line=.5, cex.main=1.3)

plot(sr.8, type="h", ylab="", xlab="Observation", cex.lab=1.3, cex.axis=1.3)
abline(h=0)
abline(h=c(-2,2), lty=3)
title("Log(Income)", line=.5, cex.main=1.3)
mtext(side=2, "Stnd. Residual", outer=TRUE, line=-1.5, cex=1.3)



#
#	Example 19 - Cook's Distance
#
par(mfrow=c(1,2))
h <- h.7
pch <- rep(16, length(h))
pch[which(h > 3*mean(h))] <- 17
cex <- rep(.3, length(h))
cex[which(h > 3*mean(h))] <- 1
hi.cd <- cd.7 > 4/length(cd.7)

plot(yhat.7, sr.7, pch=pch, cex=cex, xlab="Fitted Value", ylab=""
	, las=1, cex.lab=1.3, cex.axis=1.3, xlim = c(26, 38))
title("Income", line=.5)
abline(h=0)
abline(h=c(-2,2), lty=3)
points(yhat.7[which(hi.cd)], sr.7[which(hi.cd)], cex=1.5)

h <- h.8
pch <- rep(16, length(h))
pch[which(h > 3*mean(h))] <- 17
cex <- rep(.3, length(h))
cex[which(h > 3*mean(h))] <- 1
hi.cd <- cd.8 > 4/length(cd.8)

plot(yhat.8, sr.8, pch=pch, cex=cex, xlab="Fitted Value", ylab=""
	, las=1, cex.lab=1.3, cex.axis=1.3)
title("Log(Income)", line=.5)
abline(h=0)
abline(h=c(-2,2), lty=3)
mtext(side=2, "Stnd. Residual", outer=TRUE, line=-1.5, cex=1.3)
points(yhat.8[which(hi.cd)], sr.8[which(hi.cd)], cex=1.5)



#
#	Example 20 - residual plots
#
par(mfrow=c(1,2))
plot(train$AGE, sr.8, pch=16, cex=.3, xlab="AGE", ylab=""
	, las=1, cex.lab=1.3, cex.axis=1.3)
abline(h=c(-2,0,2), lty = 2)
lines(lowess(train$AGE,sr.8), lwd=4, col="slategrey")

pch <- as.numeric(factor(train$RACE)) + 5
col <- as.vector(factor(train$MALE, labels=c("grey42", "black")))
plot(yhat.8, sr.8, pch=pch, col=col, , cex=.7, xlab="Fitted Value", ylab=""
	, las=1, cex.lab=1.3, cex.axis=1.3)
abline(h=c(-2,0,2), lty = 2)


txt <- c("White", "Black", "Other")
legend("topleft", txt, pch=sort(unique(pch)), inset=.01, cex=1.2)
legend(28.2,5.9, c("Female","Male"), col=unique(col), pch=19, inset=.01, cex=1.2)
mtext(side=2, "Stnd. Residual", outer=TRUE, line=-1.5, cex=1.3)


# motivated by residual plots, add quadratic term and interaction
m9 <- lm(BMI ~ AGE + MALE + RACE + LOGINCOME + I(AGE^2) + MALE:RACE, train)
AIC(m8, m9)	
summary(m9)


#
#	Example 21 - QQ plots
#
sr.1 <- rstandard(lm(BMI~1,train))
sr.9 <- rstandard(m9)

par(mfrow=c(1,2))
qqnorm(sr.1, main="", ylab="", las=1, cex.axis=1.2, cex.lab=1.2)
qqline(sr.1)
title("Null Model", line=.5, cex.main=1.2)
qqnorm(sr.9, main="", ylab="", las=1, cex.axis=1.2, cex.lab=1.2)
qqline(sr.9)
title("Model with Quadratic Term", cex.main=1.2)
title("and Race Interaction", line=.5, cex.main=1.2)
mtext(side=2, "Sample Quantiles", outer=TRUE, line=-1.5, cex=1.3)


#
#	Example 22 - Summary of Examples
#

mods <- list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9)
mods

# functions to compute various model adequacy measures
s2  <- function(x)  round(summary(x)$sigma^2, 2)
r2  <- function(x)  round(summary(x)$r.squared, 4)
r2a <- function(x)  round(summary(x)$adj.r.squared, 4)
SSPE <- function(mod){
	yhat <- predict(mod, test)
	y <- test$BMI
	sspe <- sum( (y-yhat)^2 )
	return(round(sspe))
}

compare <- NULL
for(i in 1:length(mods)){
	mod <- mods[[i]]
	temp <- c(mod$df, s2(mod), r2(mod), r2a(mod)
		, round(AIC(mod)), round(PRESS(mod)), SSPE(mod))
	compare <- rbind(compare, temp)
}
rownames(compare) <- 1 : nrow(compare)
colnames(compare) <- c("df","s2","R2","adj R2","AIC","PRESS","SSPE")
compare




######################################################################
#	Part 2:	Case Study (Section 3)
######################################################################
rm(list=ls())
dat <- read.csv("MEPS.Diabetes.csv")

# We will treat MENTHEALTH as an ordinal categorical variable
dat$MENTHEALTH <- factor(dat$MENTHEALTH)
summary(dat$MENTHEALTH)

#
#	Split data into training, holdout sets
#
train   <- dat[dat$PANEL==13,]
holdout <- dat[dat$PANEL==14,]
nrow(dat); nrow(train); nrow(holdout)


#
#	EDA:  target variable visualization
#
par(mfrow=c(1,2), mar=c(2,2,2,1)+0.1)
truehist(dat$EXPEND, xlab="", col='grey20')
title("EXPEND", line=.5)
truehist(dat$LOGEXPEND, xlab="", col='grey20')
title("LOGEXPEND", line=.5)



#
#	EDA:  continuous variable visualization
#
par(mfrow=c(1,1))
cont <- c("INCOME", "LOGINCOME", "BMI", "AGE", "COMORB.CNT", "LOGEXPEND")
### function from psych package
pairs.panels(dat[,cont], pch='.', gap=0, ellipses=F, col.points='grey'
	, lwd=2, hist.col='aliceblue')   


###	confirm that p-values of all correlation coefficients are < 0.01
library(Hmisc)	### contains rcorr() function
temp <- as.matrix(dat[,cont])
rcorr(temp, type="pearson")


#
#	EDA:  discrete variable visualization
#
vars <- c("FEMALE","RACE","HISPANIC"
	,"EMPLOYED","UNINSURED","MEDICAID", "MENTHEALTH"  
	,"SMOKER","EMPHYSEMA","STROKE","CORONARY","CHOLEST"   
	,"CANCER","ASTHMA","HIGHBP")
vars <- vars[15:1]
par(mfrow=c(3,5), oma=c(6,6,1,1), mgp=c(2,.7,0), tck=-.01, mar=c(1,0,2,0))
for (i in 1:length(vars)){ 
	if (i != 14) bb=boxplot(dat[,"LOGEXPEND"]~dat[,vars[i]], ylim=c(0,14), yaxt="n", cex.axis=1.6)
	if (i == 14) bb=boxplot(dat[,"LOGEXPEND"]~dat[,vars[i]], ylim=c(0,14), yaxt="n", cex.axis=1.6, las=2)
	if (i %in% c(1,6,11)) axis(side=2, at=seq(0,14,2), labels=T, cex.axis=1.6, las=2)
		
	abline(h=median(dat[,"LOGEXPEND"]))
	perc <- paste(round(bb$n/sum(bb$n),2)*100,"%",sep="")
	text(1:length(perc), .5, perc, cex=1.4)
	title(vars[i], line=-1.2, cex.main=1.6)
}
mtext("LOGEXPEND", side=2, outer=T, line=3, cex=1.2)
## Note: for above graph changed cex in text line to 1.4 to allow % to show up clearly


#
#	Fit Candidate Models
#

# start by fiting an "everything" model using most variables
cs1 <- lm(LOGEXPEND ~ LOGINCOME 
	+ FEMALE + AGE + BMI + RACE + HISPANIC 
	+ EMPLOYED + UNINSURED + MEDICAID + MENTHEALTH 
	+ EMPHYSEMA + STROKE + CORONARY + CHOLEST
	+ CANCER + ASTHMA + HIGHBP
	, data=train)
summary(cs1)

# remove non-significant explanatory variables
cs2 <- lm(LOGEXPEND ~ AGE + BMI + RACE + HISPANIC 
	+ EMPLOYED + UNINSURED + MENTHEALTH 
	+ EMPHYSEMA + STROKE + CORONARY + CHOLEST
	+ CANCER + ASTHMA + HIGHBP
	, data=train)
summary(cs2)
AIC(cs1)
AIC(cs2)
anova(cs2, cs1, test = "F")

# Verify that stepAIC results in the same eliminations.
# Note: stepAIC is an automatic variable selection procedure.
check <- stepAIC(cs1)
data.frame(coef(check), coef(cs2))


# Should we drop EMPHYSEMA and STROKE?
AIC(cs2)
AIC(update(cs2, .~. - EMPHYSEMA - STROKE))
anova(update(cs2, .~. - EMPHYSEMA - STROKE), cs2, test = "F")
# Let's keep them:  cs2 remains our baseline model


#
#	Explore potential need to model non-linearities and interactions
#

# Do we need nonlinear terms for AGE and BMI?
anova(update(cs2, .~. + I(AGE^2)), cs2, test = "F")
anova(update(cs2, .~. + I(BMI^2)), cs2, test = "F")
# This comports with the EDA observation of 
# approximately linear relationships


# Do we need any interactions terms?
# Let's explore a possible interaction between AGE and STROKE.

# Note:  in this section we elected to use (y-yhat)/s as the
#	standardized residual.  The rstandard() function (used above)
#	produces standardized residuals consistent with 
#	the discussion in Section 2 of the text.

# Verify that un this example, the two standardized 
# residual concepts yield virtually identical results.
plot(resid(cs2)/sd(resid(cs2)), rstandard(cs2))
abline(0,1,col="green")
cor(resid(cs2), rstandard(cs2))

par(mfrow=c(1,2), mar=c(2,2,2,2), oma=c(2,3,1,1))
e <- residuals(cs2) / summary(cs2)$sigma
AGE <- train$AGE
plot(e~AGE, pch='.', xlab="AGE", cex.lab=1.6, cex.axis=1.4
	, ylab="Standardized Residuals", ylim=c(-3,3), xlim=c(20,85))
abline(lm(e~AGE), lwd=2)
lines(lowess(AGE,e), lwd=3, lty=2) 
title("All Data", line=.5, cex.main=1.6)
mtext(side=1, "AGE", outer=F, line=2, cex=1.4)
mtext(side=2, "Standardized Residuals", outer=F, line=3, cex=1.4)

keep <- I(train$STROKE==1)
e <- e[keep]
AGE <- AGE[keep]
plot(e~AGE, pch=1, cex=.6, xlab="AGE", cex.lab=1.6, cex.axis=1.4
	, ylab="", ylim=c(-3,3), xlim=c(20,85))
abline(lm(e~AGE), lwd=2)
lines(lowess(AGE,e), lwd=3, lty=2) 
title("Stroke Only", line=.5, cex.main=1.6)
mtext(side=1, "AGE", outer=F, line=2, cex=1.4)


# Add AGE:STROKE interaction
cs3 <- lm(LOGEXPEND ~ AGE + AGE:STROKE	
	+ BMI + RACE + HISPANIC 
	+ EMPLOYED + UNINSURED + MENTHEALTH 
	+ EMPHYSEMA + STROKE + CORONARY + CHOLEST
	+ CANCER + ASTHMA + HIGHBP
	, data=train)
summary(cs3)
anova(cs2, cs3, test = "F")
AIC(cs2);AIC(cs3)


# Graphically analyze AGE:STROKE interaction
temp <- train
temp$yhat.cs2 <- predict(cs2, temp)
temp$yhat.cs3 <- predict(cs3, temp)
stroke <- temp[temp$STROKE==1,]
other <- temp[temp$STROKE==0,]
col <- rep("grey", nrow(temp)); col[temp$STROKE==1] <- "black" 
cex <- rep(.5, nrow(temp)); cex[temp$STROKE==1] <- 1.2
pch <- rep(20, nrow(temp)); pch[temp$STROKE==1] <- 8

par(mfrow=c(1,2), mar=c(2,2,2,2), oma=c(2,3,1,1))
plot(other$LOGEXPEND~other$AGE, xlab="", ylab="", cex.axis=1.4
	, pch='.', cex=.8, ylim=c(6,12), xlim=c(20,85))
lines(lowess(other$AGE,other$LOGEXPEND), lwd=8, lty=1, col='black') 
lines(lowess(other$AGE,other$yhat.cs2), lwd=3, lty=6, col='slategrey') 
lines(lowess(other$AGE,other$yhat.cs3), lwd=3, lty=1, col='darkgrey') 
txt <- "Predictions for non-Stroke Victims"
txt <- "STROKE=0"
title(txt, line=.5, cex.main=1.6)
txt <- c("Actual", "Predicted - CS2", "Predicted - CS3")
legend("topleft", txt, col=c("black", "slategrey", "darkgrey")
	, lwd=c(6,3,3), inset=.01, bg='white', lty=c(1,6,1), cex=1.2)
mtext(side=1, "AGE", outer=F, line=2, cex=1.4)

plot(stroke$LOGEXPEND~stroke$AGE, xlab="", ylab="", cex.axis=1.4
	, pch=1, cex=.6, ylim=c(6,12), xlim=c(20,85))
lines(lowess(stroke$AGE,stroke$LOGEXPEND), lwd=8, lty=1, col='black') 
lines(lowess(stroke$AGE,stroke$yhat.cs2), lwd=3, lty=6, col='slategrey') 
lines(lowess(stroke$AGE,stroke$yhat.cs3), lwd=3, lty=1, col='darkgrey') 
txt <- "Predictions for Stroke Victims"
txt <- "STROKE=1"
title(txt, line=.5, cex.main=1.6)
txt <- c("Actual", "Predicted - CS2", "Predicted - CS3")
legend("topleft", txt, col=c("black", "slategrey", "darkgrey")
	, lwd=c(6,3,3), inset=.01, bg='white', lty=c(1,6,1), cex=1.2)
mtext(side=1, "AGE", outer=F, line=2, cex=1.4)
mtext(side=2, "LOGEXPEND", outer=T, line=1, cex=1.4)


# Center continuous variables for more interpretable parameters.
mean(train$AGE); mean(train$BMI)
train$AGE.C <- train$AGE - 60
train$BMI.C <- train$BMI - 32

cs3b <- lm(LOGEXPEND ~ AGE.C + AGE.C:STROKE	
	+ BMI.C + RACE + HISPANIC 
	+ EMPLOYED + UNINSURED + MENTHEALTH 
	+ EMPHYSEMA + STROKE + CORONARY + CHOLEST
	+ CANCER + ASTHMA + HIGHBP
	, data=train)
summary(cs3b)
data.frame(coef(cs3), coef(cs3b))
# Note that only the intercept and the STROKE coefficient change.
# Each of these quantities is more interpretable in model cs3b than in cs3.


#
#	Residual analysis
#
mmod <- cs3
yhat <- predict(mmod)
ddat <- train
ddat$e <- residuals(mmod) / summary(mmod)$sigma
e <- ddat$e

par(mfrow=c(2,2), mar=c(3,4,3,1)+0.1, oma=c(1,1,1,1))
qqnorm(e, col='black', pch=20, cex=.5, main="", cex.axis=1.4, xlab="", ylab="")
abline(0,1)
title("Normal Q-Q Plot", line=.5, cex.main=1.4)

plot(e~yhat, pch='.', ylim=c(-3,3), xlab="", ylab="", cex.axis=1.4)
lines(lowess(yhat,e), lwd=2, lty=2) 
abline(h=0, lwd=2)
abline(h=quantile(e, p=c(.025, .975)))
title("Residuals versus Predicted", line=.5, cex.main=1.4)

vv <- "AGE"
plot(e~ddat[,vv], pch='.',ylim=c(-3,3), xlab="", ylab="", cex.axis=1.4)
ttxt <- paste("Residuals versus", vv)
title(ttxt, line=.5, cex.main=1.4)
abline(lm(e~ddat[,vv]), lwd=2)
lines(lowess(ddat[,vv],e), lwd=2, lty=2) 
vv <- "BMI"
plot(e~ddat[,vv], pch='.',ylim=c(-3,3), xlab="", ylab="", cex.axis=1.4)
ttxt <- paste("Residuals versus", vv)
title(ttxt, line=.5, cex.main=1.4)
abline(lm(e~ddat[,vv]), lwd=2)
lines(lowess(ddat[,vv],e), lwd=2, lty=2) 


vars <- c("RACE","HISPANIC","EMPLOYED","UNINSURED", "MENTHEALTH"
	,"EMPHYSEMA","STROKE","CORONARY","CHOLEST","CANCER","ASTHMA","HIGHBP")
vars <- vars[12:1]
par(mfrow=c(3,4), oma=c(6,4,1,1), mgp=c(2,.7,0), tck=-.01, mar=c(1,0,2,0))
for (i in 1:length(vars)){ 
	if (i != 12) boxplot(e~ddat[,vars[i]], ylim=c(-3,3), yaxt="n", cex.axis=1.6)
	if (i == 12) boxplot(e~ddat[,vars[i]], ylim=c(-3,3), yaxt="n", las=2, cex.axis=1.6)
	if (i %in% c(1,5,9)) axis(side=2, at=seq(-3,3,1), labels=T, cex.axis=1.6, las=2)
	abline(h=median(e))
	title(vars[i], line=-1.2, cex.main=1.6)
}


#
#	Validation on holdout data
#
par(mfrow=c(1,2), mar=c(4,4,3,1)+0.1, oma=c(2,2,1,1))
yhat <- predict(update(cs3, data=holdout))
LOGEXPEND <- holdout$LOGEXPEND
plot(LOGEXPEND,yhat,pch=20, cex=.5, col='grey'
	, cex.lab=1.4, cex.axis=1.4
	, ylab="Predicted LOGEXPEND", xlab="Actual LOGEXPEND")
abline(lm(yhat~LOGEXPEND), lwd=2)
lines(lowess(LOGEXPEND,yhat), lwd=2, lty=2, col='black') 
rho <- round(cor(LOGEXPEND,yhat),2); rho
cor(LOGEXPEND,yhat);cor.test(LOGEXPEND,yhat)
text(4,9.5,paste("r =",rho),cex=2,font=2)
title("Predicted versus Actual LOGEXPEND", line=.5, cex.main=1.6)

yhat <- predict(update(cs3, data=holdout))
qq <- seq(0,1,.1)
cutoffs <- quantile(yhat, p=qq)
centile <- cut(yhat, breaks=quantile(yhat, p=qq), labels=F, include.lowest=T)
yy <- tapply(holdout$EXPEND, centile, sum)
sum(yy); sum(holdout$EXPEND)
yy <- yy[length(yy):1]
yy <- c(0,yy)
yy <- cumsum(yy)/sum(yy)
plot(qq, yy, type='n', lwd=3, cex.lab=1.4, cex.axis=1.4
	, xlab="Cumulative % of Observations"
	, ylab="Cumulative % of EXPEND")
abline(v=qq,col='slategrey',lty=3); abline(h=qq,col='slategrey',lty=3)
lines(qq, yy, type='l', lwd=2)
points(qq, yy, pch=20, cex=2)
points(qq[4], yy[4], cex=3, pch=1)
abline(0,1, lwd=2)
title("Gains Chart", line=.5, cex.main=1.6)


####################################
#	end of case study
####################################