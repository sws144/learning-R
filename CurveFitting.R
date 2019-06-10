

# Curve fitting
# https://davetang.org/muse/2013/05/09/on-curve-fitting/


x <- c(32,64,96,118,126,144,152.5,158)
y <- c(99.5, 104.8, 108.5, 100, 86, 64, 35.3, 15)
# y is reponse, x is predictor
# y on y-axis
plot(x,y,pch=19)

#fit first
fit <- lm(y~x)
#second deg
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third deg
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth leg
fit4 <- lm(y~poly(x,4,raw=TRUE))

#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(30,160,length=50)
plot(x,y,pch=19,ylim=c(0,150))

lines(xx,predict(fit,data.frame((x=xx))),col="red")
lines(xx,predict(fit2,data.frame((x=xx))),col="green")

summary(fit)
summary(fit2)
summary(fit3)
summary(fit4)

anova(fit, fit2)

#significant
anova(fit2,fit3)

#not significant
anova(fit3,fit4)

#y=ax+b
coef(fit)
coef(fit4)

#create function
third_order <-function(newdist,model){
  coefs <- coef(model)
  res <- coefs[1] + (coefs[2]*newdist) + (coefs[3]*newdist^2)+(coefs[4]*newdist^3)
  
}
