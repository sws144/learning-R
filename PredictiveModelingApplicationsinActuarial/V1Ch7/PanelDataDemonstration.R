#  GENERATE THE DATA
nsamp <- 50
rbivariate <- function(mean.x = 70, sd.x=3, mean.y=82, sd.y=7, r=.50, iter=nsamp) {
  z1 <- rnorm(iter)
  z2 <- rnorm(iter)
  x <- sqrt(1-r^2)*sd.x*z1 + r*sd.x*z2 + mean.x
  y <- sd.y*z2 + mean.y
  return(list(x,y))
}
set.seed(123457)
data1 <- rbivariate(mean.x = 70, sd.x=3, mean.y=82, sd.y=7, r=.75, iter=nsamp)
eps2 <- rbivariate(mean.x = 1, sd.x=3, mean.y=1, sd.y=14, r=.50,iter=nsamp)
iter <- rep(1:nsamp,2)
time <- rep(1:2, nsamp)
y <- c(data1[[1]],.95*data1[[1]]+eps2[[1]]/2)
x <- c(data1[[2]],1.05*data1[[2]]+eps2[[2]]/2)
newdata <- data.frame(cbind(iter, time, y, x))

#  FIGURE. PLOT ONLY THE FIRST YEAR
plot(data1[[1]],data1[[2]], ylab="",xlab="Rating Variable", xlim=c(58,78),ylim=c(60,110), main="Year 1")
mtext("Loss", side=2, line=1, at=115, cex=1.0, las=1)
#  NOTE THE STRONG POSITIVE STATISTICAL SIGNIFICANCE
summary(lm(data1[[2]]~data1[[1]]))



#  FIGURE. PLOT BOTH YEARS BUT IGNORE DYNAMIC EFFECT
plot(x~y, data = newdata, ylab="",xlab="Rating Variable",xlim=c(58,78),ylim=c(60,110), main="Years 1 and 2")
#  NOTE THE STRONG POSITIVE STATISTICAL SIGNIFICANCE
summary(lm(y~x))

#  GENERATE THE DATA
nsamp <- 50
rbivariate <- function(mean.x = 70, sd.x=3, mean.y=82, sd.y=7, r=.50, iter=nsamp) {
  z1 <- rnorm(iter)
  z2 <- rnorm(iter)
  x <- sqrt(1-r^2)*sd.x*z1 + r*sd.x*z2 + mean.x
  y <- sd.y*z2 + mean.y
  return(list(x,y))
}

#  FIGURE. PLOT CONNECTING THE YEARS ;
plot(x~y, data = newdata, ylab="",xlab="Rating Variable",xlim=c(58,78),ylim=c(60,110), main="Years 1 and 2")
for (i in iter) {lines(x~y, data = subset(newdata, iter == i)) }
mtext("Loss", side=2, line=1, at=115, cex=1.0, las=1)
#  NOTE THE STRONG NEGATIVE STATISTICAL SIGNIFICANCE
summary(lm(y~x+ factor(iter)))