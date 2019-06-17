
# model selection 101 using-r 
# https://towardsdatascience.com/model-selection-101-using-r-c8437b5f9f99

rm(list=ls())

packages <- c("car")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(car)


#load data and convert to 
dat <- read.table("employment_data.txt", header = T)
names(dat) <- c("employed", "foreigner", "gov.support", "age", "education", "young.children", "school.children")
levels(dat$employed)[1] <- 'yes'
levels(dat$employed)[2] <- 'no'
levels(dat$foreigner)[1] <- 'yes'
levels(dat$foreigner)[2] <- 'no'

str(dat)
summary(dat)

fit.1 <- glm(employed =="yes" ~ ., data = dat , family = binomial)
summary(fit.1)

tempfit <- update(fit.1, .~. + factor(young.children == 0)
                              + factor(school.children == 0)
                              + factor(young.children + school.children ==0))
summary(tempfit)


logit.plot <- function(x, y, h, link='logit', xlab=deparse(substitute(x)), yname=deparse(substitute(y)), 
                       ylab, rug=T, data, main){
  if(!missing(data)){
    call <- match.call()
    dataPos <- match("data",names(call))
    return(invisible(with(data, eval(call[-dataPos]))))
  }
  if (length(levels(factor(y)))!=2) stop('y must be binary')
  succes <- levels(factor(y))[2]
  if (missing(ylab)) ylab <- paste(binomial(link)$link,' P{',yname,'=',succes,'|',xlab,'}', sep='', collapse='')
  if (is.factor(y)) y <- as.numeric(y==levels(y)[2])
  x.seq <- seq(min(x),max(x),length=101)
  smooth <- binomial(link)$linkfun(ksmooth(x, y, 'normal', h, x.points=x.seq)$y)
  plot(smooth~x.seq, ylab=ylab, xlab=xlab, type='l', main = main)
  if (rug) rug(x)
  invisible(xy.coords(x = x.seq, y = smooth, xlab = xlab, ylab = ylab))
}

num = 1
for(i in seq(2.5,10,length.out = 6)){
  logit.plot(x = age, y = employed == 'yes', h = i, data = dat, main = paste0("plot: ", num, ", bandwidth = ",i))
  num= num +1
}

tempfit <- update(tempfit, .~. + I(age^2) 
                  + I(education^2)
                  + I(gov.support^2))
summary(tempfit)

fit.2 <- tempfit
add1.test <- add1(fit.2, scope = .~. + .^2, test="Chisq")
add1.test[order(add1.test$'Pr(>Chi)'),]

fit.3 <- update(fit.2, .~. 
                + foreigner:age
                + foreigner:factor(young.children+school.children==0)
                + age:school.children
                + gov.support:factor(young.children==0)) 
summary(fit.3)

drop1.test <- drop1(fit.3,test="Chisq")
drop1.test[rev(order(drop1.test$'Pr(>Chi)')),]

fit.4 <-update(fit.3, .~.
               - I(gov.support^2)
               - young.children
               - education
               - I(education^2))
summary(fit.4)

## final model
glm(employed == "yes" ~ foreigner
    + gov.support
    + age
    + school.children
    + factor(young.children==0)
    + factor(school.children==0)
    + factor(young.children + school.children==0)
    + I(age^2)
    + foreigner:age 
    + foreigner:factor(young.children + school.children == 0) 
    + age:school.children 
    + gov.support:factor(young.children == 0)
    , family = binomial, data = dat)


influencePlot(fit.4)
outlierTest(fit.4)

par(mfrow=c(2,2))
plot(fit.4, col=ifelse(as.integer(rownames(dat))==416, "red", "black"), pch=20)


par(mfrow=c(1,1)) 
plot(p.resid ~ fit.val)

