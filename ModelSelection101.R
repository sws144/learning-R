
# model selection 101 using-r 
# https://towardsdatascience.com/model-selection-101-using-r-c8437b5f9f99

rm(list=ls())

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