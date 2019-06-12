
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