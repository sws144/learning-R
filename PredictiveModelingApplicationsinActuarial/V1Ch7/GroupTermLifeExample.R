gterm = read.table(file="GLifeMonthlyData.txt", sep =" ", quote = "",header=TRUE)
#     CONVERT MONTHLY TO ANNUAL DATA
gterm$year <- trunc((gterm$MONTH-1)/12)+1
gterm$lastmonth <- 1*(gterm$MONTH==12)+1*(gterm$MONTH==24)+1*(gterm$MONTH==36)+1*(gterm$MONTH==48)
gterm$uniqueid <- gterm$CONT_ID*10000+gterm$year
Cover <- 12*fitted.values(lm(LS_RP_CV~0+factor(uniqueid),data=gterm))
Claims <- 12*fitted.values(lm(LS_TOT_C~0+factor(uniqueid),data=gterm))
temp2<- data.frame(cbind(gterm$CONT_ID,gterm$year,Cover,Claims,gterm$lastmonth))
colnames(temp2) <- c("ID","Year","Coverage","Claims","Y")
temp3 <- subset(temp2, Y>0)
temp3$claimsCover <- temp3$Claims/(1+temp3$Cover)
temp3$lnCover <- log(1+temp3$Cover)
temp3$lnClaims <- log(1+temp3$Claims)
temp3$Year1=temp3$Year+1987
gtermdata  <- subset(temp3, Coverage>1)
#write.csv(gtermdata, file = "gtermdata.csv")
summary(gtermdata)

#     READ IN ANNUAL DATA
gtermdata = read.csv(file="gtermdata.csv", header=TRUE)
#  SUMMARY STATISTICS
Xymat <- data.frame(gtermdata)
colnames(Xymat) <- c("Coverage", "Claims", "claimsCover", "lnCover", "lnClaims")
Xymat[,1] <- Xymat[,1]/1000
Xymat[,2] <- Xymat[,2]/1000
Xymat[,3] <- Xymat[,3]*100
meanSummary <- sapply(Xymat, mean,  na.rm=TRUE) 
sdSummary   <- sapply(Xymat, sd,    na.rm=TRUE) 
minSummary  <- sapply(Xymat, min,   na.rm=TRUE) 
maxSummary  <- sapply(Xymat, max,   na.rm=TRUE) 
medSummary  <- sapply(Xymat, median,na.rm=TRUE) 
summvar <- cbind(meanSummary, medSummary, sdSummary, minSummary, maxSummary)
#write.csv(summvar, file = "gtermdatasummary.csv")
round(summvar,digits=4)

#  PLOTS
plot(lnClaims ~ Year1, data = gtermdata, xaxt="n", ylab="",xlab="")
for (i in gtermdata$ID) {lines(lnClaims ~ Year1, data = subset(gtermdata, ID == i)) }
plot(lnCover ~ Year1, data = gtermdata, xaxt="n", ylab="",xlab="")
for (i in gtermdata$ID) {lines(lnCover ~ Year1, data = subset(gtermdata, ID == i)) } 
plot(claimsCover ~ Year1, data = gtermdata, xaxt="n", ylab="",xlab="")
for (i in gtermdata$ID) {lines(claimsCover ~ Year1, data = subset(gtermdata, ID == i)) }
plot(lnClaims ~ lnCover, data = gtermdata, ylim=c(2,14))
for (i in gtermdata$ID) {lines(lnClaims ~ lnCover, data = subset(gtermdata, ID == i)) }
gtermdata1 <- subset(gtermdata, lnClaims>1)
gtermdata2 <- subset(gtermdata1, Year<2 | Year>3)
plot(lnClaims ~ lnCover, data = gtermdata2, ylim=c(2,14))
for (i in gtermdata$ID) {lines(lnClaims ~ lnCover, data = subset(gtermdata2, ID == i)) }

library(nlme)
gtermdata1 <- subset(gtermdata, lnClaims>1)
gtermGraph <- groupedData(lnClaims ~ Year1|ID, data=gtermdata1)
plot(gtermGraph, xlab="Year", ylab="Logarithmic Claims", scale = list(x=list(draw=FALSE)))

#  MODELS
#  Cross-Sectional Model;
Model.1 <- lm(lnClaims ~ lnCover , data=gtermdata)
summary(Model.1)
#  Basic Fixed Effects Model
Model.2 <- lm(lnClaims ~ lnCover + factor(ID), data=gtermdata)
summary(Model.2)
anova(Model.1, Model.2)
#  Random Effects Model
Model.3 <-lme(lnClaims ~ lnCover, data=gtermdata, random=~1|ID, method="ML")
summary(Model.3)
#  CORRELATED ERRORS
Model.5 <- gls(lnClaims ~ lnCover, correlation=corAR1(form=~Year|ID), data=gtermdata)
summary(Model.5)

#  Lagged Dependent Variables
library(plm)
Model.4 <-plm(lnClaims ~ lnCover +lag(lnClaims), data=gtermdata, index=c("ID","Year"),model="pooling")
summary(Model.4)
#  Other PLM Models
summary(plm(lnClaims ~ lnCover , data=gtermdata, index=c("ID","Year"),model="pooling"))
summary(plm(lnClaims ~ lnCover , data=gtermdata, index=c("ID","Year"),model="within"))
summary(plm(lnClaims ~ lnCover , data=gtermdata, index=c("ID","Year"),model="random"))