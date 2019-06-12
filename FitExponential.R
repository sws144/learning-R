
# exponential fit
# https://www.theanalysisfactor.com/r-tutorial-5/

# part 5
A <- structure(list(
        Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
              14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
        Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
              46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
              22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), 
        .Names = c("Time", "Counts"), 
        row.names = c(1L, 2L, 
           3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 
           20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L,  31L), 
        class = "data.frame")

attach(A)
names(A)

exponential.model <- lm(log(Counts) ~ Time)

summary(exponential.model)

timevalues <- seq(0,30,0.1)
counts.exponential2 <- exp(predict(exponential.model, list(Time=timevalues)))
plot(Time,Counts,pch=16)
lines(timevalues,counts.exponential2,lwd=2, col = "red", xlab="Time (s)", ylab = "Counts")

# part 6
x <- seq(-4,4,0.2) ; y <- 2*x^2 + 4*x -7; plot(x,y)
plot(x,y,pch=16,col="red")

rug(x)


# part 7  https://www.theanalysisfactor.com/r-tutorial-7/

X <- c(3, 4, 6, 6, 7, 8, 9, 12)
B1 <- c(4, 5, 6, 7, 17, 18, 19, 22)
B2 <- c(3, 5, 8, 10, 19, 21, 22, 26)

plot(X, B1, type="o", pch = 17, cex=1.2, col="darkgreen", ylim=c(0, 25))

lines(B2, type="o", pch=16, lty=2, col="blue")

title(main="MY FIRST R PLOT", col.main="blue", font.main=2)

B1 <- c(1, 2, 4, 5, 7, 12, 14, 16, 19)
B2 <- c(2, 3, 6, 7, 8, 9, 11, 12, 18)
B3 <- c(0, 1, 7, 3, 2, 2, 7, 9, 13)

yaxismax<- max(B1, B2, B3)
yaxismax

plot(B1, pch = 15, type="o", col="blue", ylim=c(0, yaxismax),axes=FALSE, ann=FALSE)
axis(1, at=1:9, lab=c("A","B","C","D","E", "F", "G","H", "I"))

axis(2, las=1, at=2*0: yaxismax)

box()
lines(B2, pch = 16, type="o", lty=2, col="red")
lines(B3, pch = 17, type="o", lty=3, col="darkgreen")

title(main="MY LINE PLOTS IN R", col.main="red", font.main=2)

title(xlab=toupper("ALPHABET"), col.lab="purple")
title(ylab="NUMERALS", col.lab="brown")

legend(1, yaxismax, c('B1','B2', 'B3'), cex=0.7, col=c('blue', 'red1', 'darkgreen'),
       pch=c(15, 16, 17), lty=1:3)
       

