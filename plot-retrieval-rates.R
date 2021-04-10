


par(mfrow = c(1,2), col.axis = "white")

eq1 <- function(x){(20*x*x)/(x*(x+10))}

plot(eq1,1,100, xlab = "Time", lwd=2,ylab="Mean response", xaxt="n", yaxt="n", main = "(A) Vocabulary size difference and same retrieval rate")


eq2 <- function(x){(15*x*x)/(x*(x+10))}

plot(eq2,1,100, xlab = "", ylab="", xaxt="n", yaxt="n", add=T,lty=2, lwd=2)


plot(eq1,1,100, xlab = "Time", lwd=2, ylab="Mean response", xaxt="n", yaxt="n", main = "(B) Same vocabulary size and different retrieval rate")

eq3 <- function(x){
  (23*x*x)/(x*(x+30))
}

plot(eq3,1,100, xlab = "", ylab="", xaxt="n", yaxt="n", add=T, lty=2, lwd=2)


