

eq1 <- function(x){(20*x*x)/(x*(x+10))}

plot(eq1,1,100, xlab = "Time", ylab="Mean response", xaxt="n", yaxt="n", main = "Vocabulary size difference and same retrieval rate")


eq2 <- function(x){(15*x*x)/(x*(x+10))}

plot(eq2,1,100, xlab = "", ylab="", xaxt="n", yaxt="n", add=T)


plot(eq1,1,100, xlab = "Time", ylab="Mean response", xaxt="n", yaxt="n", main = "Same vocabulary size and different retrieval rate")

eq3 <- function(x){
  (23*x*x)/(x*(x+30))
}

plot(eq3,1,100, xlab = "", ylab="", xaxt="n", yaxt="n", add=T)
