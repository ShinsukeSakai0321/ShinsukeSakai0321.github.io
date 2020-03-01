#Newton-Raphson method
f <- function (x) x^3 - 2  #definition of function
aa <- uniroot(f, c(0, 2))   #find the solution for the range (0,2)
aa
curve(f, 0,2, n=100)
abline(h=0)
points(aa$root, 0, col = 2, pch = 19)
