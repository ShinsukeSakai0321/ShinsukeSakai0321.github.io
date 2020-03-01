#http://www.mk-mode.com/octopress/2014/02/12/r-mandelbrot/

library(ggplot2)

g <- function(x0, y0) {
  x <- 0
  y <- 0
  for (i in 1:20) {
    xtemp <- x ^ 2 - y ^ 2 + x0
    y <- 2 * x * y + y0
    x <- xtemp
  }
  exp(-(x^2+y^2))
}
m <- 600
X <- seq(-1.8, 0.6, length.out=m)
Y <- seq(-1.2, 1.2, length.out=m)
grid   <- expand.grid(x=X, y=Y)
grid$z <- g(grid$x, grid$y)

#ggplot(grid[grid$z != 0, ], aes(x=x, y=y, fill=z)) + geom_tile()
ggplot(grid, aes(x=x, y=y, fill=z)) + geom_tile()
#enlarged figure

X <- seq(-1.85, -1.65, length.out=m)
Y <- seq(-0.1, 0.1, length.out=m)
grid   <- expand.grid(x=X, y=Y)
grid$z <- g(grid$x, grid$y)

ggplot(grid, aes(x=x, y=y, fill=z)) + geom_tile()
#ggplot(grid[grid$z != 0, ], aes(x=x, y=y, fill=z)) + geom_tile()
