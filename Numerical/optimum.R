library(GA)
f <- function(x) x^2*sin(x) + x*cos(x)
min <- -5; max <- 10
png("maximum.png",width=500,height=400)
curve(f, min, max, n=100)

#Optimum analysis for evaluating the maximum value
#for the analysis using optim
ans <- optimize(f,lower=min,upper=max,maximum=TRUE)
points(ans$maximum,ans$objective,  col = 4, pch = 19)

#only one line by using GA algorithm!
GA <- ga(type="real-valued", fitness=f, min=min, max=max)
#summary(GA)
png("fitness.png",width=500,height=400)
plot(GA)

points(GA@solution, GA@fitnessValue, col = 2, pch = 19)

dev.off()
