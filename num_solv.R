th <- 1/3
library(deSolve)
trinet <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- 0
    dy2 <- -2  * b * y[2]
    dy3 <- -2  * b * y[3]
    dy4 <- -2  * b * y[4]
    dy5 <- b * (y[2] + y[3] - 2 * y[5])
    dy6 <- b * (y[2] + y[4] - 2 * y[6])
    dy7 <- b * (y[3] + y[4] - 2 * y[7])
    dy8 <- 2 * b * (y[5] + y[6] + y[7])
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.5)
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode (times = times, y = yini, func = trinet, parms = parms)
png(filename = "./trinet.png")
plot(out)
dev.off()

trilin <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- 0
    dy2 <- -b * y[2]
    dy3 <- -2  * b * y[3]
    dy4 <- -b * y[4]
    dy5 <- b * (y[2] + y[3] - y[5])
    dy6 <- -2 * b * y[6]
    dy7 <- b * (y[3] + y[4] - y[7])
    dy8 <- b * (y[5] + 2 * y[6] + y[7])
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.5)
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode (times = times, y = yini, func = trilin, parms = parms)
png(filename = "./trilin.png")
plot(out)
dev.off()

