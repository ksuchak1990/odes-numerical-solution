th <- 1/3
library(deSolve)

#### SI triangle network ####
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

#### SI line network ####

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

#### SIS triangle network ####

sistrilin <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * (y[1] + y[2] + y[3])
    dy2 <- g * (y[5] + y[6]) - (2 * b + g) * y[2]
    dy3 <- g * (y[5] + y[7]) - (2 * b + g) * y[3]
    dy4 <- g * (y[6] + y[7]) - (2 * b + g) * y[4]
    dy5 <- g * y[8] + b * (y[2] + y[3]) - 2 * (b + g) * y[5]
    dy6 <- g * y[8] + b * (y[2] + y[4]) - 2 * (b + g) * y[6]
    dy7 <- g * y[8] + b * (y[3] + y[4]) - 2 * (b + g) * y[7]
    dy8 <- 2 * b * (y[5] + y[6] + y[7]) - 3 * g * y[8]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.5, g=0.01)
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode (times = times, y = yini, func = sistrilin, parms = parms)
png(filename = "./sis_trinet.png")
plot(out)
dev.off()
