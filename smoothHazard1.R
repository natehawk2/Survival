library(survival)
library(asaur)
library(muhaz)

haz_data <- data.frame(
  time=c(1,2,3,4,5,6,7,7,8,10,12),
  stat=c(0,1,0,1,0,0,1,1,0,1,1)
)

Surv(haz_data$time, haz_data$stat)

fit1 <- survfit(Surv(time, stat)~1, data=haz_data)
sfit1 <- summary(fit1)
str(sfit1)
plot(fit1, cumhaz = TRUE)


# point mass hazard function

haz <- sfit1$n.event/sfit1$n.risk
plot(sfit1$time, haz, xlab='time', ylab='hazard',
     ylim=c(0,1.1), xlim=c(0,13))
lines(c(0, sfit1$time, 13), rep(0, 7), type='c')


## kernel
ep_k <- function(d) {
  3/4*(1-d^2)*(d>-1)*(d<1)
}

b <- 2

plot(sfit1$time, haz,
     xlab='time', ylab='hazard',
     ylim=c(0,1), xlim=c(0,15))

for(i in 1:5) {
  curve(ep_k((x-sfit1$time[i])/b)/b*haz[i],
        col=i+1, add=TRUE)
}

curve(
  ep_k((x-2)/b)/b*haz[1] +
  ep_k((x-4)/b)/b*haz[2] + 
  ep_k((x-7)/b)/b*haz[3] +
  ep_k((x-10)/b)/b*haz[4] +
  ep_k((x-12)/b)/b*haz[5],
  add=TRUE
)

dens1 <- density(sfit1$time, weight=haz,
                 bw=b/2, kernel='ep')
lines(dens1, col='forestgreen')


## Cumulative hazard

mfit1 <- muhaz(haz_data$time, haz_data$stat,
               max.time=12)

plot(mfit1)
points(sfit1$time, haz)

plot(fit1, cumhaz=TRUE)
lines(mfit1$est.grid, 
      cumsum(mfit1$haz.est)*diff(mfit1$est.grid)[1],
      col='blue')

plot(fit1)
lines(mfit1$est.grid,
      exp(-cumsum(mfit1$haz.est)*diff(mfit1$est.grid)[1]),
      col='blue')

## myeloma data

my.dat <- myeloma[myeloma$entry==0,]
fit2 <- survfit(Surv(futime, death) ~ 1, data=my.dat)
sfit2 <- summary(fit2)

haz2 <- sfit2$n.event/sfit2$n.risk
plot(sfit2$time, haz2, xlab='time', ylab='hazard',
     type='h', log='y')

mfit2 <- muhaz(my.dat$futime, my.dat$death)
plot(mfit2, col='blue')

plot(fit2, cumhaz=TRUE)
lines(mfit2$est.grid, 
      cumsum(mfit2$haz.est)*diff(mfit2$est.grid)[1],
      col='blue')

plot(fit2)
lines(mfit2$est.grid,
      exp(-cumsum(mfit2$haz.est)*diff(mfit2$est.grid)[1]),
      col='blue')


