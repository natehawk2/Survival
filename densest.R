library(survival)
library(logspline)
library(locfit)
library(lubridate)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("faithful_cens.R")
str(faithful.cens)

plot(density(geyser$duration), type='l', ylim=c(0,0.8))
rug(jitter(geyser$duration[geyser$duration %in% 2:4]))
lines(density(geyser$duration, adjust=1/2), col='blue')
lines(density(geyser$duration, adjust=1/3), col='forestgreen')
lines(density(faithful.cens$uncensored, adjust=1/2), col='red')


## logspline package ----

g1 <- oldlogspline(geyser$duration, 
                   lbound=0) # original data
g2 <- oldlogspline(faithful.cens$uncensored, 
                   lbound=0) # uncensored only
g3 <- oldlogspline(uncensored = faithful.cens$uncensored,
                   right=faithful.cens$right,
                   left=faithful.cens$left,
                   interval=faithful.cens$int,
                   lbound=0)

plot(g1)
plot(g2, add=TRUE, col='blue')
plot(g3, add=TRUE, col='forestgreen')

plot(g3, what='p')
plot(g3, what='s', add=TRUE, col='red')

plot(g3, what='h')

## locfit package ----

faithful2 <- rbind(data.frame(duration=faithful.cens$uncensored, stat=0),
                   data.frame(duration=faithful.cens$right, stat=1))

lf1 <- locfit(~lp(duration), data=geyser)
lf2 <- locfit(~lp(duration), data=faithful2, cens=stat)

plot(lf1)
plot(lf2, add=TRUE, col='blue')

plot(g3, col='red')
plot(lf2, add=TRUE)

## Stan Mixture, no censoring ----

stan1 <- stan_model("normmix1.stan")

standat1 <- list(K=2, 
                 N=length(faithful.cens$uncensored),
                 y=faithful.cens$uncensored)

fit1 <- sampling(stan1, standat1)
standat1.3 <- standat1
standat1.3$K <- 3
fit1.3 <- sampling(stan1, standat1.3)

stan_dens(fit1.3, separate_chains = TRUE)

tmp <- summary(fit1)$summary

curve(tmp[1,1]*dnorm(x, tmp[3,1], tmp[5,1]) +
        tmp[2,1]*dnorm(x, tmp[4,1], tmp[6,1]),
      from=0, to=6)
lines(density(standat1$y), col='green')

## Add Censored ----

stan1c <- stan_model("normmix1c.stan")

standat1c <- standat1
standat1c$N_left <- length(faithful.cens$left)
standat1c$y_left <- faithful.cens$left
standat1c$N_right <- length(faithful.cens$right)
standat1c$y_right <- faithful.cens$right

fit1c <- sampling(stan1c, standat1c)

tmp <- summary(fit1c)$summary

curve(tmp[1,1]*dnorm(x, tmp[3,1], tmp[5,1]) +
        tmp[2,1]*dnorm(x, tmp[4,1], tmp[6,1]),
      from=0, to=6)
plot(g3, add=TRUE, col='green')

## Below is the result of a bunch of experimenting
## You are welcome to try it out, this will not be on 
## any tests.


migraine <- read.csv("Migraine.csv")
migraine$Dates <- ymd(migraine$Dates)

tmpfun <- function(df) {
  tmp <- df$Dates
  tmp <- c(tmp, ymd("2017-01-01"))
  days <- as.numeric(diff(tmp))
  cens <- rep(1, length(days))
  cens[length(days)] <- 0
  data.frame(id=df$id[1], days=days, cens=cens, 
             treat=df$treat[1], sex=df$sex[1])
}

migraine2 <- migraine |>
  split(migraine$id) |>
  lapply(tmpfun) |>
  do.call(what=rbind)

migraine3 <- migraine2[migraine2$treat=='c' &
                         grepl('\\.1$', rownames(migraine2)),]


plot(density(migraine3$days), type='l')

standat1.2 <- list(K=3, N=nrow(migraine3),
                   y=migraine3$days)
fit1.2 <- sampling(stan1, standat1.2)

tmp <- summary(fit1.2)$summary

curve(tmp[1,1]*dnorm(x, tmp[4,1], tmp[7,1]) +
        tmp[2,1]*dnorm(x, tmp[5,1], tmp[8,1])+
        tmp[3,1]*dnorm(x, tmp[6,1], tmp[9,1]),
      from=0, to=25)
lines(density(standat1.2$y, adjust=0.75), col='green')

stan_dens(fit1.3, separate_chains = TRUE)

stan3 <- stan_model('normmix3.stan')

fit3 <- sampling(stan3, standat1.2)

tmp <- summary(fit3)$summary

curve(tmp[1,1]*dnorm(x, tmp[4,1], tmp[7,1]) +
        tmp[2,1]*dnorm(x, tmp[5,1], tmp[7,1])+
        tmp[3,1]*dnorm(x, tmp[6,1], tmp[7,1]),
      from=0, to=25)
lines(density(standat1.2$y), col='green')

## fixed mu

normsum <- function(x, mu, sigma, theta=1/length(mu)) {
  sapply(x, function(x){
    sum(dnorm(x,mu,sigma)*theta)
  })
}

curve(normsum(x, 1:10, 1), from=0, to=15)
integrate(normsum, -Inf, Inf, mu=1:10, sigma=1)

stan4 <- stan_model('normmix4.stan')

standat4 <- list(
  K=25,
  N=nrow(migraine3),
  y=migraine3$days,
  mu=1:25,
  sigma=1
)


fit4 <- sampling(stan4, standat4)

tmp <- summary(fit4)$summary

hist(migraine3$days, breaks=0:26, prob=TRUE)
curve(normsum(x, 1:25, 1, tmp[1:25, 1]), add=TRUE, col='purple')
curve(normsum(x, 1:25, 1, tmp[1:25, 6]), add=TRUE, col='blue')
lines(density(standat4$y, adjust=0.7), col='green')

standat4.1 <- list(
  K=13,
  N=standat1$N,
  y=standat1$y,
  mu=seq(0,6, by=0.5),
  sigma=0.5
)

fit4.1 <- sampling(stan4, standat4.1)

tmp <- summary(fit4.1)$summary

hist(faithful.cens$uncensored, prob=TRUE)
curve(normsum(x, standat4.1$mu, 0.5, tmp[1:13, 1]), add=TRUE, col='blue')
lines(density(standat4.1$y, adjust=0.7), col='green')
