library(survival)
library(asaur)

survfit(Surv(ttr, relapse) ~ grp, 
             data=pharmacoSmoking) |>
  print() |>
  plot(col=c('blue','red'))

survfit(Surv(ttr, relapse)~grp,
        data=pharmacoSmoking) |>
  plot(fun='cloglog')

# Don't do this in practice
# Just for learning/understanding

beta <- seq(-0.5, 1.5, by=0.01)
psi <- exp(beta)

ll <- sapply(beta,
             function(x){
               coxph(Surv(ttr, relapse) ~ offset(x*(grp=='patchOnly')),
                     data=pharmacoSmoking)$loglik
             })
plot(beta, ll, type='l')
max(ll)
beta[which.max(ll)]
abline(v=beta[which.max(ll)])

plot(psi, ll, type='l')
abline(v=psi[which.max(ll)])
psi[which.max(ll)]


ml.i <- which.max(ll)
null.i <- which(beta==0)

# Wald test

# estimate variance/information with 
# difference of differences

var.est <- -1/diff(diff(ll[(ml.i-1):(ml.i+1)]))*(0.01^2)
cs.stat <- (beta[ml.i] - 0)^2/var.est
cs.stat
pchisq(cs.stat, 1, lower.tail=FALSE)

plot(beta, ll, type='l')
abline(v=beta[c(ml.i, null.i)])


# Score test

# estimate variance/information at null

var.est2 <- -1/diff(diff(ll[(null.i-1):(null.i+1)]))*(0.01^2)

# estimate slope

slope.est <- mean(diff(ll[(null.i-1):(null.i+1)]))/0.01

cs.stat2 <- slope.est^2*var.est2
cs.stat2
pchisq(cs.stat2, 1, lower.tail=FALSE)

plot(beta, ll, type='l')
abline(ll[null.i], slope.est)

## Likelihood Ratio

lr <- 2*(ll[ml.i] - ll[null.i])
lr
pchisq(lr, 1, lower.tail=FALSE)

plot(beta, ll, type='l')
tmp.x <- par('usr')[1]
tmp.y <- par('usr')[3]
lines(c(tmp.x, beta[null.i], beta[null.i]),
      c(ll[null.i], ll[null.i], tmp.y))
lines(c(tmp.x, beta[ml.i], beta[ml.i]),
      c(ll[ml.i], ll[ml.i], tmp.y))

# Do it this way

fit1 <- coxph(Surv(ttr, relapse) ~ grp,
              data=pharmacoSmoking)
fit1
summary(fit1)
confint(fit1)

tmpdat <- data.frame(grp=c("combination", "patchOnly"))

plot(survfit(fit1, tmpdat), col=c('blue','red'))

lines(survfit(Surv(ttr, relapse)~grp, 
              data=pharmacoSmoking),
      col=c('blue','red'), lty=2)


# non-proportional hazard example

set.seed(538)
t1 <- rweibull(100, 1.5, 10)
t2 <- rweibull(100, 1, 5)

mydata <- data.frame(
  time=c(t1,t2),
  status=1,
  group=rep(1:2, each=100)
)

plot(survfit(Surv(time,status)~group, data=mydata),
     col=c('red','blue'))
plot(survfit(Surv(time,status)~group, data=mydata),
     col=c('red','blue'),
     fun='cloglog')

fit <- coxph(Surv(time,status)~group, data=mydata)
summary(fit)

newdat <- data.frame(group=1:2)
plot(survfit(fit, newdat), 
     col=c('red','blue')
     )
lines(survfit(Surv(time,status)~group, data=mydata),
      col=c('red','blue'), lty=2)

plot(survfit(fit, newdat), 
     col=c('red','blue'),
     fun='cloglog'
)
lines(survfit(Surv(time,status)~group, data=mydata),
      col=c('red','blue'), lty=2,
      fun='cloglog')

