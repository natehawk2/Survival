fit1 <- coxph(Surv(time,cens) ~ sex, 
              data=ChanningHouse)
summary(fit1)

library(ggplot2)
ggplot(ChanningHouse, aes(x=entry/12, color=sex)) +
  geom_density()

fit2 <- coxph(Surv(entry, exit, cens) ~ sex, data=ChanningHouse)
summary(fit2)

plot(survfit(fit1, data.frame(sex=c('Female', 'Male'))), 
     col=c('red','blue'))
plot(survfit(fit2, data.frame(sex=c('Female', 'Male'))), 
     col=c('red', 'blue'))

set.seed(538)
tmp1 <- rweibull(1000, shape=1.5, scale=10)
tmp2 <- rweibull(1000, shape=1.5, scale=15)
ggplot(mapping=aes(x=c(tmp1,tmp2), color=rep(c('A','B'), each=1000))) +
  geom_density()

c(mean(tmp1), mean(tmp2))
c(median(tmp1), median(tmp2))

tmp1.2 <- tmp1[tmp1 > 4]
tmp2.2 <- tmp2[tmp2 > 4]

c(length(tmp1.2), length(tmp2.2))

rbind(
  c(mean(tmp1), mean(tmp2)),
  c(mean(tmp1.2), mean(tmp2.2))
)
c(mean(tmp1.2), mean(tmp2.2)) - c(mean(tmp1), mean(tmp2))

rbind(
  c(median(tmp1), median(tmp2)),
  c(median(tmp1.2), median(tmp2.2))
)
ggplot(mapping=aes(x=c(tmp1.2,tmp2.2), 
                   color=rep(c('A','B'), c(length(tmp1.2),length(tmp2.2))))) +
  geom_density()


tmpfun <- function(n=100, shape=1.5, scale=10) {
  time <- rweibull(n, shape, scale)
  trunc <- rweibull(n, shape, scale/2)
  status1 <- rep(1, length(time))
  time2 <- time[time > trunc]
  trunc2 <- trunc[time > trunc]
  status2 <- rep(1, length(time2))
  fit1 <- survfit(Surv(time, status1)~1)
  fit2 <- survfit(Surv(time2, status2)~1)
  fit3 <- survfit(Surv(trunc2, time2, status2)~1)
  c(quantile(fit1, 0.5)[[1]], 
    quantile(fit2, 0.5)[[1]], 
    quantile(fit3, 0.5)[[1]])
}

out <- replicate(1000, tmpfun())
rowMeans(out)
pairs(t(out), asp=1, xlim=range(out), ylim=range(out))
plot(out[1,], out[2,], asp=1)
abline(0,1, col='red')
plot(out[1,], out[3,], asp=1)
abline(0, 1, col='red')

tmpdf <- data.frame(med=c(out[1,], out[2,], out[3,]),
                    model=rep(c("Full", "Wrong", "Trunc"), each=1000))
ggplot(tmpdf, aes(x=med, color=model)) +
  geom_density()
