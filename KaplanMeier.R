library(survival)
library(asaur)

time <- c(1, 2, 3, 4, 5, 6, 7, 8, 12)
cens <- c(0, 1, 0, 1, 0, 0, 1, 0, 1)

fit1 <- survfit(Surv(time, cens) ~ 1)
plot(fit1, mark.time=TRUE)

fit2 <- survfit(Surv(time, status) ~ 1, data=aml)
plot(fit2, mark.time=TRUE)
fit2
summary(fit2)


fit3 <- survfit(Surv(time, status) ~ 1, data=aml,
                stype=2)
plot(fit3, mark.time=TRUE)
fit3
summary(fit3)

plot(fit3, cumhaz=TRUE)


fu <- 1-aml$status
follow <- survfit(Surv(time, 1-status)~1, data=aml)
follow
