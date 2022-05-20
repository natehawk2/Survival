fit2 <- coxph(Surv(ttr, relapse) ~ 
                grp + gender + employment +
                yearsSmoking + levelSmoking +
                age + priorAttempts +
                longestNoSmoke,
              data=pharmacoSmoking)
summary(fit2)

plot(basehaz(fit2, centered = FALSE), 
     type='l')

plot(survfit(fit2))

summary(pharmacoSmoking)
new.df <- data.frame(
  grp=levels(pharmacoSmoking$grp),
  gender="Female",
  employment='ft',
  yearsSmoking=30,
  levelSmoking='heavy',
  age=49,
  priorAttempts=2,
  longestNoSmoke=90
)

survfit(fit2, newdata = new.df) |>
  plot(col=c('blue','red')) 

