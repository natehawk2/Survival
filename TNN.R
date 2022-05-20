needle = read.csv("CSVforSurvivalAnalysis.csv")
head

hist(needle$Bends)
dim(needle)

library(survival)
kp_model = survfit(Surv(Bends) ~ Gender, data = needle)


plot(kp_model, lwd = 2, conf.int = FALSE, col = "blue")
lines(kp_model2alt, col = "blue", lwd = 2, conf.int = FALSE)
text(1000, 0.5, "Standard Model", col = "black")
text(500, 0.1, "Left-truncated Model", col = "blue")

fit1 = coxph(Surv(Bends)~ Gender ,
      data=needle)

plot(survfit(Surv(Bends)~Gender, data=needle),
     col=c('red','blue'), 
     xlab = "Bends", 
     ylab = "Risk", main = "Bends Until Failure", 
     ylim = c(0,1))
text(25,0.5, "Male",col = "blue")
text(45,0.9, "Female",col = "red")

library(survminer)

fit1 = coxph(Surv(Bends)~ Gender + Lot,
             data=needle)

fit <- survfit(Surv(Bends)~ Gender, data = needle)

survminer::ggsurvplot(survfit(fit1), data = needle)

ggsurvplot(
  fit,
  data = needle,
  size = 1,                 # change line size
  palette =
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  #pval = TRUE,              # Add p-value
  risk.table = FALSE,        # Add risk table
  risk.table.col = "Gender",# Risk table color by groups
  legend.labs =
    c("Female", "Male"), 
  legend = "bottom", # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw() ,
  xlim = c(0,55)# Change ggplot2 theme
)

fit1 <- survfit(Surv(Bends)~ 1, data = needle)

ggsurvplot(
  fit1,
  data = needle,
  size = 1,                 # change line size
  palette =
    c("#2E9FDF"),           # custom color palettes
  conf.int = TRUE,          # Add confidence interval
  #pval = TRUE,              # Add p-value
  risk.table = FALSE,        # Add risk table

  legend = "none", # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw() ,
  xlim = c(0,55)# Change ggplot2 theme
)

plot(survfit(Surv(Bends)~Lot, data=needle),
     col=c('red','blue'), 
     xlab = "Bends", 
     ylab = "Risk", main = "Bends Until Failure", 
     ylim = c(0,1))
text(25,0.5, "Lot 1",col = "blue")
text(40,0.95, "Lot 2",col = "red")


fit1 = coxph(Surv(Bends)~ 1,
             data=needle)




mu=8
mu0=6
sd=3
alpha=0.05
beta=0.01
(n=(sd*(qnorm(1-alpha)+qnorm(1-beta))/(mu-mu0))^2)
ceiling(n)

#To have 99% power and detect a difference of at least 2 with sd = 3, we need 36 samples

t.test(needle$Bends, mu = 6, alternative = "greater")

qnorm(0.01, mean = mean(needle$Bends[needle$Gender == "Standard"]), 
      sd = sd(needle$Bends[needle$Gender == "Standard"]))

qnorm(0.01, mean = mean(needle$Bends[needle$Gender == "Female"]), 
      sd = sd(needle$Bends[needle$Gender == "Female"]))

qnorm(0.0001, mean = mean(needle$Bends), 
      sd = sd(needle$Bends))

xseq = seq(0,60, by = 1)
curve(dnorm(x, mean = mean(needle$Bends), sd = sd(needle$Bends)), from = 0, to = 80,
      ylab = "Probability", 
      xlab = "Number of Bends", 
      main = "At least 16 breaks with 99% confidence")
abline(v = 8, col = "red", lty = 2)
abline(h = 0)
text(10,0.02, "99th \npercentile")

summary(fit1)
predict(fit1, newdata = data.frame(Bends = 2))



#create plot of probability mass function
plot(xseq, dpois(xseq, lambda=mean(needle$Bends)), type='h',
     ylab = "Probability", 
     xlab = "Number of Bends", 
     main = "At least 19 breaks with 99.99% confidence")
abline(v = 19, col = "red", lty = 2)
abline(h = 0)
text(10,0.02, "99.99 \npercentile", col = "red")

qpois(0.0001, lambda = mean(needle$Bends))

plot(density(needle$Bends), xlim = c(0,60))





### Bootstrap
hist(needle$Bends, breaks = 20)
sample(1:48, 30, replace = TRUE)
model = survreg(Surv(Bends) ~ 1, data = needle, dist = "exponential")
model$scale
model$terms
curve(dexp(x, rate = 1/scale), from = 0, to = 60)





tmp.coef <- summary(fit1)$table[,1]

fitc <- coxph(Surv(Bends) ~ 1,
              data=needle)
summary(fitc)


plot(survfit(Surv(Bends)~1, data=needle),
     col=c('red','blue'), 
     xlab = "Bends", 
     ylab = "Risk", main = "Bends Until Failure", 
     ylim = c(0,1))
text(25,0.5, "Male",col = "blue")
text(45,0.9, "Female",col = "red")



curve(pweibull(x, exp(-tmp.coef[2]), exp(tmp.coef[1]), lower=FALSE),
      add=TRUE, col='purple', lty=3, lwd = 3)
mean(needle$Bends)

qt(0.01, mean = mean(needle$Bends), sd = sd(needle$Bends))

mins = rep(NA, 1000)
for(i in 1:1000){
  samp = sample(1:48, 30, replace = TRUE)
  train = needle[samp,]
  fit1 = survreg(Surv(Bends) ~ 1, train, dist='weibull')
  tmp.coef <- summary(fit1)$table[,1]
  
  draws = rweibull(1, exp(-tmp.coef[2]), exp(tmp.coef[1]))

  mins[i] = min(draws)
}

plot(density(mins))

quantile(mins, 0.001)

df = 100
xbar = mean(mins)
zalpha = qt(0.975, df = df-1)
s2 = sum((mins - xbar)^2)/(df-1)
xbar + zalpha*s2/sqrt(df)
xbar - zalpha*s2/sqrt(df)

