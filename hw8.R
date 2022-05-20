# Wald deaths
expLogMeanDeaths <- function(Delta, alpha, pwr) {
  z.alpha <- qnorm(alpha, lower.tail=F)
  z.beta <- qnorm(1-pwr, lower.tail=F)
  num <- (z.alpha + z.beta)^2
  denom <- (log(Delta))^2
  dd <- num/denom
  dd }

#Likelihood Ratio Delta
expLikeRatio <- function(d, alpha, pwr) {
  num <- qchisq(alpha, df=(2*d), lower.tail=F)
  denom <- qchisq(pwr, df=(2*d), lower.tail=F)
  Delta <- num/denom
  Delta }

#Likelihood Ratio Deaths
expLRdeaths <- function(Delta, alpha, pwr) {
  LRD <- function(x, alpha, pwr)
    expLikeRatio(x, alpha, pwr) - Delta
  result <- uniroot(f=LRD, lower=1,upper=1000,
                    alpha=alpha, pwr=pwr)
  result$root }



expLRdeaths(1/0.7, 0.05, 0.9)
expLogMeanDeaths(1/0.7, 0.05, 0.9)

