# install.packages("sn")
# install.packages("fGarch")
# install.packages("hnp")

# packageDescription("sn")
# packageDescription("fGarch")
# packageDescription("hnp")
rm(list=ls(all=TRUE))
  
require (sn)
require (fGarch)
require (hnp)
require(gamlss)

setwd("~/")
dados = read.table("~/apps/esquema_fatorial/A_FatCruz_ex2.txt", header = TRUE)
y = dados$Y
y

# montagem do modelo
m1 = gamlss(y ~ 1, family = "NO", data = dados)
summary(m1)
# captura dos residuos do modelo
r = residuals(m1)
shapiro.test(r)


res_quant <- function(obj){
  qnorm(pNO(q     = obj$y, 
            mu    = obj$mu.fv, 
            sigma = obj$sigma.fv))
}

d.fun <- function(obj) res_quant(obj) # this is the default if no

s.fun <- function(n, obj) {
  mu <- obj$mu.fv
  sig <- obj$sigma.fv
  rNO(n, mu=mu, sigma=sig)
}

# fitfun
my.data <- data.frame(y)
f.fun <- function(y.) gamlss(y. ~ 1, family= "NO", data=my.data)

# hnp call
my.hnp <- hnp(m1,newclass=TRUE, diagfun=d.fun, simfun=s.fun,
              fitfun=f.fun,halfnormal = F, print.on=TRUE, plot=FALSE, data=my.data)

plot(my.hnp, main="Distribuição Normal", xlab="Half-normal scores",
     ylab="Quantiles residuals", legpos="topleft")
