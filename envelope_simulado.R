
## seta o diretório do projeot "wordir"
setwd("~/")
dados = read.delim("~/apps/esquema_fatorial/A_FatCruz_ex2.txt", header = TRUE)
dados
str(dados)

hist(dados$Y)

A = as.factor(dados$A)
B = as.factor(dados$B)

str(A)
str(B)

modelo = with(dados, aov(Y~A))
summary(modelo)

## montagem do envolope simulado
hnp::hnp(modelo, sim=1000, las=1, seed=1, pch=16)
