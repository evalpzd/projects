c.table<-array(data = c(251, 48, 34, 5), dim = c(2,2), dimnames = 
                 list(Primero = c("éxito", "fracaso"),Segundo = c("éxito", "fracaso")))
list(Primero = c("éxito", "fracaso"), Segundo = c("éxito", "fracaso"))  
c.table  # Tabla de contingencia

pi.hat.table<-c.table/rowSums(c.table)
pi.hat.table

alpha<-0.05
pi.hat1<-pi.hat.table[1,1]
pi.hat2<-pi.hat.table[2,1]

# Wald
var.wald<-pi.hat1*(1-pi.hat1) / sum(c.table[1,]) + pi.hat2*(1-pi.hat2) / sum(c.table[2,])
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.wald)

# Agresti-Caffo
pi.tilde1<-(c.table[1,1]+1)/(sum(c.table[1,])+2)
pi.tilde2<-(c.table[2,1]+1)/(sum(c.table[2,])+2)
var.AC<-pi.tilde1*(1-pi.tilde1) / (sum(c.table[1,])+2) + pi.tilde2*(1-pi.tilde2) / (sum(c.table[2,])+2)
pi.tilde1 - pi.tilde2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.AC)

# Como 0 está contenido en el intervalo Agresti-Caffo, no se puede detectar un cambio en 
# la probabilidad de un segundo tiro exitoso que sigue a un primer tiro exitoso/fallido.
# Esto significa que o bien, no hay diferencia, o que si hay una diferencia, ésta no se 
# pudo detectar en la muestra.

# Otra manera de calcular los intervalos sin usar tablas:

w1<-251
n1<-285
w2<-48
n2<-53
alpha<-0.05
pi.hat1<-w1/n1
pi.hat2<-w2/n2
var.wald<-pi.hat1*(1-pi.hat1) / n1 +  pi.hat2*(1-pi.hat2) / n2
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) *  sqrt(var.wald)


##############################################################
# Prueba de Hipótesis para la diferencia de dos probabilidades
prop.test(x = c.table, conf.level = 0.95, correct = FALSE)

# LRT
pi.bar<-colSums(c.table)[1]/sum(c.table)
log.Lambda<-c.table[1,1]*log(pi.bar/pi.hat.table[1,1]) + c.table[1,2]*log((1-pi.bar)/(1-pi.hat.table[1,1])) +
  c.table[2,1]*log(pi.bar/pi.hat.table[2,1]) + c.table[2,2]*log((1-pi.bar)/(1-pi.hat.table[2,1]))
test.stat<--2*log.Lambda
crit.val<-qchisq(p = 0.95, df = 1)
p.val<-1-pchisq(q = test.stat, df = 1)
round(data.frame(pi.bar, test.stat, crit.val, p.val, row.names = NULL), 4)

library(vcd)
assocstats(x = c.table)

