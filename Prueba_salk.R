# Tabla de contingencia
c.table<-array(data = c(57, 142, 200688, 201087), dim = c(2,2), dimnames = list(Treatment = c("vacuna", "placebo"),
                                                                                Result = c("polio", "sin polio")))
c.table

# Calcular los valores de pi^j
pi.hat.table<-c.table/rowSums(c.table)
pi.hat.table

sum(pi.hat.table[1,])
pi.hat1<-pi.hat.table[1,1]
pi.hat2<-pi.hat.table[2,1]


#####################################################################
# Intervalo de confianza para la diferencia de proporciones

alpha<-0.05

# Wald
pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) *
  sqrt(pi.hat1*(1-pi.hat1) / sum(c.table[1,]) +
         pi.hat2*(1-pi.hat2) / sum(c.table[2,]))

# Agresti-Caffo
pi.tilde1<-(c.table[1,1]+1)/(sum(c.table[1,])+2)
pi.tilde2<-(c.table[2,1]+1)/(sum(c.table[2,])+2)
pi.tilde1 - pi.tilde2 + qnorm(p = c(alpha/2, 1-alpha/2)) *
  sqrt(pi.tilde1*(1-pi.tilde1) / (sum(c.table[1,])+2) +
         pi.tilde2*(1-pi.tilde2) / (sum(c.table[2,])+2))


####################################################
# Riesgo Relativo

# RR donde éxito = "polio"
round(pi.hat1/pi.hat2, 4)
round(1/(pi.hat1/pi.hat2), 4)

alpha<-0.05
n1<-sum(c.table[1,])
n2<-sum(c.table[2,])

# Intervalo de confianza de Wald
var.log.rr <- 1/c.table[1,1] - 1/sum(c.table[1,]) + 1/c.table[2,1] - 1/sum(c.table[2,])
ci<-exp(log(pi.hat1/pi.hat2) + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.log.rr))
round(ci, 4)
#rev(round(1/ci, 4))  # invertido

####################################################
# OR

OR.hat<-c.table[1,1]*c.table[2,2] / (c.table[2,1]*c.table[1,2])
round(OR.hat, 4)
round(1/OR.hat, 4)

alpha<-0.05
var.log.or<-1/c.table[1,1] + 1/c.table[1,2] + 1/c.table[2,2] + 1/c.table[2,1]
OR.CI<-exp(log(OR.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.log.or))
round(OR.CI, 4)
#rev(round(1/OR.CI, 4))
