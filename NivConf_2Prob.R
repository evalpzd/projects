alpha<-0.05
pi1<-0.2
pi2<-0.4
n1<-10
n2<-10

# Todas las posibles combinaciones de w1 y w2
w.all<-expand.grid(w1 = 0:n1, w2 = 0:n2)

# Todas las posibles combinaciones de pi^_1 y pi^_2
pi.hat1<-(0:n1)/n1
pi.hat2<-(0:n2)/n2
pi.hat.all<-expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)

# Para calcular la probabilidad conjunta de w1 y w2
prob.w1<-dbinom(x = 0:n1, size = n1, prob = pi1)
prob.w2<-dbinom(x = 0:n2, size = n2, prob = pi2)
prob.all<-expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
pmf<-prob.all$prob.w1*prob.all$prob.w2

# Probabilidad conjunta de observar w1 y w2 (i.e., P(W1 = w1, W2 = w2))
head(data.frame(w.all, pmf = round(pmf,4)))
tail(data.frame(w.all, pmf = round(pmf,4)))

# Verdadero nivel de confianza para el intervalo de Wald
var.wald<-pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2
lower<-pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper<-pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1-alpha/2) * sqrt(var.wald)
save<-ifelse(test = pi1-pi2 > lower,
             yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
sum(save*pmf)
#Ejemplo: data.frame(w.all, round(data.frame(pmf, lower, upper),4), save)[1:15,] 

# Verdadero nivel de confianza para el intervalo de Agresti-Caffo
pi1tilde<-(0:n1+1)/(n1+2)
pi2tilde<-(0:n2+1)/(n2+2)
pi.all.tilde<-expand.grid(pi1tilde = pi1tilde, pi2tilde = pi2tilde)
var.ac<-pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
  pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2)
lower.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] - qnorm(p = 1-alpha/2) * sqrt(var.ac)
upper.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] + qnorm(p = 1-alpha/2) * sqrt(var.ac)
save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
sum(save.AC*pmf)
#Ejemplo: data.frame(w.all, round(data.frame(pmf, lower, upper),4), save)[1:15,]  

