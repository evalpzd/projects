
wheat <- read.csv("wheat.csv")

# Gráficas
###########
# Gráfica de "Estrellas"
x11(width = 7, height = 6, pointsize = 12)

stars(x = wheat[order(wheat$type),-1], ncol = 20, key.loc = c(10, 0), 
      draw.segments=TRUE, label = NULL, cex=0.75)
################################
# Gráfica de coordenadas paralelas

library(MASS) 

x11(width = 10, height = 6, pointsize = 12)

# Redefiniendo los datos para la variable "class"

wheat2<-data.frame(kernel = 1:nrow(wheat), wheat[,2:6],  
                   class.new = ifelse(test = wheat$class == "hrw",
                                      yes = 0, no = 1))
head(wheat2)

# Colores por condición:
wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                     no = ifelse(test = wheat$type=="Sprout", yes = "red", 
                                 no = "green"))
# Tipo de recta por condición:
wheat.lty<-ifelse(test = wheat$type=="Healthy", yes = "solid", 
                  no = ifelse(test = wheat$type=="Sprout", yes = "longdash", no = "dotdash"))
# pdf(file = "c:\\figures\\Figure3.2color.pdf", width = 7, height = 6, colormodel = "cmyk")  
parcoord(x = wheat2, col = wheat.colors, lty = wheat.lty)  # Plot
legend(x = 6.15, y = 0.75, legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"),
       col=c("black", "red", "green"), cex=0.8, bty="n")

# Observación con el menor peso
wheat[wheat$weight == min(wheat2$weight),]  # 269
order(wheat$size)  # 269 es el segundo grano más pequeño

# Para resaltar la observación #269 en la gráfica
wheat[269,]  # scab
wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                     no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
wheat.colors[269]<-"purple"
line.width<-c(rep(x = 1, times = 268), 10, rep(x = 1, times = 6))            
parcoord(x = wheat2, col = wheat.colors, lwd = line.width, lty = wheat.lty, 
         main = "Gráfica de coordenadas paralelas - grano 269 resaltado")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab", "Kernel 269"), lty = c("solid", "longdash", "dotted", "dotted"), 
       col=c("black", "red", "green", "purple"), cex=0.75, bty="n", lwd = c(1, 1, 1, 10)) 

# Ordenar por tipo de grano
wheat.colors2<-ifelse(test = wheat$type=="Healthy", yes = 1, 
                      no = ifelse(test = wheat$type=="Sprout", yes = 2, no = 3))
wheat3<-data.frame(wheat.colors2, wheat2)     
x11(width = 7, height = 7, pointsize = 9)         
parcoord(x = wheat3[order(wheat.colors2),], col = wheat.colors[order(wheat.colors2)], 
         main = "Gráfica de coordenadas paralelas - clasificado por tipo")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c(1,1,1), col=c("black", "red", "green"), cex=1, bty="n") 

# Forma alterna
# install.packages("iplots")
library(iplots)  
ipcp(wheat3[order(wheat.colors2),])

###################################################################################
# Análisis de componentes principales

save<-princomp(formula = ~ density + class.new + hardness + size + weight + moisture, data = wheat2, 
               cor = TRUE, scores = TRUE)
summary(save, loadings = TRUE, cutoff = 0.0)

par(pty = "s")
wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                     no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
symbols(x = save$scores[,1], y = save$scores[,2], circles = save$scores[,3]-min(save$scores[,3]), 
        inches=0.25, xlab = "Componente principal 1", ylab = "Componente Principal 2", fg = wheat.colors, 
        main = "Gráfica de burbujas")  
abline(h = 0, lty = 1, lwd = 2)  
abline(v = 0, lty = 1, lwd = 2)  
text(x = save$scores[,1], y = save$scores[,2], col = 2, cex = 0.5) 
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), pch = c(1,1,1), 
       col=c("black", "red", "green"), cex=1, bty="n") 


###################################################################################
# Modelo de regresión multinomial  

library(nnet)

levels(wheat$type)  

# Estimación del modelo
mod.fit<-multinom(formula = type ~ class + density + hardness + size + weight + moisture, data=wheat)
summary(mod.fit)

# Revisar las funciones "método"
class(mod.fit)
methods(class = multinom)
sqrt(vcov(mod.fit)[2,2])


################################
# Prueba de hipótesis

# Prueba de Wald
sum.fit<-summary(mod.fit)
test.stat<-sum.fit$coefficients/sum.fit$standard.errors
p.value<-2*(1-pnorm(q = abs(test.stat), mean = 0, sd = 1))
round(test.stat,2)
round(p.value,2)

# LRT para class:
mod.fit.Ho<-multinom(formula = type ~ density + hardness + size + weight + moisture, data=wheat)
anova(mod.fit.Ho, mod.fit)   
library(car)
Anova(mod.fit)
qchisq(p = 0.95, df = 2)

# Información adicional
G.sq.Ho<-mod.fit.Ho$deviance 
G.sq.Ha<-mod.fit$deviance 
G.sq<-G.sq.Ho-G.sq.Ha
p.value<-1-pchisq(q = G.sq, df = 2)
data.frame(G.sq.Ho, G.sq.Ha, G.sq, p.value, df = 2)


################################
# Probabilidad estimada de pertenecer a una categoría

# pi^
pi.hat<-predict(object = mod.fit, newdata = wheat, type = "probs")
head(pi.hat)

head(predict(object = mod.fit, newdata = wheat, type = "class"))


# Predicción con base en "class"
predict(object = mod.fit, newdata = wheat[1,], type = "class")

##############################
# Intervalos de confianza para pi_j

# Valores observados
x1<-0;          x2<-wheat[1,2]; x3<-wheat[1,3]
x4<-wheat[1,4]; x5<-wheat[1,5]; x6<-wheat[1,6]

# Cadenas de caracteres
scab<-"exp(b20 + b21*x1 + b22*x2 + b23*x3 + b24*x4 + b25*x5 + b26*x6)"
sprout<-"exp(b30 + b31*x1 + b32*x2 + b33*x3 + b34*x4 + b35*x5 + b36*x6)"

# pi^_Healthy

g.healthy<-paste("1 / (1 + ", scab, "+", sprout, ")")
g.healthy
calc.healthy<-deltaMethod(object =  mod.fit, g = g.healthy,
                          parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                             "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
names(calc.healthy)
calc.healthy$Estimate  # pi^_Healthy
calc.healthy$SE        # sqrt(Var^(pi^_Healthy))
alpha<-0.05
calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.healthy$SE

# pi^_Scab
g.scab<-paste(scab, "/ (1 + ", scab, "+", sprout, ")")
g.scab
calc.scab<-deltaMethod(object =  mod.fit, g = g.scab,
                       parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                          "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
calc.scab
calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.scab$SE

# pi^_Sprout
g.sprout<-paste(sprout, "/ (1 + ", scab, "+", sprout, ")")
g.sprout
calc.sprout<-deltaMethod(object =  mod.fit, g = g.sprout,
                         parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                            "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
calc.sprout
calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.sprout$SE


################################
# Revisar contenido de mod.fit

names(mod.fit)  # No existe objeto "coefficients" - usar coefficients(mod.fit) 

mod.fit$deviance
mod.fit$convergence
head(mod.fit$fitted.values) 

# Resíduos
head(mod.fit$residuals, n = 3)
head(residuals(mod.fit), n = 3)
c(1,0,0) - mod.fit$fitted.values[1,]  


################################
# Gráfica del modelo que sólo considera "density" como variable independiente

x11(width = 7, height = 6, pointsize = 12)

# Estimación del modelo
mod.fit.nom.density<-multinom(formula = type ~ density, data = wheat)
summary(mod.fit.nom.density)
beta.hat<-coefficients(mod.fit.nom.density)

#  pdf(file = "c:\\figures\\Figure3.3color.pdf", width = 7, height = 6, colormodel = "cmyk")   
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))
# Graficar cada pi_j
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout
legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
       col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)
# dev.off()  

# Verificación
density.values<-seq(from = 0.8, to = 1.6, by = 0.1)
data.frame(density.values, round(predict(object = mod.fit.nom.density, newdata = data.frame(density = density.values), type = "probs"), 2))

################################
# Información adicional

# Modelo con predictor no lineal
mod.fit.trans1<-multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat)  # No converge
summary(mod.fit.trans1)
mod.fit.trans2<-multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat,
                         maxit = 1000)  # Convergencia
summary(mod.fit.trans2)

# Cambio de nivel base
wheat.original<-wheat  # guardar el formato original
wheat$type<-relevel(x = wheat$type, ref = "Sprout") 
levels(wheat$type)
mod.fit.relevel<-multinom(formula = type ~ class + density + hardness + size + weight + moisture, data=wheat)
summary(mod.fit.relevel)
wheat<-wheat.original  # Regresar al formato original

################################
# Razón de Momios


summary(wheat)
sd.wheat<-apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
c.value<-c(1, sd.wheat)  # class = 1 es el primer valor
round(c.value,2)

# beta.hat_jr para r = 1, ..., 6  y j = 2, 3
beta.hat2<-coefficients(mod.fit)[1,2:7]
beta.hat3<-coefficients(mod.fit)[2,2:7]

# Razón de Momios para j = 2 vs. j = 1 (scab vs. healthy)
round(exp(c.value*beta.hat2),2)
round(1/exp(c.value*beta.hat2),2)

# Razón de Momios para j = 3 vs. j = 2 (sprout vs. healthy)
round(exp(c.value*beta.hat3),2)
round(1/exp(c.value*beta.hat3),2)

# Intervalos de confianza de Wald 
conf.beta<-confint(object = mod.fit, level = 0.95)
round(conf.beta,2)  # Los resultados son guardados en un arreglo de 3 vias
conf.beta[2:7,1:2,1]  # C.I.s para beta_2r
conf.beta[2:7,1:2,2]  # C.I.s para beta_3r

c.value*conf.beta[2:7,1:2,1]
c.value[2]*conf.beta[2,1:2,1]

# Intervalos de confianza para OR
ci.OR2<-exp(c.value*conf.beta[2:7,1:2,1])
ci.OR3<-exp(c.value*conf.beta[2:7,1:2,2])  

round(data.frame(low = ci.OR2[,1], up = ci.OR2[,2]), 2)
round(data.frame(low = 1/ci.OR2[,2], up = 1/ci.OR2[,1]), 2)[c(2,5),]  

round(data.frame(low = ci.OR3[,1], up = ci.OR3[,2]), 2)
round(data.frame(low = 1/ci.OR3[,2], up = 1/ci.OR3[,1]), 2)[c(2,3),]  

###################################################################################
# Modelos de momios proporcionales 

library(MASS)  

# Reordenando tal que Scab < Sprout < Healthy
levels(wheat$type)
wheat$type.order<-factor(wheat$type, levels = c("Scab",  "Sprout", "Healthy"))
head(wheat)
levels(wheat$type.order)
# Manera alterna:
# ordered(x = wheat$type, levels = c("Scab",  "Sprout", "Healthy"))  #Also will do ordering
# within(data = wheat, expr = type.ordered<-ordered(x = wheat$type, levels = c("Scab",  "Sprout", "Healthy")))


# Modelo con todas las variables independientes
mod.fit.ord<-polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
class(mod.fit.ord)
summary(mod.fit.ord)
library(car)  
Anova(mod.fit.ord)

################################
# Calcular la probabilidad de caer en una categoría en particular

pi.hat.ord<-predict(object = mod.fit.ord, type = "probs")
head(pi.hat.ord)

# Ejemplo con argumento newdata
predict(object = mod.fit.ord, newdata = wheat[1,], type = "probs")

# Predicción de las clasificaciones
head(predict(object = mod.fit.ord, type = "class"))

# función delthaMethod.pol2
deltaMethod.polr2<-function(object, g)  {
  beta.hat<-c(-object$coefficients, object$zeta)
  
  numb.slope<-length(object$coefficients)
  numb.int<-length(object$zeta)
  
  names(beta.hat)<-c(paste("b", 1:numb.slope, sep=""), paste("b", 1:numb.int, "0", sep=""))
  
  cov.mat<-vcov(object)
  cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]<-
    -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
  cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]<-
    -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]
  
  deltaMethod(object = beta.hat, g = g, vcov. = cov.mat)
}

# La función deltaMethod.polr2 no tiene un argumento para indicar 
# el nombre de los parámetros, éstos son creados durante la ejecución de 
# la función
x1<-0;          x2<-wheat[1,2]; x3<-wheat[1,3]
x4<-wheat[1,4]; x5<-wheat[1,5]; x6<-wheat[1,6]

alpha<-0.05

# Cadenas de caracteres
scab<-"exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"
sprout<-"exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"

# pi^_Scab
g.scab<-paste(scab, "/ (1 + ", scab, ")")
g.scab
calc.scab<-deltaMethod.polr2(object = mod.fit.ord, g = g.scab)
calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.scab$SE

# pi^_Sprout
g.sprout<-paste(sprout, "/ (1 + ", sprout, ")", " - ", scab, "/ (1 + ", scab, ")")
g.sprout
calc.sprout<-deltaMethod.polr2(object = mod.fit.ord, g = g.sprout)
calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.sprout$SE

# pi^_Healthy
g.healthy<-paste("1 - ", sprout, "/ (1 + ", sprout, ")")
g.healthy
calc.healthy<-deltaMethod.polr2(object = mod.fit.ord, g = g.healthy)
calc.healthy$Estimate  # pi^_Healthy
calc.healthy$SE  # sqrt(Var^(pi^_Healthy))
calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.healthy$SE


################################
# Gráficas

x11(width = 7, height = 6, pointsize = 12)

# Estimación del modelo con densidad como única variable independiente
mod.fit.dens<-polr(formula = type.order ~ density, data = wheat, method = "logistic")
summary(mod.fit.dens)

min(wheat$density)
max(wheat$density)

# pdf(file = "c:\\figures\\Figure3.5color.pdf", width = 7, height = 6, colormodel = "cmyk")   
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))
lwd.mult<-2
# Gráfica de cada pi_j para el modelo de regresión multinomial
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout

lwd.po<-4
# Gráfica de cada pi_j para el modelo de momios proporcionales
curve(expr = plogis(q = mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "green",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Scab"]), max(wheat$density[wheat$type.order == "Scab"])),
      add = TRUE, lty = "dotdash", lwd = lwd.po, n = 1000)  # Scab
curve(expr = plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x) - plogis(q =mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "red",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Sprout"]), max(wheat$density[wheat$type.order == "Sprout"])),
      add = TRUE, lty = "longdash", lwd = lwd.po, n = 1000)  # Sprout
curve(expr = 1 - plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x), col = "black",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Healthy"]), max(wheat$density[wheat$type.order == "Healthy"])),
      add = TRUE, lty = "solid", lwd = lwd.po, n = 1000)  # Healthy
legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
       col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)
# dev.off()  # Create plot for book



################################
# Razones de momios

# Se usa la función sd para calcular el valor del factor c
summary(wheat)
sd.wheat<-apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
c.value<-c(1, sd.wheat)
round(c.value, 2)  # class = 1 es el primer valor

# OR
round(exp(c.value*(-mod.fit.ord$coefficients)),2)
round(1/exp(c.value*(-mod.fit.ord$coefficients)),2)

# Intervalos de confianza
conf.beta<-confint(object = mod.fit.ord, level = 0.95)
conf.beta
c.value*(-conf.beta)
c.value[2]*(-conf.beta[2,])

ci<-exp(c.value*(-conf.beta))
round(data.frame(low = ci[,2], up = ci[,1]), 2)
round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)

# Ejemplo considerando sólo la variable density
confint(object = mod.fit.ord, parm = "density", level = 0.95)  

# Wald
vcov(mod.fit.ord) 
beta.ci<-(-mod.fit.ord$coefficients[2]) + qnorm(p = c(0.025, 0.975))*sqrt(vcov(mod.fit.ord)[2,2])
beta.ci
round(rev(1/exp(beta.ci*c.value[2])),2)


