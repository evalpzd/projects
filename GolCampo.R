#####################################################################
# Asumiendo que el archivo "Placekick.csv" está en el directorio activo de 
# trabajo.
placekick<-read.table(file = "Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

# Estimación del modelo

mod.fit<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
mod.fit 
names(mod.fit)         # Componentes del objeto mod.fit
mod.fit$coefficients   # Estimados de los parámetros
beta_0.hat <- mod.fit$coefficients[1]
beta_1.hat <- mod.fit$coefficients[2]

#####################################################################
# Estimados de las probabilidades para cada observación
pi.hat <- mod.fit$fitted.values
length(pi.hat) 
length(mod.fit$residuals) # Longitud del vector de residuos "y - pi.hat"
help(glm) # c.f. Sección "Value"

class(mod.fit)
methods(class = glm)  # La clase glm tiene asociadas funciones "method" que ayudan 
                      # a obtener información asociada al modelo.
methods(class = lm)   
summary(mod.fit)      # Al ejecutarse "summary()" R busca primero la clase del
                      # objeto y luego busca en la lista de funciones "method" 
                      # si está definida.

#####################################################################
# Modelos con más de una variable independiente
mod.fit2<-glm(formula = good ~ change + distance, family = binomial(link = logit), 
              data = placekick) 
mod.fit2$coefficients 

#####################################################################
# Estimados de las varianzas y covarianzas

# Tabla de coeficientes
round(summary(mod.fit)$coefficients,4)

# Matriz de los estimadores de covarianza
vcov(mod.fit)
vcov(mod.fit)[2,2]  # Varianza estimada del estimador del parámetro beta1
summary(mod.fit)$coefficients[2,2]^2

#####################################################################
# Maximización de la función de verosimilitud

# Función de verosimilitud
logL<-function(beta, x, Y) {
  pi<-exp(beta[1] + beta[2]*x)/(1+exp(beta[1] + beta[2]*x))
  # Alternatively, could use exp(X%*%beta)/(1+exp(X%*%beta)) where X is the design matrix
  sum( Y*log(pi) + (1-Y)*log(1-pi))
}

# Evaluar la función de verosimilitud en pi.hat
logL(beta = mod.fit$coefficients, x = placekick$distance, Y = placekick$good)
logLik(mod.fit)  # Verificación usando la función "logLik()"

# Calculamos valores iniciales para los estimados de los parámetros.
reg.mod<-lm(formula = good ~ distance, data = placekick)
reg.mod$coefficients
mod.fit.optim<-optim(par = reg.mod$coefficients, fn = logL, hessian = TRUE, x = placekick$distance, Y = placekick$good,
                     control = list(fnscale = -1), method = "BFGS")
names(mod.fit.optim)
mod.fit.optim$par # Estimados de los parámetros
mod.fit.optim$value # Valor máximo de la función de verosimilitud 
mod.fit.optim$convergence # 0 si se alcanzó la convergencia.
solve(mod.fit.optim$hessian) # Matriz de covarianza para los estimados de los
                             # Parámetros.

######################################
# Gráfica de la función de verosimilitud

# Evaluando la función de verosimilitud en distintos valores beta0 y beta1
beta0.values<-seq(from = -5, to = 18, by = 0.1)
beta1.values<-seq(from = -0.65, to = 0.25, by = 0.01)
count<-1
save.logL<-numeric(length(beta0.values)*length(beta1.values))
for (beta0 in beta0.values) {
  for (beta1 in beta1.values) {
    save.logL[count]<-logL(beta = c(beta0, beta1), x = placekick$distance, Y = placekick$good)
    count<-count+1
  }
}
max(save.logL)

library(rgl)  
open3d()  # Ventana de graficación
# Gráfica 3D con retícula
persp3d(x = beta1.values, y = beta0.values, z = save.logL, xlab = "beta1", ylab = "beta0", zlab = "log(L)", ticktype = "detailed", col="red")
grid3d(c("x", "y+", "z"))

# Curvas de nivel
save.logL2<-matrix(save.logL, nrow = length(beta0.values), ncol = length(beta1.values), byrow = T)
x11(width = 7, height = 6, pointsize = 12)
par(pty = "s")
contour(x = beta0.values, y = beta1.values, z = save.logL2,  xlab = 
          expression(beta[0]), ylab = expression(beta[1]), levels = -c(10000, 7500, 5000, 2500, 1000, 750, 500, 450, 400))
abline(h = mod.fit$coefficients[2], lty = "dashed", lwd = 2, col= "red")
abline(v = mod.fit$coefficients[1], lty = "dashed", lwd = 2, col= "red")

#####################################################################
# "Redefiniendo" los datos en forma binomial

# Calculamos la proporción de éxitos para cada distancia
w<-aggregate(formula = good ~ distance, data = placekick, FUN = sum)
n<-aggregate(formula = good ~ distance, data = placekick, FUN = length)
w.n<-data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good/n$good,4))
head(w.n)
tail(w.n)

mod.fit.bin<-glm(formula = success/trials ~ distance, weights = trials, family = binomial(link = logit), data = w.n, trace = TRUE)
summary(mod.fit.bin) 

#####################################################################
# Prueba de hipótesis

library(car)  # Para usar la función Anova() 

# Modelo que incluye "change" y "distance"
mod.fit2<-glm(formula = good ~ change + distance, family = binomial(link = logit), data = placekick)
round(summary(mod.fit2)$coefficients, 4)   # Prueba de Wald 
Anova(mod = mod.fit2, test.statistic="Wald")  # De manera alterna

# LRT
Anova(mod.fit2, test = "LR")
anova(mod.fit2, test = "Chisq")

# Para comparar dos modelos
mod.fit.Ho<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
anova(mod.fit.Ho, mod.fit2, test = "Chisq")

# Prueba de Ho: logit(pi) = beta_0  vs. Ha: logit(pi) = beta_0 + beta_1*change
mod.fit.Ho<-glm(formula = good ~ 1, family = binomial(link = logit), data = placekick)
mod.fit.Ha<-glm(formula = good ~ change, family = binomial(link = logit), data = placekick)
anova(mod.fit.Ho, mod.fit.Ha, test = "Chisq")
pi.hat.Ho<-mod.fit.Ho$fitted.values
pi.hat.Ha<-mod.fit.Ha$fitted.values
y<-placekick$good
stat<--2*sum(y*log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))  # -2log(Lambda)
pvalue<-1-pchisq(q = stat, df = 1)
data.frame(stat, pvalue)
head(pi.hat.Ho)  
mean(y)  

#####################################################################
# Razón de Momios

# Estimado de la razón de momios.
exp(mod.fit$coefficients[2])
exp(-10*mod.fit$coefficients[2])

# Intervalo de Wald
beta.ci<-confint.default(object = mod.fit, parm = "distance", level = 0.95)
beta.ci  # Int. Conf. para beta
exp(beta.ci)  # Int. Conf. para OR con c = 1
exp(beta.ci*10)  # Int. Conf. para OR con c = 10
rev(exp(-10*beta.ci))  # Int. Conf. para OR con c = -10
rev(1/exp(beta.ci*10))  # Invertir Int. Conf. para OR con c = 10

# Intervalo perfil de verosimilitud
beta.ci<-confint(object = mod.fit, parm = "distance", level = 0.95)
beta.ci  # Int. Conf. para beta
rev(exp(-10*beta.ci))  # Intervalo perfil verosimilitud
as.numeric(rev(exp(-10*beta.ci)))  

# Forma alterna
library(mcprofile)
K <- matrix(data = c(0, 1), nrow = 1, ncol = 2, byrow = TRUE)
linear.combo <- mcprofile(object = mod.fit, CM = K)
ci.log.OR <- confint(object = linear.combo, level = 0.95, adjust = "none")
names(ci.log.OR)
as.numeric(rev(exp(-10*ci.log.OR$confint)))


#####################################################################
# Estimación de la probabilidad de éxito.


# pi.hat para una distancia de 20 yardas
linear.pred<-mod.fit$coefficients[1] + mod.fit$coefficients[2]*20
linear.pred
exp(linear.pred)/(1+exp(linear.pred))
as.numeric(exp(linear.pred)/(1+exp(linear.pred)))  


predict.data<-data.frame(distance = 20)
predict(object = mod.fit, newdata = predict.data, type = "link")
predict(object = mod.fit, newdata = predict.data, type = "response")

head(placekick$distance == 20)
mod.fit$fitted.values[3]  # 3rd observation has distance = 20


# Intervalo de Wald

alpha<-0.05
linear.pred<-predict(object = mod.fit, newdata = predict.data, type = "link", se = TRUE)
linear.pred
pi.hat<-exp(linear.pred$fit)/(1+exp(linear.pred$fit))
CI.lin.pred<-linear.pred$fit + qnorm(p = c(alpha/2, 1-alpha/2))*linear.pred$se
CI.pi<-exp(CI.lin.pred)/(1+exp(CI.lin.pred))
CI.pi
round(data.frame(predict.data, pi.hat, lower = CI.pi[1], upper = CI.pi[2]),4)

#########################
# Ejemplos adicionales del intervalo de Wald

# Estimar pi para distance = 20, 30 y change = 1
predict.data<-data.frame(distance = c(20,30), change = c(1, 1))
predict.data
alpha<-0.05
linear.pred<-predict(object = mod.fit2, newdata = predict.data, type = "link", se = TRUE)
CI.lin.pred.x20<-linear.pred$fit[1] + qnorm(p = c(alpha/2, 1-alpha/2)) * linear.pred$se[1]
CI.lin.pred.x30<-linear.pred$fit[2] + qnorm(p = c(alpha/2, 1-alpha/2)) * linear.pred$se[2]
round(exp(CI.lin.pred.x20)/(1+exp(CI.lin.pred.x20)),4)  # Int. Conf. para distance = 20
round(exp(CI.lin.pred.x30)/(1+exp(CI.lin.pred.x30)),4)  # Int. Conf. para distance = 30

#########################
# Intervalos perfil LR 

library(mcprofile)

# Matriz para obtener beta_0 + beta_1
K<-matrix(data = c(1, 20), nrow = 1, ncol = 2)  
K
linear.combo<-mcprofile(object = mod.fit, CM = K)  # Calcular -2log(Lambda)
ci.logit.profile<-confint(object = linear.combo, level = 0.95)  # Int. Conf. para beta_0 + beta_1 * x
ci.logit.profile
names(ci.logit.profile)
ci.logit.profile$confint
exp(ci.logit.profile$confint)/(1 + exp(ci.logit.profile$confint))
expit(ci.logit.profile)  
1/(1 + exp(-ci.logit.profile$confint))  

names(ci.logit.profile)

#####################################################################
# Gráficas

# Encontrar la proporción observada de éxitos para cada distancia
w<-aggregate(formula = good ~ distance, data = placekick, FUN = sum)
n<-aggregate(formula = good ~ distance, data = placekick, FUN = length)
w.n<-data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good/n$good,4))
head(w.n)
tail(w.n)

# Gráfica de las proporciones observadas con el modelo de regresión logística
x11(width = 7, height = 6, pointsize = 12)
plot(x = w$distance, y = w$good/n$good, xlab = "Distancia (yardas)", ylab = "Probabilidad Estimada",
     panel.first = grid(col = "gray", lty = "dotted"))
# Agregar el modelo de regresión logística estimado
curve(expr = predict(object = mod.fit, newdata = data.frame(distance = x), type = "response"), col = "red", add = TRUE,
      xlim = c(18, 66))

##########################
# Agregar los límites del intervalo de confianza

# Función para los Intervalos de confianza
ci.pi<-function(newdata, mod.fit.obj, alpha){
  linear.pred<-predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  CI.lin.pred.lower<-linear.pred$fit - qnorm(p = 1-alpha/2)*linear.pred$se
  CI.lin.pred.upper<-linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
  CI.pi.lower<-exp(CI.lin.pred.lower) / (1 + exp(CI.lin.pred.lower))
  CI.pi.upper<-exp(CI.lin.pred.upper) / (1 + exp(CI.lin.pred.upper))
  list(lower = CI.pi.lower, upper = CI.pi.upper)
}

# Casos de Prueba
ci.pi(newdata = data.frame(distance = 20), mod.fit.obj = mod.fit, alpha = 0.05)
ci.pi(newdata = data.frame(distance = 60), mod.fit.obj = mod.fit, alpha = 0.05)

# Graficar las bandas de los intervalos de confianza
curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$lower, col = "blue", 
      lty = "dotdash", add = TRUE, xlim = c(18, 66))
curve(expr = ci.pi(newdata = data.frame(distance = x), mod.fit.obj = mod.fit, alpha = 0.05)$upper, col = "blue", 
      lty = "dotdash", add = TRUE, xlim = c(18, 66))

legend(x = 20, y = 0.4, legend = c("Modelo de Regresión Logística", "Intervalo de confianza del 95%"), lty = c("solid", "dotdash"), col = c("red", "blue"), bty = "n")