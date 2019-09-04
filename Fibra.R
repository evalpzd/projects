diet <- read.csv("Fiber.csv")

diet$fiber<-factor(x = diet$fiber, levels = c("none", "bran", "gum", "both"))
diet$bloat<-factor(x = diet$bloat, levels = c("none", "low", "medium", "high"))

diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
diet.table


#####################################################################
# Pruebas de independencia

class(diet.table)
summary(diet.table)

ind.test<-chisq.test(x = diet.table, correct = FALSE)
ind.test
ind.test$expected
ind.test$stdres  # Residuos estandarizados de Pearson

library(vcd)
assocstats(x = diet.table)


#####################################################################
#Simulación de Monte Carlo

n<-sum(diet.table)  # Tamaño de muestra
pi.hat<-ind.test$expected/n  # Estimado del valor esperado pi^_ij bajo Ho
pi.hat
c(pi.hat) 
sum(pi.hat)  # comprobación


B<-10000  # Número de conjuntos de datos simulados
set.seed(1298)
table.count<-rmultinom(n = B, size = n, prob = pi.hat)  
table.count[,1:2]  

rowMeans(table.count)/n

# Función que calcula X^2 para un conjunto de datos y que revisa el tamaño de la tabla de contingencia
calc.stat<-function(data) {
  c.table<-array(data = data, dim=c(4,4), dimnames = list(fiber = c("bran", "gum", "combo", "control"),
                                                          bloat = c("high", "medium", "low", "none")))
  ind.test<-chisq.test(c.table, correct = FALSE)
  ck.0.row<-sum(rowSums(c.table) == 0)  # Número de filas con todas las entradas 0
  ck.0.col<-sum(colSums(c.table) == 0)  # Número de columnas con todas las entradas 0
  c(ind.test$statistic, ck.0.row, ck.0.col)
}

calc.stat(data = table.count[,1]) # Ejemplo de aplicación de la función

# Calcular X^2* para cada conjunto de datos
save.star<-apply(X = table.count, MARGIN = 2, FUN = calc.stat)
# Las advertencias son debido a  "Chi-squared approximation may be incorrect"

sum(save.star[2,])
sum(save.star[3,])

c.table<-array(data = table.count[,691], dim=c(4,4), dimnames = list(fiber = c("bran", "gum", "combo", "control"),
                                                                     bloat = c("high", "medium", "low", "none")))
X.sq.star<-save.star[1, save.star[2,] == 0 & save.star[3,] == 0]


#####################################################################
# Gráficas

x11(width = 10, height = 6, pointsize = 12)

par(mfrow=c(1,2))

# Histograma
df<-9
hist(x = X.sq.star, main = "Histogram", freq = FALSE, xlab = expression(X^"2*"))
curve(expr = dchisq(x = x, df = df), col = "red", add = TRUE)

# Comparación de las distribuciones acumuladas
plot.ecdf(x = X.sq.star, verticals = TRUE, do.p = FALSE, main = "CDFs", lwd = 2, col = "black",
          xlab = expression(X^"2*"), panel.first = grid(col="gray", lty="dotted"),
          ylab = "CDF")
curve(expr = pchisq(q = x, df = df), col = "red", add = TRUE, lwd = 1)
legend(x = 15, y = 0.4, legend = c(expression(X^"2*"), expression(chi[9]^2)), lwd = c(2,1),
       col = c("black", "red"), bty = "n")


#####################################################################
# Modelo de regresión multinomial

library(nnet)
library(car)

# Estimación del modelo
mod.fit.nom<-multinom(formula = bloat ~ fiber, weights = count, data = diet)
summary(mod.fit.nom)
logLik(mod.fit.nom)

# LRT
Anova(mod.fit.nom)

# Forma alterna LRT
mod.fit.Ho<-multinom(formula = bloat ~ 1, weights = count, data=diet)
summary(mod.fit.Ho)

# -2log(Lambda)
mod.fit.Ho$deviance - mod.fit.nom$deviance  # mismo valor que con Anova()


# ORs
round(exp(coefficients(mod.fit.nom)[,-1]), 2)
exp(13.3561038 - (-4.1103893)) 
diet.table[1,1]*diet.table[2,2]/(diet.table[1,2]*diet.table[2,1]) 
diet.table[1,1]*diet.table[2,2]/(diet.table[1,2]*diet.table[2,1]) 

conf.beta<-confint(object = mod.fit.nom, level = 0.95)
conf.beta
ci.OR.low<-exp(conf.beta[2:4,1:2,1])
ci.OR.medium<-exp(conf.beta[2:4,1:2,2])
ci.OR.high<-exp(conf.beta[2:4,1:2,3])
round(data.frame(low = ci.OR.low[,1], up = ci.OR.low[,2]), 2)
round(data.frame(low = ci.OR.medium[,1], up = ci.OR.medium[,2]), 2)
round(data.frame(low = ci.OR.high[,1], up = ci.OR.high[,2]), 2)


# Estimados de probabilidades
pi.hat<-predict(object = mod.fit.nom, newdata = diet[1:4,], type = "probs")
pi.hat
data.frame(fiber = diet[1:4,1], round(pi.hat,4))
# Probabilidades condicionales
diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
round(diet.table/rowSums(diet.table),4)


# Sumar 0.5 a las celdas con valor 0 y volver a estimar el modelo
diet$count2<-ifelse(test = diet$count == 0, yes = diet$count + 0.5, no = diet$count)
mod.fit.nom2<-multinom(formula = bloat ~ fiber, weights = count2, data = diet)
sum.fit<-summary(mod.fit.nom2)
names(sum.fit)
round(sum.fit$coefficients, 4)
round(sum.fit$standard.errors, 4)
round(exp(coefficients(mod.fit.nom2)[,-1]), 2)
conf.beta<-confint(object = mod.fit.nom2, level = 0.95)
round(exp(conf.beta[2:4,,1]),1)  
round(exp(conf.beta[2:4,,2]),1)  
round(exp(conf.beta[2:4,,3]),2)  


#####################################################################
# Regresión multinomial e interacciones

diet$bran<-factor(ifelse(test = diet$fiber == "bran" | diet$fiber == "both", yes = "yes", no = "no"))
diet$gum<-factor(ifelse(test = diet$fiber == "gum" | diet$fiber == "both", yes = "yes", no = "no"))
diet

# Modelo de regresión multinomial sin interacciones
#mod.fit.nom.main<-multinom(formula = bloat ~ bran + gum, weights = count, data = diet)
#summary(mod.fit.nom.main)

# Modelo de regresión multinomial con interacciones
mod.fit.nom.inter<-multinom(formula = bloat ~ bran + gum + bran:gum, weights = count, data = diet)
summary(mod.fit.nom.inter)
library(car)  
Anova(mod.fit.nom.inter)
logLik(mod.fit.nom.inter)

#####################################################################
# Tablas de contingencia

library(MASS)
levels(diet$bloat)

mod.fit.ord<-polr(formula = bloat ~ fiber, weights = count, data=diet, method = "logistic")
summary(mod.fit.ord)
library(car)
Anova(mod.fit.ord)  # LRT

# ORs
round(exp(-coefficients(mod.fit.ord)), 2)
round(1/exp(-coefficients(mod.fit.ord)), 2)

conf.beta<-confint(object = mod.fit.ord, level = 0.95)
ci<-exp(-conf.beta)
round(data.frame(low = ci[,2], up = ci[,1]), 2)
round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)

diet$fiber2<-relevel(x = diet$fiber, ref = "bran")
mod.fit.ord2<-polr(formula = bloat ~ fiber2, weights = count, data=diet, method = "logistic")
conf.beta2<-confint(object = mod.fit.ord2, level = 0.95)
ci2<-exp(-conf.beta2)
round(data.frame(low = ci2[,2], up = ci2[,1]), 2)
round(data.frame(low = 1/ci2[,1], up = 1/ci2[,2]), 2)


#####################################################################
# Modelo de momios no proporcionales

library(VGAM)  

# Primero se ajusta un modelo de momios proporcionales

saveELM <- diet$bloat
diet$bloat <- ordered(diet$bloat)

mod.fit.po<-vglm(formula = bloat ~ fiber, family = cumulative(parallel = TRUE),
                 weights = count, data = diet[diet$count != 0,])
summary(mod.fit.po)

slotNames(mod.fit.po)  # estructura parecida a names( ) 
mod.fit.po@coefficients
mod.fit.po@df.residual
#showMethods(class = "vglm") # Parecido a method(class = " ") in 

mod.fit.npo<-vglm(formula = bloat ~ fiber, family = cumulative(parallel = FALSE),
                  weights = count, data = diet[diet$count != 0,])
#summary(mod.fit.npo) 
round(mod.fit.npo@coefficients, 2)

#anova(mod.fit.po, mod.fit.npo)

tran.LR<-deviance(mod.fit.po) - deviance(mod.fit.npo)
df<-mod.fit.po@df.residual - mod.fit.npo@df.residual
p.value<-1 - pchisq(q = tran.LR, df = df)
data.frame(tran.LR, df, p.value)

lrtest(mod.fit.npo, mod.fit.po) # Forma alterna

# Estimación de pi_j
predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"))
predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"), type = "response")
predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"), se.fit = TRUE)



