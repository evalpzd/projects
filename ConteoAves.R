alldata <- read.table(file = "C:\\data\\BirdCounts.csv", sep = ",", header = TRUE)
head(alldata)
contrasts(alldata$Loc)

# Ajustar regresión de Poisson y probar si todas las medias son iguales
M1 <- glm(formula = Birds ~ Loc, family = poisson(link = "log"), data = alldata)
summary(M1)
M1$coefficients 
exp(M1$coefficients) 

# Obtener predicciones de las medias
pred.data <- data.frame(Loc = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
means <- predict(object = M1, newdata = pred.data, type = "link", se.fit = TRUE)
alpha <- 0.05

# Intervalo de confianza de Wald
lower.logmean <- means$fit + qnorm(alpha/2)*means$se.fit
upper.logmean <- means$fit + qnorm(1 - alpha/2)*means$se.fit

# Uniendo resultados
mean.wald.ci <- data.frame(pred.data, round(cbind(exp(means$fit), exp(lower.logmean), exp(upper.logmean)), digits = 2))
colnames(mean.wald.ci) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci

# Para probar "todas las medias son iguales" vs. "no todas las medias son iguales"
# library(car)
anova(M1, test ="Chisq")

######################################################################
# Obtener intervalos LR para las medias.  
library(mcprofile)
K <- matrix(data = c(1, 1, 0, 0, 0, 0,
                     1, 0, 1, 0, 0, 0,
                     1, 0, 0, 1, 0, 0,
                     1, 0, 0, 0, 0, 0,
                     1, 0, 0, 0, 1, 0,
                     1, 0, 0, 0, 0, 1), nrow = 6, ncol = 6, byrow = TRUE)
K

linear.combo <- mcprofile(object = M1, CM = K)
ci.log.mu <- confint(object = linear.combo, level = 0.95, adjust = "none")

mean.LR.ci1 <- data.frame(Loc = pred.data, Estimate = exp(ci.log.mu$estimate), Lower = exp(ci.log.mu$confint[,1]), Upper = exp(ci.log.mu$confint[,2]))
mean.LR.ci1

mean.LR.ci1$Loc2 <- factor(mean.LR.ci1$Loc, levels = levels(mean.LR.ci1$Loc)[c(2,3,4,1,5,6)])

# Gráfica
x11(width = 7, height = 5, pointsize = 12)   
# pdf(file = "c:\\figures\\Figure4.7color.pdf", width = 7, height = 5, colormodel = "cmyk")   
stripchart(Lower ~ Loc2, data = mean.LR.ci1, vertical = FALSE, xlim = c(20,150), col = "red", pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
stripchart(Upper ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = ")", add = TRUE)
stripchart(Estimate ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = "+", add = TRUE)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = mean(alldata$Birds), col = "darkblue", lwd = 4)
# dev.off()  

#####################################################################
# Intervalos de confianza para combinaciones lineales de los parámetros

# Coeficientes para las combinaciones lineales
contr.mat <- rbind(c(0,.5,.5,0,0,0), c(0,.5,.5,-1,0,0), c(0,.5,.5,0,-.5,-.5), 
                   c(0,0,0,1,0,0), c(0,0,0,1,-.5,-.5), c(0,0,0,0,-.5,-.5))
rownames(contr.mat) <- c("For-Edge", "For-Frag", "For-Past", "Frag-Edge", "Frag-Past", "Edge-Past")
contr.mat
