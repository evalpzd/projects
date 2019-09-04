dehart <- read.table("DeHartSimplified.csv", header = TRUE, sep = ",", na.strings = " ")
head(dehart)

# Reducir los datos para los ejemplos
saturday <- dehart[dehart$dayweek == 6, c(1,4,7,8)]
head(round(x = saturday, digits = 3))
dim(saturday)

#####################################################################
# Usar los eventos negativos para estimar el número de bebidas

# Ajuste del modelo
mod.neg <- glm(formula = numall ~ negevent, family = poisson(link = "log"), data = saturday)

# Análisis del ajuste del modelo
summary(mod.neg)
100*(exp(mod.neg$coefficients[2]) - 1)
beta1.int <- confint(mod.neg, parm = "negevent", level = 0.95)
100*(exp(beta1.int) - 1)
library(car)
Anova(mod.neg)

# Gráfica de los datos y media estimada
x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.5color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
plot(x = saturday$negevent, y = saturday$numall, xlab = "Índice de eventos negativos", ylab = "Bebidas alcohólicas consumidas")
curve(expr = exp(mod.neg$coefficients[1] + x*mod.neg$coefficients[2]), add = TRUE, lwd = 2)

#####################################################################
# Usar eventos positivos y negativos para estimar el número de bebidas

# Ajuste y análisis del modelo de NEG vs. POS e interacciones
mod.negpos <- glm(formula = numall ~ negevent*posevent, family = poisson(link = "log"), data = saturday)
summary(mod.negpos)
confint(mod.negpos)
Anova(mod.negpos)

x1 <- seq(from = 0, to = 2.5, by = .01)
y1 <- seq(from = 0, to = 3.5, by = .01)
xy1 <- data.frame(expand.grid(negevent = x1, posevent = y1))

surface = matrix(predict(object = mod.negpos, newdata = xy1, type = "response"), nrow = length(x1))

x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.6-1BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
drink.colors <- gray(1 - saturday$numall/max(saturday$numall))
plot(x = saturday$negevent, y = saturday$posevent, xlim = c(0,2.5), ylim = c(0,3.5), xlab = "Índice de eventos negativos", ylab = "Índice de eventos positivos", pch = 21, bg = drink.colors, cex = 1.5, main = "Número de bebidas vs. Eventos positivos y negativos")
contour(x = x1, y = y1, z = surface, xlim = c(0,2.5), ylim = c(0,3.5), labcex = 1, levels = c(1,2,3,4,5,6,7,8,9,10,15,20,30,40,50,60,80), add = TRUE)
# dev.off()  

library(rgl)
open3d()
persp3d(x = x1, y = y1, z  =  surface, col = "red", xlab = "Índice de eventos negativos", ylab = "Índice de eventos positivos", zlab = "Predicción de Bebidas")
points3d(x = saturday$negevent, y = saturday$posevent, z = saturday$numall, col = "blue")

# Gráfica de los datos y media estimada en los cuartiles de POSEVENT
posev.quart <- summary(saturday$posevent)
posev.quart

x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.6-2BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
posev.colors <- gray(1 - saturday$posevent/max(saturday$posevent))

plot(x = saturday$negevent, y = saturday$numall, xlab = "Índice de eventos negativos", ylab = "Bebidas alcohólicas consumidas", pch = 21, bg = posev.colors, cex = 1.5, main = "Número de bebidas vs. Eventos negativos")
curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[2]*mod.negpos$coefficients[3] + x*posev.quart[2]*mod.negpos$coefficients[4]), add = TRUE, lwd = 2)
curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[3]*mod.negpos$coefficients[3] + x*posev.quart[3]*mod.negpos$coefficients[4]), add = TRUE, lty = "dashed", lwd = 2)
curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[5]*mod.negpos$coefficients[3] + x*posev.quart[5]*mod.negpos$coefficients[4]), add = TRUE, lty = "dotted", lwd = 2)
legend(x = 1.0, y = 20, legend = c("1st", "2nd", "3rd"), lty = c("solid", "dashed", "dotted"), lwd = 2, title = "Cuartil de índice de eventos positivos", bty = "n")
# dev.off()  

mean.ratio <- exp(mod.negpos$coefficients[2] + posev.quart[c(2,3,5)]*mod.negpos$coefficients[4])
mean.ratio
100*(mean.ratio - 1)

library(package = mcprofile)
K <- matrix(data = c(0, 1, 0, 1*posev.quart[2],
                     0, 1, 0, 1*posev.quart[3],
                     0, 1, 0, 1*posev.quart[5]), nrow = 3, ncol = 4, byrow = TRUE)
linear.combo <- mcprofile(object = mod.negpos, CM = K)  #Calculate -2log(Lambda)
ci.beta <- confint(object = linear.combo, level = 0.95)
# ci.beta$confint
100*(exp(ci.beta$estimate) - 1) 
100*(exp(ci.beta$confint) - 1)

