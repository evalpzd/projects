tomato <- read.csv(file  = "C:\\data\\TomatoVirus.csv")
head(tomato)
tomato

# Estimación del modelo
class(tomato$Control)
levels(tomato$Control)
contrasts(tomato$Control)

class(tomato$Infest)
levels(tomato$Infest)
class(factor(tomato$Infest))
levels(factor(tomato$Infest))
contrasts(factor(tomato$Infest))

tomato$Infest<-factor(tomato$Infest)
head(tomato)
class(tomato$Infest)

temp<-factor(tomato$Control, levels = c("C", "B", "N"))  # cambiar orden de niveles
levels(temp)
temp2<-factor(tomato$Control, labels = c("B1", "C1", "N1"))  # Cambiar nombres de niveles
levels(temp2)

mod.fit<-glm(formula = Virus8/Plants ~ Infest + Control, family = binomial(link = logit), data = tomato,
             weights = Plants)
summary(mod.fit)

mod.fit$xlevels

# Modelo con interacción
mod.fit.inter<-glm(formula = Virus8/Plants ~ Infest + Control + Infest:Control, family = binomial(link = logit), data = tomato,
                   weights = Plants)
summary(mod.fit.inter)

# LRT
library(car)
Anova(mod.fit.inter)

#################################################
# Razón de momios para el modelo sin interacción

exp(mod.fit$coefficients[2])
1/exp(mod.fit$coefficients[2])
# as.numeric(exp(mod.fit$coefficients[2]))
# attributes(mod.fit$coefficients[2])

# Intervalo de Wald
exp(confint.default(object = mod.fit, parm = "Infest2", level = 0.95))
# as.numeric(exp(confint.default(object = mod.fit, parm = "Infest2", level = 0.95)))

##################################################
# OR para Control

exp(mod.fit$coefficients[3:4])

# Intervalo Wald
exp(confint.default(object = mod.fit, parm = c("ControlC", "ControlN"), level = 0.95))

# Intervalo Wald para Control N vs. Control C
beta.hat<-mod.fit$coefficients[-1]  # Matches up beta indices with [i] to help avoid mistakes
exp(beta.hat[3] -  beta.hat[2])
cov.mat<-vcov(mod.fit)[2:4,2:4]
var.N.C<-cov.mat[3,3] + cov.mat[2,2] - 2*cov.mat[3,2]
CI.betas<- beta.hat[3]- beta.hat[2] + qnorm(p = c(0.025, 0.975))*sqrt(var.N.C)
exp(CI.betas)

# Forma alterna
tomato$Control.reorder<-relevel(x = tomato$Control, ref = "C")
mod.fit2<-glm(formula = Virus8/Plants ~ Infest + Control.reorder, family = binomial(link = logit), data = tomato,
              weight = Plants)
# summary(mod.fit2)
exp(confint.default(object = mod.fit2, parm = c("Control.reorderB", "Control.reorderN"), level = 0.95))
exp(confint(object = mod.fit2, parm = c("Control.reorderB", "Control.reorderN"), level = 0.95))


# Usando mcprofile
library(package = mcprofile)
K<-matrix(data = c(0, 0,  1,  0,
                   0, 0,  0,  1),  nrow = 2, ncol = 4, byrow = TRUE)
linear.combo<-mcprofile(object = mod.fit, CM = K)
ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
ci.log.OR
comparison<-c("C vs. B", "N vs. B")
data.frame(comparison, OR = exp(ci.log.OR$confint))  

# N vs. C 
K<-matrix(data = c(0, 0, -1,  1),  nrow = 1, ncol = 4, byrow = TRUE)
linear.combo<-mcprofile(object = mod.fit, CM = K)
ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
ci.log.OR
data.frame(comparison = "N vs. C", OR = exp(ci.log.OR$confint))  
save.wald<-wald(object = linear.combo)
ci.logit.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
data.frame(lower = exp(ci.logit.wald$confint[,1]), upper = exp(ci.logit.wald$confint[,2]))

#############################
# OR estimado para Control


beta.hat<-mod.fit.inter$coefficients[-1]

N.B.Infest2.0<-exp(beta.hat[3])
N.B.Infest2.1<-exp(beta.hat[3] + beta.hat[5])
C.B.Infest2.0<-exp(beta.hat[2])
C.B.Infest2.1<-exp(beta.hat[2] + beta.hat[4])
N.C.Infest2.0<-exp(beta.hat[3] - beta.hat[2])
N.C.Infest2.1<-exp(beta.hat[3] - beta.hat[2] + beta.hat[5] - beta.hat[4])
comparison<-c("N vs. B", "N vs. B", "C vs. B", "C vs. B", "N vs. C", "N vs. C")
data.frame(Infest2 = c(0, 1, 0, 1, 0, 1),
           Control = comparison,
           OR.hat = round(c(N.B.Infest2.0, N.B.Infest2.1, C.B.Infest2.0, C.B.Infest2.1,
                            N.C.Infest2.0, N.C.Infest2.1),2))

# Usando mcprofile
# library(package = mcprofile) 
K<-matrix(data = c(0, 0,  0,  1,  0,  0,
                   0, 0,  0,  1,  0,  1,
                   0, 0,  1,  0,  0,  0,
                   0, 0,  1,  0,  1,  0,
                   0, 0, -1,  1,  0,  0,
                   0, 0, -1,  1, -1,  1),  nrow = 6, ncol = 6, byrow = TRUE)
linear.combo<-mcprofile(object = mod.fit.inter, CM = K)
ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
ci.log.OR
data.frame(Infest2 = c(0, 1, 0, 1, 0, 1), comparison, OR = round(exp(ci.log.OR$estimate),2),
           OR.CI = round(exp(ci.log.OR$confint),2))