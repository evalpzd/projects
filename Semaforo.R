# https://www.google.com/maps/place/1501+N+33rd+St,+Lincoln,+NE+68503,+EE.+UU./@40.8283387,-96.6735948,18z/data=!4m5!3m4!1s0x8796bea2e956c939:0x6783a116a747753a!8m2!3d40.828021!4d-96.672861
stoplight <- read.csv(file  = "Stoplight.csv")
View(stoplight)

# media y varianza de los datos observados
mean(stoplight$vehicles)
var(stoplight$vehicles)

# Frequencias
table(stoplight$vehicles) #Obs. que y = 0, 1, ..., 8 tiene conteos positivos
rel.freq <- table(stoplight$vehicles)/length(stoplight$vehicles)
rel.freq2 <- c(rel.freq, rep(0, times = 7))

# Cálculos poblacionales
y <- 0:15
prob <- round(dpois(x = y, lambda = mean(stoplight$vehicles)), 4)

# Observados y Poisson
data.frame(y, prob, rel.freq = rel.freq2)

# Gráfica
x11(width = 7, height = 6, pointsize = 12)
plot(x = y - 0.1, y = prob, type = "h", ylab = "Probabilidad", xlab = "Número de vehículos", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "red")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observado"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "red"), bty = "n")

########################################################################
# Intervalos de confianza

alpha <- 0.05
n <- length(stoplight$vehicles)
mu.hat <- mean(stoplight$vehicles)

# Wald
mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(mu.hat/n)

# Score
(mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))/(2*n)) + qnorm(p = c(alpha/2, 1 - alpha/2)) * sqrt((mu.hat + qnorm(p = 1 - alpha/2)/(4*n))/n)

# Exact
qchisq(p = c(alpha/2, 1 - alpha/2), df = c(2*n*mu.hat, 2*(n*mu.hat + 1)))/(2*n)
# Forma alterna
# qgamma(p = c(alpha/2, 1-alpha/2), shape = c(n*mu.hat, n*mu.hat+1), scale = 1)/n
# c(qchisq(p = alpha/2, df = 2*n*mu.hat),qchisq(p = 1-alpha/2, df = 2*(n*mu.hat+1)))/(2*n)

# Exact usando poisson.test 
poisson.test(x = mu.hat*n)$conf.int / n

# Usual t-distribution based interval
t.test(x = stoplight$vehicles, conf.level = 0.95)
