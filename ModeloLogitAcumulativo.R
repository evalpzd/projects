#####################################################################
# Gráfica del modelo de Momios proporcionales

# Recuérdese que beta10 < beta20 < beta30
beta<-c(0, 2, 4, 2) #beta10, beta20, beta30, beta1
x.range<-c(-5, 3)

x11(width = 10, height = 6, pointsize = 12)

# pdf(file = "c:\\figures\\Figure3.4color.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
par(mfrow = c(1, 2))
curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(P(Y<=j)),
      xlab = expression(x[1]), main = "Probabilidades Acumulativas para Y", lwd = 2)
curve(expr = plogis(q = beta[2] + beta[4]*x), add = TRUE, lty = "dashed", col = "red", lwd = 2)
curve(expr = plogis(q = beta[3] + beta[4]*x), add = TRUE, lty = "dotted", , col = "blue", lwd = 2)
legend(x = -5.5, y = 0.9, legend = c(expression(P(Y<=1)), expression(P(Y<=2)), expression(P(Y<=3))),
       lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue"),
       bty = "n", lwd = 2)

curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(pi[j]),
      xlab = expression(x[1]), main = "Probabilidades para Y", lwd = 2)
curve(expr = plogis(q = beta[2] + beta[4]*x) - plogis(q = beta[1] + beta[4]*x), add = TRUE,
      lty = "dashed", col = "red", lwd = 2)
curve(expr = plogis(q = beta[3] + beta[4]*x) - plogis(q = beta[2] + beta[4]*x), add = TRUE,
      lty = "dotted", , col = "blue", lwd = 2)
curve(expr = 1 - plogis(q = beta[3] + beta[4]*x), add = TRUE,
      lty = "dotdash", col = "green", lwd = 2)
legend(x = -5.5, y = 0.9, legend = c(expression(pi[1]), expression(pi[2]), expression(pi[3]), expression(pi[4])),
       lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue", "green"),
       bty = "n", lwd = 2)
# dev.off()  # Create plot for book