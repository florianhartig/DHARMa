set.seed(124)

col = c("#99000050", "#00990050")



curve(dnorm, -3,3, ylim = c(0,1))
curve(pnorm, -3,3, add = T)

x = rlnorm(500, meanlog = 0.4, sdlog = 0.4)

hist(x, xlim = c(-0,5), breaks = 50, col = col[1], main = "", xlab = "Values", freq = F, ylim = c(0,1))

ecdens = ecdf(x)
dens = function(x) ecdens(x)

curve(dens, 0,20, add = T, lwd = 3)




hist(rpois(500, lambda  = rnorm(500, mean = 5, sd = 4)) + 0.2, xlim = c(-0.2,20), breaks = seq(-0.2,20,0.2), col = col[2], add = T)

legend("topright", legend = c("conditional", "unconditional"), pch = c(15,15), col = col)
