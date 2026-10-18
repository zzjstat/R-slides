# R-5 教师制图脚本；从 R-5 目录运行，不作为新增课堂知识。
set.seed(20261018)
figdir <- "figs"
dir.create(figdir, showWarnings = FALSE)

pdf(file.path(figdir, "uniform-mean.pdf"), width = 7.4, height = 4.7,
    family = "Helvetica", useDingbats = FALSE)
par(mar = c(4.2, 4.6, 1, 2.2), las = 1, cex = 1.12,
    col.axis = "#24313D", col.lab = "#24313D")
u <- runif(50000, 0, 1)
idx <- 1:length(u)
avg <- cumsum(u) / idx
plot(idx, avg, type = "l", col = "#193B5B", lwd = 1.5,
     ylim = c(0.45, 0.55), xlab = "Number of observations",
     ylab = "Sample mean", xaxs = "i")
abline(h = 0.5, lty = 2, col = "#92533C", lwd = 1.5)
legend("topright", c("Sample mean", "Expectation = 0.5"),
       col = c("#193B5B", "#92533C"), lty = c(1, 2), lwd = 1.5,
       bty = "n", cex = 0.94)
dev.off()

for (n in c(30, 300)) {
  y <- matrix(rpois(1000 * n, 0.5), 1000, n)
  z <- (rowSums(y) / n - 0.5) * sqrt(n / 0.5)
  x <- seq(-3, 3, 0.1)
  f <- c()
  for (i in 1:length(x)) {
    f <- c(f, sum(z <= x[i]) / 1000)
  }
  phi <- pnorm(x, 0, 1)
  print(c(n = n, largest_grid_difference = max(abs(f - phi))))
  pdf(file.path(figdir, paste0("poisson-n", n, ".pdf")),
      width = 7.4, height = 4.7, family = "Helvetica",
      useDingbats = FALSE)
  par(mar = c(4, 4.5, 1, 1), las = 1, cex = 1.12,
      col.axis = "#24313D", col.lab = "#24313D")
  plot(x, phi, type = "l", lwd = 2, col = "black",
       ylim = c(0, 1), xlab = "x", ylab = "Cumulative probability")
  for (i in 1:(length(x) - 1)) {
    lines(x[i:(i + 1)], c(f[i], f[i]), col = "red", lwd = 2)
    points(x[i], f[i], pch = 20, col = "red", cex = 0.62)
  }
  points(x[length(x)], f[length(x)], pch = 20, col = "red", cex = 0.62)
  legend("topleft", c("Standard normal", "Empirical (1000 groups)"),
         lty = c(1, 1), pch = c(NA, 20), col = c("black", "red"),
         lwd = 2, bty = "n", cex = 0.88)
  dev.off()
}
