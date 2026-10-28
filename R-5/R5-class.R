# R语言基础（五）：随机模拟与正态近似
# 课堂程序按下面的分段在RStudio中运行。
# 教材印刷第84—93页。

# 1. 平均值与频率
x <- c(0, 1, 1, 0, 1)
mean(x)
mean(x == 1)

# 2. 均匀随机数与正态随机数
x <- runif(10, 0, 1)
x
mean(x)
x <- rnorm(10, 0, 1)
x

# 3. 均匀分布的样本平均值（教材案例2.17）
x <- runif(50000, 0, 1)
mean(x[1:100])
mean(x[1:1000])
mean(x)

# 4. 矩的平均、频率与经验累计比例（例2.6.1—2.6.3）
x <- runif(10000, 0, 1)
mean(x^2)
mean(x <= 0.5)
t <- 0.5
mean(x <= t)

# 5. 例2.6.5：方法一，均匀随机数计算正态密度积分
Y <- runif(10000, 0.1, 2)
c <- 1 / sqrt(2 * pi)
f <- c * exp(-(Y * Y) / 2)
p <- (2 - 0.1) * mean(f)
p

# 6. 例2.6.5：方法二，正态随机数计算区间频率
x <- rnorm(100000, 0, 1)
I <- x > 0.1 & x < 2
p <- mean(I)
p
pnorm(2, 0, 1) - pnorm(0.1, 0, 1)

# 7. 课堂练习一
# 分别运行上面第5、6段，各重复两次，记录结果。
# 第5段把10000改为100000，第6段把100000改为10000。
# 重新运行相应整段，与0.437422比较，说明结果的差异。

# 8. 例2.6.6：36名女大学生的平均体重超过57 kg
z <- sqrt(36) * (57 - 51.96) / 6.31
z
1 - pnorm(z, 0, 1)

# 9. 泊松模拟完整段（教材例2.6.7，第92—93页）
# 每行是一组样本，n为每组样本量，共1000组。
# 教师演示绘图段，学生重点理解n、1000与图形比较。
n <- 30
y <- matrix(rpois(1000 * n, 0.5), 1000, n)
z <- (rowSums(y) / n - 0.5) * sqrt(n / 0.5)
x <- seq(-3, 3, 0.1)
f <- c()
for (i in 1:length(x)) {
  f <- c(f, sum(z <= x[i]) / 1000)
}
phi <- pnorm(x, 0, 1)

# 绘图：phi是理论累计概率，f是经验累计比例。
# 修正教材第93页plot中不一致的对象名。
plot(x, phi, type = "l", lwd = 2, col = "black",
     ylim = c(0, 1), xlab = "x", ylab = "Cumulative probability")
for (i in 1:(length(x) - 1)) {
  lines(x[i:(i + 1)], c(f[i], f[i]), col = "red", lwd = 2)
  points(x[i], f[i], pch = 20, col = "red")
}
points(x[length(x)], f[length(x)], pch = 20, col = "red")
legend("topleft", c("Standard normal", "Empirical (1000 groups)"),
       lty = c(1, 1), pch = c(-1, 20),
       col = c("black", "red"), lwd = 2)

# 10. 课堂练习二
# 只将第9段首行的 n <- 30 改为 n <- 300。
# 重新运行第9段的全部代码，包括生成数据、计算比例和绘图。
# 保持1000组，比较两幅图，说明每组样本量与重复组数的区别。
