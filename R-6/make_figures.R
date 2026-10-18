# R-6课程图形。此文件供教师重新生成图形，不作为新增课堂知识。
source("R6-data.R")
dir.create("figs", showWarnings = FALSE)
# PDF的GB1字体族支持中文，不依赖X11。
open_plot <- function(name, width = 7.2, height = 4.8) {
  current_plot <<- file.path("figs", paste0(name, ".pdf"))
  pdf(file = current_plot,
      width = width, height = height, family = "GB1")
  par(mar = c(4.2, 4.7, 1.7, 1), mgp = c(2.8, .8, 0),
      cex = 1.15, las = 1, col.axis = "#24313D", col.lab = "#24313D")
}
close_plot <- function() {
  invisible(dev.off())
  embedded <- tempfile(fileext = ".pdf")
  embedFonts(current_plot, outfile = embedded)
  stopifnot(file.copy(embedded, current_plot, overwrite = TRUE))
  unlink(embedded)
  invisible(NULL)
}
hist_height <- function(freq = FALSE, breaks = seq(157.3, 181.8, 3.5), main = "") {
  hist(height, breaks = breaks, freq = freq, main = main,
       xlab = "身高（cm）", ylab = if (freq) "频数" else "频率/组距",
       col = "#DCE6EE", border = "#193B5B")
}
open_plot("height-density"); hist_height(); close_plot()
open_plot("height-density-count", 9, 4.1)
par(mfrow = c(1, 2), mar = c(3.8, 4.7, 2.2, 1), cex = 1.15)
hist_height(main = "频率直方图")
hist_height(TRUE, main = "分组频数条形图")
close_plot()
open_plot("height-groups", 9, 4.1)
par(mfrow = c(1, 3), mar = c(3.8, 4.1, 2.2, .6), cex = 1.15)
hist_height(breaks = seq(157.3, 181.8, length.out = 3), main = "2组")
hist_height(breaks = seq(157.3, 181.8, length.out = 8), main = "7组")
hist_height(breaks = seq(157.3, 181.8, length.out = 21), main = "20组")
close_plot()
open_plot("score-density", 9, 4.1)
par(mfrow = c(1, 2), mar = c(4.7, 5.5, 2.2, 1),
    mgp = c(3.5, .8, 0), cex = 1.15)
for (name in c("prob02", "prob03")) {
  hist(get(name), right = FALSE, freq = FALSE,
       breaks = c(0, 60, 70, 80, 90, 100),
       main = if (name == "prob02") "2002级" else "2003级",
       xlab = "成绩", ylab = "频率/组距", col = "#DCE6EE", border = "#193B5B")
}
close_plot()
open_plot("score-count-density", 9, 4.1)
par(mfrow = c(1, 2), mar = c(4.7, 5.5, 2.2, 1),
    mgp = c(3.5, .8, 0), cex = 1.15)
hist(prob02, right = FALSE, freq = TRUE,
     breaks = c(0, 60, 70, 80, 90, 100), main = "2002级：频数",
     xlab = "成绩", ylab = "频数", col = "#DCE6EE", border = "#193B5B")
hist(prob02, right = FALSE, freq = FALSE,
     breaks = c(0, 60, 70, 80, 90, 100), main = "2002级：频率/组距",
     xlab = "成绩", ylab = "频率/组距", col = "#DCE6EE", border = "#193B5B")
close_plot()
open_plot("speed-density");
hist(speed, breaks = seq(5, 55, 5), freq = FALSE, main = "",
     xlab = "车速（km/h）", ylab = "频率/组距", col = "#DCE6EE", border = "#193B5B")
close_plot()
u <- table(as.factor(gender)); v <- prop.table(u)
open_plot("gender-bars", 9, 4.1)
par(mfrow = c(1, 2), mar = c(3.8, 4.7, 2.2, 1), cex = 1.15)
barplot(u, main = "频数条形图", ylab = "频数", col = "#DCE6EE", border = "#193B5B")
barplot(v, main = "频率条形图", ylab = "频率", col = "#DCE6EE", border = "#193B5B")
close_plot()
open_plot("gender-pie", 6.3, 4.3)
par(mar = c(.5, .5, .5, .5), cex = 1.45)
pie(u, col = c("#DCE6EE", "#7D9CB4"), border = "#193B5B")
close_plot()
x <- matrix(c(12, 19, 17, 21), nrow = 2, ncol = 2)
colnames(x) <- c("男", "女"); rownames(x) <- c("文科", "理科")
y <- as.table(x)
open_plot("major-stacked")
barplot(t(y), xlim = c(0, 3.5), legend.text = colnames(y),
        args.legend = list(x = "topright"), ylab = "频数",
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
close_plot()
open_plot("major-proportions")
barplot(prop.table(t(y), margin = 2), xlim = c(0, 3.5),
        legend.text = colnames(y), args.legend = list(x = "topright"),
        ylab = "频率", col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
close_plot()
open_plot("major-beside")
barplot(t(y), beside = TRUE, legend.text = colnames(y),
        args.legend = list(x = "topleft"), ylab = "频数",
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
close_plot()
open_plot("reversed-bars", 9, 4.1)
par(mfrow = c(1, 3), mar = c(3.8, 4.1, 2.2, .6), cex = 1.15)
barplot(y, xlim = c(0, 4.5), legend.text = rownames(y),
        args.legend = list(x = "topright", cex = .9), main = "普通堆积",
        ylab = "频数", col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
barplot(prop.table(y, margin = 2), xlim = c(0, 4.5),
        legend.text = rownames(y), args.legend = list(x = "topright", cex = .9),
        main = "等高堆积", ylab = "频率",
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
barplot(y, beside = TRUE, ylim = c(0, 32), legend.text = rownames(y),
        args.legend = list(x = "topleft", cex = .9), main = "并列",
        ylab = "频数", col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
close_plot()
z <- matrix(c(15, 20, 25, 25, 30, 15), nrow = 3)
rownames(z) <- c("文科", "理科", "工科"); colnames(z) <- c("男", "女")
open_plot("exercise-bars", 9, 4.1)
par(mfrow = c(1, 3), mar = c(3.8, 4.1, 2.2, .6), cex = 1.15)
centres <- barplot(t(z), xlim = c(0, 5.7), axisnames = FALSE, main = "普通堆积", ylab = "频数",
        legend.text = colnames(z), args.legend = list(x = "topright", cex = .9),
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
axis(1, at = centres, labels = rownames(z), tick = FALSE, gap.axis = 0, cex.axis = .85)
centres <- barplot(prop.table(t(z), margin = 2), xlim = c(0, 5.7), axisnames = FALSE, main = "等高堆积", ylab = "频率",
        legend.text = colnames(z), args.legend = list(x = "topright", cex = .9),
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
axis(1, at = centres, labels = rownames(z), tick = FALSE, gap.axis = 0, cex.axis = .85)
centres <- barplot(t(z), beside = TRUE, xlim = c(0, 14.5), axisnames = FALSE, main = "并列", ylab = "频数",
        legend.text = colnames(z), args.legend = list(x = "topright", cex = .9),
        col = c("#7D9CB4", "#DCE6EE"), border = "#193B5B")
axis(1, at = colMeans(centres), labels = rownames(z), tick = FALSE, gap.axis = 0, cex.axis = .85)
close_plot()
stopifnot(length(height) == 56, length(prob02) == 35, length(prob03) == 96,
          length(gender) == 29, sum(gender == "男") == 12,
          sum(gender == "女") == 17, length(speed) == 55)
print(table(gender)); print(addmargins(u)); print(prop.table(u))
print(addmargins(prop.table(u))); print(y); print(prop.table(t(y), margin = 2))
