# 教师用制图程序：从 R-7 目录运行。
source("R7-data.R")
dir.create("figs", showWarnings = FALSE)

draw_pdf <- function(name, draw) {
  destination <- file.path("figs", paste0(name, ".pdf"))
  pdf(destination, width = 7.2, height = 4.8, family = "GB1")
  par(mar = c(4.5, 4.7, 1.5, 1), mgp = c(2.8, .8, 0),
      cex = 1.15, las = 1, col.axis = "#24313D", col.lab = "#24313D")
  draw()
  dev.off()
  embedded <- tempfile(fileext = ".pdf")
  embedFonts(destination, outfile = embedded)
  stopifnot(file.copy(embedded, destination, overwrite = TRUE))
  unlink(embedded)
}

draw_pdf("r7-qq", function() {
  qqnorm(height, main = "", xlab = "标准正态分位数", ylab = "身高（cm）",
         pch = 19, col = "#193B5B")
  qqline(height, col = "#92533C", lwd = 2)
})
draw_pdf("r7-box", function() {
  boxplot(data.frame(甲班 = jia, 乙班 = yi), ylab = "成绩（分）",
          col = c("#DCE6EE", "#7D9CB4"), border = "#193B5B")
})
