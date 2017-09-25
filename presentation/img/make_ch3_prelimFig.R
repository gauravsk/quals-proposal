aa <- read.csv("~/Desktop/chess-kuang-for-graph.csv")

par(bg = NA, oma = c(0,0,0,0), mar = c(0,0,0,0))

aa$nichDiff_net <- 1-aa$nicheOverlap_net
aa$nichDiff_micro <- 1-aa$nicheOverlap_micro
aa$nichDiff_comp <- 1-aa$nicheOverlap_comp
plot(aa$nichDiff_net~aa$rl, type = "b", cex = 4, lwd = 4, pch = 19, log = "x", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 0.2))
points(aa$nichDiff_comp~aa$rl, type = "b", cex = 3.75, pch = 19, lwd = 2)
points(aa$nichDiff_comp~aa$rl, cex = 3, pch = 19, col = "grey")
points(aa$nichDiff_micro~aa$rl, type = "b", cex = 3.75, pch = 19, lwd = 2)
points(aa$nichDiff_micro~aa$rl, cex = 3, pch = 19, col = "grey")


plot(aa$nichDiff_net~aa$rl, type = "n", cex = 4, lwd = 4, pch = 19, log = "x", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 0.2))
points(aa$nichDiff_comp~aa$rl, type = "b", cex = 3.75, pch = 19, lwd = 2)
points(aa$nichDiff_comp~aa$rl, cex = 3, pch = 19, col = "grey")
points(aa$nichDiff_micro~aa$rl, type = "b", cex = 3.75, pch = 19, lwd = 2)
points(aa$nichDiff_micro~aa$rl, cex = 3, pch = 19, col = "grey")
