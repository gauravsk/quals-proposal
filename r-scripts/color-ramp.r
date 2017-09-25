colfunc <- colorRampPalette(c("#0B5394", "white","#E69138"))
plot(1:10, col = colfunc(10), pch = 19)
  
layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
plot(1:20, 1:20, pch = 19, cex=2, col = colfunc(20))

legend_image <- as.raster(matrix(colfunc(20), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Trait Value')
rasterImage(legend_image, 0, 0, .5,1)

