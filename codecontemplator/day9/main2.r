library(raster)

m <- read.table("input.txt", header = FALSE)

clumps <- as.matrix(clump(raster(m != 9), directions = 4))
nclumps <- max(clumps, na.rm = TRUE)

func <- function(i) { nrow(which(clumps == i, arr.ind = TRUE)) }

prod(sort(Vectorize(func)(1:nclumps), decreasing = TRUE)[1:3]) # 920448