library(raster)

m <- read.table("input.txt", header = FALSE)

clumps <- as.matrix(clump(raster(m != 9), directions = 4))
nclumps <- max(clumps, na.rm = TRUE)

f <- function(i) { sum(clumps == i, na.rm = TRUE) }

prod(sort(Vectorize(f)(1:nclumps), decreasing = TRUE)[1:3]) # 920448