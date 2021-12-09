# https://stackoverflow.com/questions/35772846/obtaining-connected-components-in-r
# https://rdrr.io/cran/raster/man/clump.html

#library(igraph)
library(raster)

d <- read.table("input_sample.txt", header = FALSE)

rmat <- raster(d != 9)

clumps <- as.matrix(clump(rmat, directions=4))
#plot(Clumps)

tot <- max(clumps, na.rm=TRUE)

func <- function(i) { 
    nrow(which(clumps == i, arr.ind = TRUE))
}

prod(sort(Vectorize(func)(1:tot), decreasing = TRUE)[1:3])