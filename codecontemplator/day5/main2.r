input <- read.table("input.txt", header = FALSE)

n <- dim(input)[1]
xs <- input[c(1, 3)]
xs <- xs - min(xs) + 1
ys <- input[c(2, 4)]
ys <- ys - min(ys) + 1

m <- matrix(0, max(ys), max(xs))

for (i in 1:n) {
    xsi <- xs[i, ]
    ysi <- ys[i, ]
    x1 <- xsi[[1]]
    x2 <- xsi[[2]]
    y1 <- ysi[[1]]
    y2 <- ysi[[2]]

    cs <- cbind(c(y1:y2), c(x1:x2))
    for (j in 1:nrow(cs)) {
        y <- cs[j, 1]
        x <- cs[j, 2]
        m[y, x] <- m[y, x] + 1
    }
}

sum(m > 1) # 4826
