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
    # print(sprintf("%d,%d -> %d,%d", x1, y1, x2, y2))
    if (x1 == x2) {
        for (y in y1:y2) {
            m[y, x1] <- m[y, x1] + 1
        }
    } else if (y1 == y2) {
        for (x in x1:x2) {
            m[y1, x] <- m[y1, x] + 1
        }
    }
}

sum(m > 1) # 4826