m <- read.table("input.txt")

step <- 0

repeat {
    step <- step + 1
    print(sprintf("---- step %d", step))

    # increment each element as specified
    m <- m + 1

    # keep track of flashed cells
    mask <- m
    mask[, ] <- 1

    # repeat until no more flashes
    repeat {
        # get new flashes
        flash <- (m > 9) * mask

        # update mask so that each cell only flashes at most once
        mask <- mask * !flash

        # get indices for new flashes
        flashi <- which(flash == TRUE, arr.ind = TRUE)

        # are we done?
        if (nrow(flashi) == 0)
            break;

        # flash! - increment neigbours of flashing cells
        dv <- expand.grid(-1:1, -1:1)  # incudes [0,0] but it does not matter
        row <- rowSums(expand.grid(flashi[, 1], dv[, 1]))
        col <- rowSums(expand.grid(flashi[, 2], dv[, 2]))
        ns <- cbind(row, col)
        rows <- ns[, 1] >= 1 &
                ns[, 1] <= nrow(m) &
                ns[, 2] >= 1 &
                ns[, 2] <= ncol(m)
        ns <- ns[rows, ]

        # m[ns] < m[ns] + 1 does not work since ns contains duplicates
        for (i in 1:nrow(ns)) {
            v <- ns[i, ]
            m[v[1], v[2]] <- m[v[1], v[2]] + 1
        }
    }

    # reset flashed cells
    m[mask == 0] <- 0

    # we are done if all cells are zero
    if (sum(m != 0) == 0)
        break;
}

step # 279