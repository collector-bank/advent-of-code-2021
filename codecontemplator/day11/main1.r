m <- read.table("input.txt")
m <- cbind(NA, rbind(NA, m, NA), NA) # pad with N/A to avoid trouble at edges

num_steps <- 100
flashc <- 0

for (i in 1:num_steps) {
    print(sprintf("---- step %d", i))

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
        #rows <- ns[, 1] >= 1 &
        #        ns[, 1] <= nrow(m) &
        #        ns[, 2] >= 1 &
        #        ns[, 2] <= ncol(m)
        #ns <- ns[rows, ]

        # m[ns] < m[ns] + 1 does not work since ns contains duplicates
        for (i in 1:nrow(ns)) {
            v <- ns[i, ]
            m[v[1], v[2]] <- m[v[1], v[2]] + 1
        }

        # keep track of number of flashes
        flashc <- flashc + nrow(flashi)
    }

    # reset flashed cells
    m[mask == 0] <- 0
}

flashc  # 1673