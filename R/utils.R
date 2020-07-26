
swap_cols <- function (x, nms) {
    temp <- x [[nms [1] ]]
    x [[nms [1] ]] <- x [[nms [2] ]]
    x [[nms [2] ]] <- temp
    return (x)
}

# measures extent to which each row of nbs$.vx0_(x,y) is to the left of
# this_edge
to_left0 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx0_y - this_edge$.vx0_y) -
        (nbs$.vx0_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}

# same but for nbs$.vx1_(x,y)
to_left1 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx1_y - this_edge$.vx0_y) -
        (nbs$.vx1_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}

duplicate_network <- function (x) {
    x2 <- x
    x2 <- swap_cols (x2, c (".vx0", ".vx1"))
    x2 <- swap_cols (x2, c (".vx0_x", ".vx1_x"))
    x2 <- swap_cols (x2, c (".vx0_y", ".vx1_y"))
    x2$edge_ <- paste0 (x2$edge_, "_rev")
    rbind (x, x2)
}
