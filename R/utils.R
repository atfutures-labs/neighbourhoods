
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
