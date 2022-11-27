
process_time <- function (pt0) {

    pt1 <- proc.time () [3] - pt0 [3]

    hh_num <- floor (pt1 / 3600)
    hh <- sprintf ("%02i", hh_num)
    pt1 <- pt1 - hh_num * 3600

    mm_num <- floor (pt1 / 60)
    mm <- sprintf ("%02i", mm_num)

    ss <- round (pt1 - mm_num * 60, digits = 2)
    ss <- ifelse (ss < 10, paste0 ("0", ss), paste0 (ss))

    paste0 (hh, ":", mm, ":", ss)
}

bb_from_graph <- function (graph, city) {

    v <- m4ra_vertices (graph, city)
    x <- mean (range (v$x)) + c (-0.5, 0.5) * diff (range (v$x)) 
    y <- mean (range (v$y)) + c (-0.5, 0.5) * diff (range (v$y))
    bb <- rbind (x, y)
    colnames (bb) <- c ("min", "max")

    return (bb)
}
