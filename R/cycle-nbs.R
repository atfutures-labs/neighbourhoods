
#' Construct adjacency matrix of neighbourhood cycles
#'
#' @param cycles List of cycles obtained from \link{network_cycles}.
#' @return A `data.frame` of three columns:
#' \enumerate{
#' \item from - cycle from which connection is made
#' \item to - cycle to which connection is made
#' \item edges - List-column of all shared edges between (from, to) pair.
#' }
#' @export
adjacent_cycles <- function (cycles) {

    edge_ <- centrality <- cycle <- NULL # suppress no visible binding notes

    paths <- lapply (seq_along (cycles), function (i)
                     cbind (cycles [[i]], cycle = i))
    paths_df <- do.call (rbind, paths) |>
        dplyr::select (edge_, centrality, cycle) |>
        dplyr::mutate (edge_ = gsub ("\\_rev$", "", edge_))

    # dplyr here is around 10 times slower
    nbs <- lapply (unique (paths_df$cycle), function (i) {

        index_in <- which (paths_df$cycle == i)
        index_out <- which (!seq (nrow (paths_df)) %in% index_in)
        e_i <- paths_df$edge_ [index_in] # all edges in cycle i
        e_j <- paths_df [index_out, ]
        e_j <- e_j [which (e_j$edge_ %in% e_i), ] # all shared edges from other cycles

        if (nrow (e_j) == 0L)
           return (NULL)

        res <- lapply (split (e_j, f = as.factor (e_j$cycle)),
                      function (i)
                      list (cycle = i$cycle [1],
                            edges = i$edge_))
        edges <- lapply (res, function (i) i$edges)
        cycle_to <- vapply (res, function (i) i$cycle,
                            integer (1))

        res <- data.frame (from = i,
                           to = cycle_to,
                           edges = I (edges))

        return (res)
        })

    nbs <- do.call (rbind, nbs)
    row.names (nbs) <- NULL

    return (nbs)
}

#' Unconstract lists of shared neighbour edges returned from
#' \link{adjacent_cycles}.
#'
#' @noRd
uncontract_nbs <- function (nbs, graph, graph_c) {

    edge_map <- duplicated_edge_map (graph_c)
    graph <- duplicate_graph (graph)

    edges <- cpp_expand_edges (nbs$edges, edge_map, paths_are_list = TRUE)

    nbs$edges <- I (edges)

    return (nbs)
}

#' Add centrality and approximate area to 'nbs' data.
#'
#' Approxmiate area because it calcualtes planar areas from geodesic
#' coordinates, but plenty near enough for present purposes.
#'
#' @return Modified version of `nbs` with additional columns of areas for each
#' "from" and "to" neighbourhood, along with various measures of centrality
#' outside and along the shared boundaries.
#' @noRd
nbs_add_data <- function (nbs, paths, graph, graph_c, popdens_file = "") {

    paths_exp <- uncontract_cycles (paths, graph, graph_c)
    cli::cli_alert_success ("[6 / 9]: Uncontracted main cycles")

    srcproj <- .lonlat() #"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs <- .sph_merc() # "+proj=merc +a=6378137 +b=6378137"

    sf::sf_use_s2 (FALSE)

    xy <- lapply (seq_along (paths_exp), function (i) {
                      p_i <- paths_exp [[i]]
                      xy_i <- rbind (cbind (x = p_i$.vx0_x, y = p_i$.vx0_y),
                                     c (x = p_i$.vx1_x [nrow (p_i)],
                                        y = p_i$.vx1_y [nrow (p_i)]))
                      if (utils::tail (p_i$.vx1, 1L) != p_i$.vx0 [1]) {
                          xy_i <- rbind (xy_i, xy_i [1, ])
                      }
                      cbind (rep (i, nrow (xy_i)),
                             xy_i)
    })
    xy <- do.call (rbind, xy)
    path_num <- factor (xy [, 1])
    xy <- reproj::reproj (xy [, 2:3], target = crs, source = srcproj)
    # split destroys the matrix structure, so has to be re-applied:
    xy <- lapply (split (xy [, 1:2], f = path_num), function (i)
                  matrix (i, ncol = 2))

    xy_polys <- lapply (xy, function (a)
                            sf::st_polygon (list (a)))
    a <- sf::st_area (sf::st_sfc (xy_polys, crs = 3857))

    nbs$area_from <- a [nbs$from]
    nbs$area_to <- a [nbs$to]
    cli::cli_alert_success ("[7 / 9]: Calculated cycle areas")

    popdens <- popdens_to_poly (paths_exp, popdens_file)
    nbs$popdens_from <- popdens$popdens [nbs$from]
    nbs$popdens_to <- popdens$popdens [nbs$to]
    cli::cli_alert_success ("[8 / 9]: Added population density to cycles")

    nbs <- uncontract_nbs (nbs, graph, graph_c)

    extra_dat <- vapply (seq.int (nrow (nbs)), function (i) {

        p1 <- paths_exp [[nbs$from [i] ]]
        p2 <- paths_exp [[nbs$to [i] ]]
        # nbs$edges is a list-col:
        i1 <- which (!p1$edge_ %in% nbs$edges [[i]])
        i2 <- which (!p2$edge_ %in% nbs$edges [[i]])

        d1 <- sum (p1$d [i1], na.rm = TRUE)
        d2 <- sum (p2$d [i2], na.rm = TRUE)

        # (median, mean, max) of distance-weighted centrality
        one_centr <- function (centr, d) {
            centr_med <- centr_mn <- centr_max <- NA
            centr_d <- centr * d / sum (d)
            if (length (centr [which (!is.na (centr))]) > 0L) {
               centr_med <- stats::median (centr_d, na.rm = TRUE)
               centr_mn <- mean (centr_d, na.rm = TRUE)
               centr_max <- max (centr_d, na.rm = TRUE)
            }
            c (centr_med, centr_mn, centr_max)
        }
        c1 <- one_centr (p1$centrality [i1], p1$d [i1])
        c2 <- one_centr (p2$centrality [i2], p2$d [i2])

        p <- rbind (p1, p2)
        index_out <- which (!p$edge_ %in% nbs$edges [[i]])
        index_in <- which (p$edge_ %in% nbs$edges [[i]])
        c_in <- one_centr (p$centrality [index_in], p$d [index_in])
        c_out <- one_centr (p$centrality [index_out], p$d [index_out])

        c (d_in = sum (p$d [index_in]),
           d_out = sum (p$d [index_out]),
           centr_med_in = c_in [1],
           centr_mn_in = c_in [2],
           centr_max_in = c_in [3],
           centr_med_out = c_out [1],
           centr_mn_out = c_out [2],
           centr_max_out = c_out [3],
           centr_med_from = c1 [1],
           centr_mn_from = c1 [2],
           centr_max_from = c1 [3],
           centr_med_to = c2 [1],
           centr_mn_to = c2 [2],
           centr_max_to = c2 [3])
    }, numeric (14))
    cli::cli_alert_success ("[9 / 9]: Added additional data to cycles")

    extra_dat <- t (extra_dat)

    hw_in <- vapply (nbs$edges, function (e) {
        index <- match (e, graph$edge_)
        hw <- table (graph$highway [index])
        hw_i <- ifelse (length (hw == 1L), 1L, which.max (hw))
        return (names (hw) [hw_i])
    }, character (1))

    hw_out <- vapply (seq.int (nrow (nbs)), function (i) {

        p1 <- paths_exp [[nbs$from [i] ]]
        p2 <- paths_exp [[nbs$to [i] ]]
        # nbs$edges is a list-col:
        i1 <- which (!p1$edge_ %in% nbs$edges [[i]])
        i2 <- which (!p2$edge_ %in% nbs$edges [[i]])

        hw_type <- function (hw) {
            res <- ""
            if (length (hw) > 0L) {
                indx <- ifelse (length (hw == 1L), 1L, which.max (hw))
                res <- names (hw) [indx]
            }
            return (res)
        }
        hw1 <- hw_type (table (p1$highway [i1]))
        hw2 <- hw_type (table (p2$highway [i2]))

        return (c (hw1 = hw1, hw2 = hw2))
    }, character (2))
    hw_out <- t (hw_out)

    return (cbind (nbs,
                   hw_shared = hw_in,
                   hw_from = hw_out [, 1],
                   hw_to = hw_out [, 2],
                   extra_dat))
}

popdens_to_poly <- function (paths, popdens_file) {

    pop <- read_popdens (paths, popdens_file)

    polys <- lapply (paths, function (p) {
            xy <- cbind (x = c (p$.vx0_x, utils::tail (p$.vx1_x, 1)),
                         y = c (p$.vx0_y, utils::tail (p$.vx1_y, 1)))
            if (utils::tail (p$.vx1, 1L) != p$.vx0 [1]) {
                xy <- rbind (xy, xy [1, ])
            }
            sf::st_polygon (list (xy))
                }) |> sf::st_sfc (crs = 4326)

    sf::sf_use_s2 (FALSE)

    pip <- sf::st_within (pop$geometry, polys, sparse = TRUE)
    index <- which (vapply (pip, length, integer (1)) > 0L)
    pop <- pop [index, ]
    pip <- pip [index]
    pip <- lapply (seq_along (pip), function (i)
                   cbind (rep (i, length (pip [[i]])), pip [[i]]))
    pip <- do.call (rbind, pip)
    pip <- data.frame (poly = pip [, 2],
                       pop = pip [, 1])
    pip <- pip [order (pip$poly), ]
    rownames (pip) <- NULL
    pip$popdens <- pop$popdens [pip$pop]
    popdens <- vapply (split (pip, f = factor (pip$poly)),
                       function (i) mean (i$popdens),
                       numeric (1))
    res <- data.frame (poly = seq_along (polys), popdens = NA)
    res$popdens [as.integer (names (popdens))] <- popdens

    # Fill in NA values with nearest non-NA neighbours:
    index_na <- which (is.na (res$popdens))
    index_not <- which (!is.na (res$popdens))
    xy <- sf::st_centroid (polys)
    d <- sf::st_distance (xy [index_na], xy [index_not])
    index <- apply (d, 1, which.min)
    res$popdens [index_na] <- res$popdens [index_not [index]]

    return (res)
}

read_popdens <- function (paths, popdens_file) {

    if (!file.exists (popdens_file))
        stop ("popdens_file [", popdens_file, "] does not exist")

    xrange <- range (do.call (c, lapply (paths, function (p) p$.vx0_x)))
    yrange <- range (do.call (c, lapply (paths, function (p) p$.vx0_y)))
    bbox <- raster::extent (c (xrange, yrange))

    ftmp <- file.path (tempdir (), "temp.tif")
    ras <- raster::raster (popdens_file) |>
        raster::crop (bbox) |>
        raster::writeRaster (filename = ftmp, overwrite = TRUE)
    pop <- stars::read_stars (ftmp) |>
        sf::st_as_sf ()
    names (pop) [1] <- "popdens"
    pop$geometry <- sf::st_centroid (pop$geometry)

    return (pop)
}
