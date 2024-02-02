#' @title Minimum Bounding Box
#'
#' @description Calculates the minimum bounding box of a group of objects
#'
#' @param x A vector of x (or longitude) coordinates.
#'
#' @param y A vector of y (or latitude) coordinates.
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A list with the bounding box coordinates, its heights, its width,
#' and the orientation of its longest side in degrees.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu},
#' Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{is_chull}}
#'
#' @export
calc_obb <- function(x, y, geo = FALSE) {

  H    <- grDevices::chull(x, y)  ## hull indices, vertices ordered clockwise
  n    <- length(H)      ## number of hull vertices
  hull <- as.matrix(data.frame(x = x[H], y = y[H]))        ## hull vertices

  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir <- diff(rbind(hull, hull[1, ])) ## hull vertices are circular
  hLens <- sqrt(rowSums(hDir^2))        ## length of basis vectors
  if (any(is.na(hLens)) || any(hLens == 0)) {
    return(list(pts = NA, height = NA, width = NA, angle = NA))
  }
  huDir <- diag(1 / hLens) %*% hDir       ## scaled to unit length

  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[, 2], huDir[, 1])

  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- tcrossprod(rbind(huDir, ouDir), hull)

  ## range of projections and corresponding width/height of bounding rectangle
  HO <- list(seq_len(n), (seq_len(n) + n))

  rangeH  <- matrix(numeric(n * 2), ncol = 2)  ## hull edge
  rangeO  <- matrix(numeric(n * 2), ncol = 2)  ## orthogonal subspace
  widths  <- numeric(n)
  heights <- numeric(n)

  for (i in 1:n) {
    rangeH[i, ] <- range(projMat[i, ])
    ## the orthogonal subspace is in the 2nd half of the matrix
    rangeO[i, ] <- range(projMat[n + i, ])
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }
  ## extreme projections for min-area rect in subspace coordinates
  ## hull edge leading to minimum-area
  eMin  <- which.min(widths * heights)

  ## move projections to rectangle corners
  hPts <- rbind(rangeH[eMin, ], rep(rangeO[eMin, 1], 2))
  oPts <- rbind(rangeH[eMin, ], rep(rangeO[eMin, 2], 2))

  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  ## basis formed by hull edge and orthogonal subspace
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[, c(2, 1)]))
  colnames(pts) <- c("X", "Y")

  ## angle of longer edge pointing up
  dPts <- diff(pts)
  e    <- dPts[which.max(rowSums(dPts^2)), ] ## one of the longer edges
  eUp  <- e * sign(e[2])       ## rotate upwards 180 deg if necessary
  deg  <- atan2(eUp[2], eUp[1])    ## angle in rads

  if (geo) {
    geodists <- geosphere::distGeo(p1 = pts)
    bwidth <- geodists[1]
    bheight <- geodists[2]
  } else {
    bwidth <- widths[eMin]
    bheight <- heights[eMin]
  }

  return(list(pts = pts, height = bheight, width = bwidth, angle = deg))
}


#' @title Group shape estimations
#'
#' @description Calculates how oblong the shape of a group is, relative
#' to its average moving direction, along with the properties of the minimum
#' bounding box around all objects.
#'
#' @param x A vector of x (or longitude) coordinates.
#'
#' @param y A vector of y (or latitude) coordinates.
#'
#' @param hs A vector of headings of the objects (in degrees).
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A list with the estimate of how oblong the group is, and
#' the details of the bounding box, i.e. its coordinates, height,
#' width, and orientation of its longest side in degrees.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu},
#' Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{is_chull}}
#'
#' @export
group_shape <- function(x, y, hs, geo = FALSE) {

  if (!all(length(x) == c(length(y), length(hs))))
    stop("x, y and hs should have the same length.")

  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(hs))
    stop("x, y and hs should be numeric.")

  theobb <- calc_obb(x, y, geo)

  hxvecs <- cos(hs)
  hyvecs <- sin(hs)

  avhead <- colMeans(cbind(hxvecs, hyvecs))
  avhead <- atan2(avhead[2], avhead[1])
  db <- abs(avhead - theobb$angle)
  db[db > pi & !is.na(db)] <- abs(db - 2 * pi)
  db[db > pi / 2 & !is.na(db)] <- pi - db
  theobb$shape <- abs(db)

  return(theobb)
}
