# netcdf_to_obs_utils_hypetools.R
#
# -------------------------------------------------------------------------------------------
#
# Added functions from HYPEtools:
#

# Additional libaries:
library(data.table) # Moved to netcdf_to_obs_utils.R

#' @importFrom data.table fwrite
#' @export
WriteForcKey <- function(x, filename = "ForcKey.txt") {
  fwrite(x, file = filename, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
}


#' @importFrom data.table fread
#' @export
ReadPTQobs <- function(filename, dt.format = "%Y-%m-%d", nrows = -1, type = "df", select = NULL, obsid = NULL) {
  
  ## import ptqobs file header, extract obsid attribute
  # import
  xattr <- readLines(filename,n = 1)
  # extract obsids
  sbd <- as.integer(strsplit(xattr, split = "\t")[[1]][-1])
  
  # argument checks
  if (!is.null(select) && !(1 %in% select)) {
    select <- c(1, select)
  }
  
  # handling output type user choice
  if (type == "df") {
    d.t <- F
  } else if (type == "dt") {
    d.t <- T
  } else {
    stop(paste("Unknown type", type, "."))
  }
  
  # create select vector from 'obsid' argument, overrides 'select' argument
  if (!is.null(obsid)) {
    if (!is.null(select)) {
      warning("Arguments 'select' and 'obsid' provided. 'obsid' takes precedence.")
    }
    te <- match(obsid, sbd)
    # stop if unknown obsids provided by user
    if (any(is.na(te))) {
      stop(paste0("Argument 'obsid': OBSIDs ", paste(obsid[is.na(te)], collapse = ", "), " not found in imported file."))
    }
    select <- c(1, te + 1)
    sbd <- obsid
  } else if (!is.null(select)) {
    # update obsid attribute to selected obsids
    sbd <- sbd[select[-1] - 1]
  }
  
  
  # create full select vector for fread, workaround for suspected bug in data.table (reported at https://github.com/Rdatatable/data.table/issues/2007)
  if (is.null(select) && is.null(obsid)) {
    select <- 1:(length(sbd) + 1)
  }
  
  # read the data
  x <- fread(filename, 
             na.strings = c("-9999", "****************", "-1.0E+04", "-1.00E+04", "-9.999E+03", "-9.9990E+03", "-9.99900E+03", "-9.999000E+03", "-9.9990000E+03", "-9.99900000E+03", "-9.999000000E+03"), 
             sep = "\t", header = T, data.table = d.t, nrows = nrows, select = select)

  # date conversion 
  xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
  x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
    print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])
    })
  
  
  ## add attributes
  
  attr(x, which = "obsid") <- sbd
  
  # conditional: timestep attribute identified by difference between first two entries
  tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
  if (!is.na(tdff)) {
    if (tdff == 24) {
      attr(x, which = "timestep") <- "day"
    } else {
      attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
    }
  } else {
    # add timestep attribute with placeholder value
    attr(x, which = "timestep") <- "unknown"
  }
  return(x)
} # ReadPTQobs

#' @export
#' @import sp
# @importFrom sp SpatialPolygonsDataFrame SpatialPolygons


# PlotMapOutput <- function(x, map, map.subid.column = 1, var.name = "", map.adj = 0, plot.legend = T, 
#                           legend.pos = "right", legend.title = NULL, legend.outer = F, legend.inset = c(0, 0), 
#                           col = "auto", col.ramp.fun, col.breaks = NULL, plot.scale = T, plot.arrow = T, 
#                           par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, restore.par = FALSE) {
  
#   # input argument checks
#   stopifnot(is.data.frame(x), dim(x)[2] == 2, class(map)=="SpatialPolygonsDataFrame", 
#             is.null(col.breaks) || is.numeric(col.breaks))
#   stopifnot(map.adj %in% c(0, .5, 1))
#   stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
#   if (length(col.breaks) == 1) {
#     col.breaks <- range(x[, 2], na.rm = T)
#     warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
#   }
#   if (!is.null(col.breaks) && (min(col.breaks) > min(x[, 2], na.rm = T) || max(col.breaks) < max(x[, 2], na.rm = T))) {
#     warning("Range of user-provided argument 'col.breaks' does not cover range of 'x[, 2]. 
#             Areas outside range will be excluded from plot.")
#   }
  
#   # check if deprecated argument col.ramp.fun was used
#   if (!missing(col.ramp.fun)) {
#     warning("Deprecated argument 'col.ramp.fun' used. Please use 'col' instead.")
#   }
  
#   # add y to legend inset if not provided by user
#   if (length(legend.inset) == 1) {
#     legend.inset[2] <- 0
#   }
  
#   # sort col.breaks to make sure breaks are in increasing order
#   if (!is.null(col.breaks)) {
#     col.breaks <- sort(col.breaks, decreasing = FALSE)
#     }
  
#   # save current state of par() variables which are altered below, for restoring on function exit
#   par.mar0 <- par("mar")
#   par.xaxs <- par("xaxs")
#   par.yaxs <- par("yaxs")
#   par.lend <- par("lend")
#   par.xpd <- par("xpd")
#   par.cex0 <- par("cex")
#   if (restore.par) {
#     on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex0))
#   }
  
#   # data preparation and conditional assignment of color ramp functions and break point vectors 
#   # to internal variables crfun and cbrks
  
#   if (is.function(col)) {
#     # Case 1: a color ramp palette function is supplied
#     crfun <- col
#     if (!is.null(col.breaks)) {
#       cbrks <- col.breaks
#     } else {
#       # color breaks: special defaults for some of the inbuilt color ramp functions
#       if (identical(col, ColDiffTemp)) {
#         # temperature differences
#         cbrks <- c(ifelse(min(x[,2]) < 7.5, min(x[,2]) - 1, 30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
#       } else if (identical(col, ColDiffGeneric)) {
#         # create a break point sequence which is centered around zero, with class withs based on equal intervals of the log-scaled
#         # variable distribution
#         cbrks <- c(rev(exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)) * -1), exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)))
#       } else {
#         # generic, quantile-based breaks for all other functions
#         cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
#       }
#     }
#   } else if (col[1] == "auto") {
#     # Case 2: limited set of pre-defined color ramps and break point vectors for select HYPE variables, with a generic "catch the rest" treatment 
#     # for undefined variables
#     if (toupper(var.name) == "CCTN") {
#       crfun <- ColNitr
#       cbrks <- c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000, ifelse(max(x[,2]) > 5000, max(x[,2]) + 1, 10000))
#       if (is.null(legend.title)) {
#         legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
#       }
#     } else if (toupper(var.name) == "CCTP") {
#       crfun <- ColPhos
#       cbrks <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, ifelse(max(x[,2]) > 250, max(x[,2]) + 1, 1000))
#       if (is.null(legend.title)) {
#         legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
#       }
#     } else if (toupper(var.name) == "COUT") {
#       crfun <- ColQ
#       cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(x[,2]) > 500, max(x[,2]) + 1, 2000))
#       if (is.null(legend.title)) {
#         legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
#       }
#     } else if (toupper(var.name) == "TEMP") {
#       crfun <- ColTemp
#       cbrks <- c(ifelse(min(x[,2]) < -7.5, min(x[,2]) - 1, -30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
#       if (is.null(legend.title)) {
#         legend.title <- expression(paste("Air Temp. ("*degree, "C)"))
#       }
#     } else {
#       crfun <- ColDiffGeneric
#       cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
#     }
#   } else if (is.vector(col)) {
#     # Case 3: a vector of colors
#     crfun <- NULL
#     if (!is.null(col.breaks)) {
#       cbrks <- col.breaks
#       if (length(col) != (length(cbrks) - 1)) {
#         stop("If colors are specified as vector in 'col', the number of colors in 'col' must be one less than the number of breakpoints in 'col.breaks'.")
#       }
#     } else {
#       cbrks <- quantile(x[, 2], probs = seq(0, 1, length.out = length(col) + 1), na.rm = T)
#     } 
#   } else {
#     # Error treatment for all other user input
#     stop("Invalid 'col' argument.")
#   }

  
#   # in variables with large numbers of "0" values, the lower 10%-percentiles can be repeatedly "0", which leads to an error with cut,
#   # so cbrks is shortened to unique values (this affects only the automatic quantile-based breaks)
#   # if just one value remains (or was requested by user), replace crbks by minmax-based range (this also resolves unexpected behaviour
#   # with single-value cbrks in 'cut' below).
#   if (is.null(crfun) && length(unique(cbrks)) < length(cbrks)) {
#     # warn, if user defined colors are discarded because of removal of quantile-based classes
#     warning("User-defined colors in 'col' truncated because of non-unique values in quantile-based color breaks. Provide breaks in 
#             'col.breaks' to use all colors.")
#   }
#   cbrks <- unique(cbrks)
#   if (length(cbrks) == 1) {
#     cbrks <- range(cbrks) + c(-1, 1)
#   }
#   if (is.null(crfun)) {
#     # truncate user-defined colors
#     col <- col[1:(length(cbrks) - 1)]
#   }
#   # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
#   x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = T)
#   # replace the factor levels with color codes using the color ramp function assigned above or user-defined colors
#   if (is.null(crfun)) {
#     levels(x[, 3]) <- col
#   } else {
#     levels(x[, 3]) <- crfun(length(cbrks) - 1)
#   }
  
#   # convert to character to make it conform to plotting requirements below
#   x[, 3] <- as.character(x[, 3])
#   # give it a name
#   names(x)[3] <- "color"
  
#   # add x to subid map table (in data slot, indicated by @), merge by SUBID
#   map@data <- data.frame(map@data, x[match(map@data[, map.subid.column], x[,1]),])
  
#   # update legend title if none was provided by user or "auto" selection
#   if (is.null(legend.title)) {
#     legend.title <- toupper(var.name)
#   }
  
#   # par settings: lend set to square line endings because the legend below works with very thick lines 
#   # instead of boxes (a box size limitation work-around); xpd set to allow for plotting a legend on the margins
#   if (!add) {
#     #x11(width = 15, height = 5)
#     #par(mfcol = c(1, 3))
#     #plot(0,type='n',axes=FALSE,ann=FALSE)
#     par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = T, cex = par.cex)
#     #plot.window(xlim = 0:1, ylim = 0:1)
#     frame()
#   } else {
#     par(lend = 1, xpd = T, cex = par.cex)
#   }
  
  
#   ## the positioning of all plot elements works with three scales for the device's plot region: 
#   ## inches, fraction, and map coordinates
  
#   # plot width (inches)
#   p.in.wd <- par("pin")[1]
  
#   # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
#   # legend colors
#   if (is.null(crfun)) {
#     lcol <- col
#   } else {
#     lcol <- crfun(length(cbrks) - 1)
#   }
#   leg.fr.pos <- legend(legend.pos, legend = rep(NA, length(cbrks) - 1),
#                col = lcol, lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
#   # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates) 
#   leg.fr.wd <- leg.fr.pos$rect$w
#   # legend box element height (fraction), with workaround for single-class maps
#   if (length(leg.fr.pos$text$y) == 1) {
#     te <- legend(legend.pos, legend = rep(NA, length(cbrks)),
#                  col = ColQ(length(cbrks)), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
#     legbx.fr.ht <- diff(c(te$text$y[length(cbrks)], te$text$y[length(cbrks) - 1]))
#   } else {
#     legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
#   }
  
  
#   ## prepare legend annotation
  
#   # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
#   ann.txt <- signif(cbrks, digits = 2)
#   # conditional: remove outer break points
#   if (!legend.outer) {
#     ann.txt[c(1, length(ann.txt))] <- ""
#   }
#   # annotation width (inches)
#   ann.in.wd <- max(strwidth(ann.txt, "inches"))
#   # legend inset required to accomodate text annotation, and scalebar (always below legend)
#   leg.inset <- c(ann.in.wd/p.in.wd, if(legend.pos %in% c("bottomright", "bottomleft")) {0.1} else {0})
  
#   # conditional on legend placement side (legend annotation always right of color boxes)
#   if (legend.pos %in% c("bottomright", "right", "topright")) {
    
#     # update legend inset
#     legend.inset <- legend.inset + leg.inset
#     ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
#     # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
#     if (add) {
#       f.inset.x <- par("usr")[2] - par("usr")[1]
#       f.inset.y <- par("usr")[4] - par("usr")[3]
#     } else {
#       f.inset.x <- 1
#       f.inset.y <- 1
#     }
#     ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) - legend.inset[1] * f.inset.x - 0.01
#     if (legend.pos == "bottomright") {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
#     } else if (legend.pos == "right") {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
#     } else {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
#     }
    
#   } else {
#     # left side legend
#     # update legend inset
#     legend.inset[2] <- legend.inset[2] + leg.inset[2]
#     ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
#     # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
#     if (add) {
#       f.inset.x <- par("usr")[2] - par("usr")[1]
#       f.inset.y <- par("usr")[4] - par("usr")[3]
#     } else {
#       f.inset.x <- 1
#       f.inset.y <- 1
#     }
#     ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] * f.inset.x - 0.01
#     if (legend.pos == "bottomleft") {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
#     } else if (legend.pos == "left") {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
#     } else {
#       ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
#     }
#   }
  
  
#   ## calculate coordinates for map positioning
  
#   # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
#   if (is.projected(map)) {
#     bbx <- bbox(map)
#     # map side ratio (h/w)
#     msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
#     # plot area side ratio (h/w)
#     psr <- par("pin")[2] / par("pin")[1]
#   } else {
#     bbx <- bbox(map)
#     # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
#     par(new = T)
#     plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ])
#     # create a map side ratio based on the device region in user coordinates and the map bounding box
#     p.range.x <- diff(par("usr")[1:2])
#     p.range.y <- diff(par("usr")[3:4])
#     m.range.x <- diff(bbox(map)[1, ])
#     m.range.y <- diff(bbox(map)[2, ])
#     # map side ratio (h/w)
#     msr <- m.range.y / m.range.x
#     # plot area side ratio (h/w)
#     psr <- p.range.y / p.range.x
#   }
  
  
#   # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
#   if (msr > psr) {
#     # map is smaller than plot window in x direction, map can be moved left or right
#     if (map.adj == 0) {
#       pylim <- as.numeric(bbx[2, ])
#       pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
#     } else if (map.adj == .5) {
#       pylim <- as.numeric(bbx[2, ])
#       pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
#     } else {
#       pylim <- as.numeric(bbx[2, ])
#       pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
#     }
#   } else {
#     # map is smaller than plot window in y direction, map can be moved up or down
#     if (map.adj == 0) {
#       pxlim <- as.numeric(bbx[1, ])
#       pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
#     } else if (map.adj == .5) {
#       pxlim <- as.numeric(bbx[1, ])
#       pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
#     } else {
#       pxlim <- as.numeric(bbx[1, ])
#       pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
#     }
#   }
  
  
#   ## plot the map and add legend using the positioning information derived above
  
#   # map, plot in current frame if not added because a new frame was already created above for calculating all the coordinates
#   if (!add) {
#     par(new = TRUE)
#   }
#   plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
#   # legend
#   if (plot.legend) {
#     legend(legend.pos, legend = rep(NA, length(cbrks) - 1), inset = legend.inset, 
#            col = lcol, lty = 1, lwd = 14,  bty = "n", title = legend.title)
#     # convert annotation positioning to map coordinates, only if 'add' is FALSE
#     # then plot annotation text
#     if (!add) {
#       ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
#       ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
#       text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
#     } else {
#       text(x = ann.fr.x, y = ann.fr.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
#     }
#   }
  
  
#   ## scale position (reference point: lower left corner), also used as reference point for north arrow
#   ## conditional on 'add'
  
#   if (add) {
    
#     # x position conditional on legend placement side
#     if (legend.pos %in% c("bottomright", "right", "topright")) {
#       lx <- par("usr")[2] - signif(diff(par("usr")[1:2])/4, 0) - legend.inset[1] * diff(par("usr")[1:2])
#     } else {
#       lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
#     }
    
#     # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
#     if (legend.pos %in% c("bottomright", "bottomleft")) {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]*f.inset.y/2)
#     } else if (legend.pos %in% c("right", "left")) {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2]/2 - .1) * f.inset.y)
#     } else {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2]/2 - .1) * f.inset.y)
#     }
#   } else {
    
#     # x position conditional on legend placement side
#     if (legend.pos %in% c("bottomright", "right", "topright")) {
#       lx <- pxlim[2] - signif(diff(bbx[1,])/4, 0) - legend.inset[1] * diff(pxlim)
#     } else {
#       lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
#     }
    
#     # y position conditional legend placement position
#     if (legend.pos %in% c("bottomright", "bottomleft")) {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2) * diff(pylim) + pylim[1]
#     } else if (legend.pos %in% c("right", "left")) {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
#     } else {
#       ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
#     }
#   }
  
#   if (plot.scale) {
#     if (!is.projected(map)) {
#       warning("Scale bar meaningless with un-projected maps. Set 'plot.scale = F' to remove it.")
#     }
#     if (!add) {
#       ldistance <- signif(diff(bbx[1,])/4, 0)
#     } else {
#       ldistance <- signif(diff(par("usr")[1:2])/4, 0)
#       }
#     .Scalebar(x = lx, 
#               y = ly, 
#               distance = ldistance, 
#               scale = 0.001, t.cex = 0.8)
#   }
  
#   if (plot.arrow) {
    
#     if (add) {
#       nlen <- diff(par("usr")[1:2])/70
#       # north arrow x position conditional on side where legend is plotted
#       if (legend.pos %in% c("bottomright", "right", "topright")) {
#         nx <- lx - 0.02 * diff(par("usr")[1:2])
#       } else {
#         nx <- lx + signif(diff(par("usr")[1:2])/4, 0) + 0.055 * diff(par("usr")[1:2])
#       }
#     } else {
#       nlen <- diff(bbx[1,])/70
#       # north arrow x position conditional on side where legend is plotted
#       if (legend.pos %in% c("bottomright", "right", "topright")) {
#         nx <- lx - 0.02 * diff(pxlim)
#       } else {
#         nx <- lx + signif(diff(bbx[1,])/4, 0) + 0.055 * diff(pxlim)
#       }
#     }
    
#     .NorthArrow(xb = nx, 
#                 yb = ly, 
#                 len = nlen, cex.lab = .8)
#   }
  
  
#   # invisible unless assigned: return map with added data and color codes
#   invisible(map)
# } # PlotMapOutput
