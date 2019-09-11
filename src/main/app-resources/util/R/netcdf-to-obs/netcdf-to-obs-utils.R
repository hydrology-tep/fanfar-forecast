# hydrogfd.to.hype.obsfiles_utils.R
#
# R script with utility functions to generate HYPE model forcing data based on HydroGFD/GCM NetCDF data
#
# Inputs:
#
#   - (optionally) HydroGFD point and polygon layer shapefiles with lat, long, elevation, row (lat), col (lon), and id=1e6 + col*1e3 + row
#   - HYPE model subbasin polygon shapefile (with subbasin identifier SUBID in attribute data)
#   - HydroGFD nc-files
#
#        daily precipitation        pr_[...]_[yyyy-yyyy].nc
#        daily mean temperature     tas_[...]_[yyyy-yyyy].nc
#        daily minimum temperature  tasmin_[...]_[yyyy-yyyy].nc
#        daily maximum temperature  tasmax_[...]_[yyyy-yyyy].nc
#
# output data:
#   - Pobs.txt, Tobs.txt, TMINobs.txt, TMAXobs.txt, ForcKey.txt, 
#   - text-file with SUBID-GFD gridpoint links and corresponding weights.
#   - Rdata binary data file with the gfd2subbasin link and weights for repeated usage.  
#
# usage:
#   - see hydrogfd.to.hype.obsfiles_first.run_example.R
#   - see hydrogfd.to.hype.obsfiles_second.run_example.R
#
# Last updated 2019-01-30, David Gustafsson, SMHI
# ---------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
# Initialisations
# -------------------------------------------------------------------------------------------
{
  # Packages (we need to load certain packages - install if you dont have them. HYPEtools can be found on github)
  {
    #library(HYPEtools) # to work with HYPE data formats
    library(ncdf4)     # to read the GFD netcdf files
    library(sp)        # spatial data base package
    library(rgdal)     # GDAL functions
    library(rgeos)     # GEOS functions
    library(raster)    # raster functions
  }
  
  # Define CRS for WGS84 lat-long (projected CRS should now be provided by the user (see run example scripts))
  {
    CRSlatlon = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  }
  
}

# -------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------
{

{
### PlotMapOutput from HYPEtools
### ----------------------------
ColNitr <- colorRampPalette(c("#fff5a8", "#6b0601"))
ColPhos <- colorRampPalette(c("#dcf5e9", "#226633"))
ColPrec <- colorRampPalette(c("#e0e7e8", "#00508c"))
ColTemp <- colorRampPalette(c("#0000ff", "#0080ff", "#80ffff", "#f0f0f0", "#ffff00", "#ff8000", "#ff0000"))
ColQ <- colorRampPalette(c("#ede7ff", "#2300ff"))
ColDiffTemp <- colorRampPalette(c("#2892c7", "#e9f0e8", "#e81515"))
ColDiffGeneric <- colorRampPalette(c("#e81515", "#e9f0e8", "#2892c7"))
ColBlues <- colorRampPalette(c("#0a0a96", "#a3a3db"))
ColReds <- colorRampPalette(c("#f77497", "#670101"))
ColGreens <- colorRampPalette(c("#04eb04", "#004400"))
ColYOB <- colorRampPalette(c("#ffe851", "#da531d", "#5b1e00"))
ColPurples <- colorRampPalette(c("#da62ed", "#300275"))

# Internal function to add a North arrow to a map plot.
# Code adapted from function north.arrow in package GISTools Chris Brunsdon <christopher.brunsdon@nuim.ie>.
# xb:      The x-centre (in map units) of the arrow base.
# yb:      The y-centre (in map units) of the arrow base.
# len:     A scaling length (in map units) for arrow dimensioning.
# lab:     Label to appear under the arrow
# cex.lab: Scale factor for the label for the arrow.
# tcol:    The colour of the label text.
# ...:     Other graphical parameters passed to the drawing of the arrow.
.NorthArrow <- function (xb, yb, len, lab = "N", cex.lab = 1, tcol = "black", ...) {
  sx <- len * .5
  sy <- len
  arrow.x = c(-1, 1, 1, 2, 0, -2, -1, -1)
  arrow.y = c(0, 0, 2, 2, 4, 2, 2, 0)
  polygon(xb + arrow.x * sx, yb + arrow.y * sy, ...)
  text(xb, yb - strheight(lab, cex = cex.lab) * .9, lab, cex = cex.lab, adj = 0.4, 
       col = tcol)
}



PlotMapOutput <- function(x, map, map.subid.column = 1, var.name = "", map.adj = 0, plot.legend = T, 
                          legend.pos = "right", legend.title = NULL, legend.outer = F, legend.inset = c(0, 0), 
                          col.ramp.fun = "auto", col.breaks = NULL, plot.scale = T, plot.arrow = T, 
                          par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, restore.par = FALSE,main.title=NULL,par.mai=NULL) {
  
  # input argument checks
#  stopifnot(is.data.frame(x), dim(x)[2] == 2, class(map)=="SpatialPolygonsDataFrame", 
#            is.null(col.breaks) || is.numeric(col.breaks))
#  stopifnot(map.adj %in% c(0, .5, 1))
#  stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
#  if (length(col.breaks) == 1) {
#    col.breaks <- range(x[, 2], na.rm = T)
#    warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
#  }
#  if (!is.null(col.breaks) && (min(col.breaks) > min(x[, 2], na.rm = T) || max(col.breaks) < max(x[, 2], na.rm = T))) {
#    warning("Range of user-provided argument 'col.breaks' does not cover range of 'x[, 2]. 
#            Areas outside range will be excluded from plot.")
#  }
  
  # add y to legend inset if not provided by user
  if (length(legend.inset) == 1) {
    legend.inset[2] <- 0
  }
  
  # save current state of par() variables which are altered below, for restoring on function exit
  par.mar0 <- par("mar")
  par.xaxs <- par("xaxs")
  par.yaxs <- par("yaxs")
  par.lend <- par("lend")
  par.xpd <- par("xpd")
  par.cex0 <- par("cex")
  par.mai0 <- par("mai")
  if(is.null(par.mai)){
    par.mai = par.mai0
  }
  if (restore.par) {
    on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex0, mai = par.mai0))
  }
  
  # data preparation and conditional assignment of color ramp functions and break point vectors 
  # to internal variables crfun and cbrks
  
  if (is.function(col.ramp.fun)) {
    # Case 1: a color ramp palette function is supplied
    crfun <- col.ramp.fun
    if (!is.null(col.breaks)) {
      cbrks <- col.breaks
    } else {
      cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
    }
  } else if (is.character(col.ramp.fun)) {
    # Case 2: no color ramp palette function is supplied and one of the predefined is requested
    # First treat the specific palette function strings, then "auto" requests, and last error handling for all other strings.
    # Specific palettes get a generic class break points if not provided with another by the user
    # THIS CODE IS REPETITIVE, COULD BE STREAMLINED BY BREAKING OUT cbrks ASSIGNMENT
    if (col.ramp.fun == "ColNitr") {
      crfun <- ColNitr
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPhos") {
      crfun <- ColPhos
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColTemp") {
      crfun <- ColTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPrec") {
      crfun <- ColPrec
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColQ") {
      crfun <- ColQ
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColDiffTemp") {
      crfun <- ColDiffTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- c(ifelse(min(x[,2]) < 7.5, min(x[,2]) - 1, 30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
        #cbrks <- quantile(x[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "ColDiffGeneric") {
      crfun <- ColDiffGeneric
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        # create a break point sequence which is centered around zero, with class withs based on equal intervals of the log-scaled
        # variable distribution
        cbrks <- c(rev(exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)) * -1), exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)))
        #cbrks <- quantile(x[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "auto") {
      # Here follows a limited set of pre-defined color ramps and break point vectors for select HYPE variables, and
      # at the end a generic "catch the rest" treatment for undefined variables
      if (toupper(var.name) == "CCTN") {
        crfun <- ColNitr
        cbrks <- c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000, ifelse(max(x[,2]) > 5000, max(x[,2]) + 1, 10000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "CCTP") {
        crfun <- ColPhos
        cbrks <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, ifelse(max(x[,2]) > 250, max(x[,2]) + 1, 1000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "COUT") {
        crfun <- ColQ
        cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(x[,2]) > 500, max(x[,2]) + 1, 2000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
        }
      } else if (toupper(var.name) == "TEMP") {
        crfun <- ColTemp
        cbrks <- c(ifelse(min(x[,2]) < -7.5, min(x[,2]) - 1, -30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Air Temp. ("*degree, "C)"))
        }
      } else {
        crfun <- ColDiffGeneric
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else {
      # Error treatment for all other strings
      stop("Invalid 'col.ramp.fun' argument. Neither a function nor a recognised character string.")
    }
  } else {
    # Error treatment for all other types of user input
    stop("Invalid 'col.ramp.fun' argument. Neither a function nor a character string.")
  }
  
  
  # in variables with large numbers of "0" values, the lower 10%-percentiles can be repeatedly "0", which leads to an error with cut,
  # so cbrks is shortened to unique values (this affects only the automatic quantile-based breaks)
  # if just one value remains (or was requested by user), replace crbks by minmax-based range (this also resolves unexpected behaviour
  # with single-value cbrks in 'cut' below).
  cbrks <- unique(cbrks)
  if (length(cbrks) == 1) {
    cbrks <- range(cbrks) + c(-1, 1)
  }
  # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
  x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(x[, 3]) <- crfun(length(cbrks) - 1)
  # convert to character to make it conform to plotting requirements below
  x[, 3] <- as.character(x[, 3])
  # give it a name
  names(x)[3] <- "color"
  
  # add x to subid map table (in data slot, indicated by @), merge by SUBID
  map@data <- data.frame(map@data, x[match(map@data[, map.subid.column], x[,1]),])
  
  # update legend title if none was provided by user or "auto" selection
  if (is.null(legend.title)) {
    legend.title <- toupper(var.name)
  }
  #x11(width = 4.5, height = 9)
  # par settings: lend set to square line endings because the legend below works with very thick lines 
  # instead of boxes (a box size limitation work-around); xpd set to allow for plotting a legend on the margins
  if (!add) {
    par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = T, cex = par.cex, mai = par.mai)
    #plot.window(xlim = 0:1, ylim = 0:1)
    frame()
  } else {
    par(lend = 1, xpd = T, cex = par.cex)
  }
  
  
  ## the positioning of all plot elements works with three scales for the device's plot region: 
  ## inches, fraction, and map coordinates
  
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  
  # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.pos <- legend(legend.pos, legend = rep(NA, length(cbrks) - 1),
                       col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
  # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.wd <- leg.fr.pos$rect$w
  # legend box element height (fraction), with workaround for single-class maps
  if (length(leg.fr.pos$text$y) == 1) {
    te <- legend(legend.pos, legend = rep(NA, length(cbrks)),
                 col = crfun(length(cbrks)), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
    legbx.fr.ht <- diff(c(te$text$y[length(cbrks)], te$text$y[length(cbrks) - 1]))
  } else {
    legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
  }
  
  
  ## prepare legend annotation
  
  # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
  ann.txt <- signif(cbrks, digits = 2)
  # conditional: remove outer break points
  if (!legend.outer) {
    ann.txt[c(1, length(ann.txt))] <- ""
  }
  # annotation width (inches)
  ann.in.wd <- max(strwidth(ann.txt, "inches"))
  # legend inset required to accomodate text annotation, and scalebar (always below legend)
  leg.inset <- c(ann.in.wd/p.in.wd, if(legend.pos %in% c("bottomright", "bottomleft")) {0.1} else {0})
  
  # conditional on legend placement side (legend annotation always right of color boxes)
  if (legend.pos %in% c("bottomright", "right", "topright")) {
    
    # update legend inset
    legend.inset <- legend.inset + leg.inset
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) - legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomright") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "right") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
    
  } else {
    # left side legend
    # update legend inset
    legend.inset[2] <- legend.inset[2] + leg.inset[2]
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomleft") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "left") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
  }
  
  
  ## calculate coordinates for map positioning
  
  # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
  if (is.projected(map)) {
    bbx <- bbox(map)
    # map side ratio (h/w)
    msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
    # plot area side ratio (h/w)
    psr <- par("pin")[2] / par("pin")[1]
  } else {
    bbx <- bbox(map)
    # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
    if(!is.null(main.title)){
      plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ],main=main.title)
    }else{
      plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ])
    }
    # create a map side ratio based on the device region in user coordinates and the map bounding box
    p.range.x <- diff(par("usr")[1:2])
    p.range.y <- diff(par("usr")[3:4])
    m.range.x <- diff(bbox(map)[1, ])
    m.range.y <- diff(bbox(map)[2, ])
    # map side ratio (h/w)
    msr <- m.range.y / m.range.x
    # plot area side ratio (h/w)
    psr <- p.range.y / p.range.x
  }
  
  
  # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
  if (msr > psr) {
    # map is smaller than plot window in x direction, map can be moved left or right
    if (map.adj == 0) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
    } else if (map.adj == .5) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
    } else {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
    }
  } else {
    # map is smaller than plot window in y direction, map can be moved up or down
    if (map.adj == 0) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
    } else if (map.adj == .5) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
    } else {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
    }
  }
  
  
  ## plot the map and add legend using the positioning information derived above
  
  # map, plot in current frame if not added because a new frame was already created above for calculating all the coordinates
  if (!add) {
    par(new = TRUE)
  }
  if(!is.null(main.title)){
    plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add, main=main.title)
  }else{
    plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
  }
  # legend
  if (plot.legend) {
    legend(legend.pos, legend = rep(NA, length(cbrks) - 1), inset = legend.inset, 
           col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title)
    # convert annotation positioning to map coordinates, only if 'add' is FALSE
    # then plot annotation text
    if (!add) {
      ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
      ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
      text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    } else {
      text(x = ann.fr.x, y = ann.fr.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    }
  }
  
  
  ## scale position (reference point: lower left corner), also used as reference point for north arrow
  ## conditional on 'add'
  
  if (add) {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- par("usr")[2] - signif(diff(par("usr")[1:2])/4, 0) - legend.inset[1] * diff(par("usr")[1:2])
    } else {
      lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
    }
    
    # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]*f.inset.y/2)
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2]/2 - .1) * f.inset.y)
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2]/2 - .1) * f.inset.y)
    }
  } else {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- pxlim[2] - signif(diff(bbx[1,])/4, 0) - legend.inset[1] * diff(pxlim)
    } else {
      lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
    }
    
    # y position conditional legend placement position
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2) * diff(pylim) + pylim[1]
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    }
  }
  
  if (plot.scale) {
    if (!is.projected(map)) {
      warning("Scale bar meaningless with un-projected maps. Set 'plot.scale = F' to remove it.")
    }
    if (!add) {
      ldistance <- signif(diff(bbx[1,])/4, 0)
    } else {
      ldistance <- signif(diff(par("usr")[1:2])/4, 0)
    }
    .Scalebar(x = lx, 
              y = ly, 
              distance = ldistance, 
              scale = 0.001, t.cex = 0.8)
  }
  
  if (plot.arrow) {
    
    if (add) {
      nlen <- diff(par("usr")[1:2])/70
      # north arrow x position conditional on side where legend is plotted
      if (legend.pos %in% c("bottomright", "right", "topright")) {
        nx <- lx - 0.02 * diff(par("usr")[1:2])
      } else {
        nx <- lx + signif(diff(par("usr")[1:2])/4, 0) + 0.055 * diff(par("usr")[1:2])
      }
    } else {
      nlen <- diff(bbx[1,])/70
      # north arrow x position conditional on side where legend is plotted
      if (legend.pos %in% c("bottomright", "right", "topright")) {
        nx <- lx - 0.02 * diff(pxlim)
      } else {
        nx <- lx + signif(diff(bbx[1,])/4, 0) + 0.055 * diff(pxlim)
      }
    }
    
    .NorthArrow(xb = nx, 
                yb = ly, 
                len = nlen, cex.lab = .8)
  }
  
  
  # invisible unless assigned: return map with added data and color codes
  invisible(map)
}
}



{

##
## ReadPTQobs
##
ReadPTQobs <- function (filename, dt.format = "%Y-%m-%d", nrows = -1) {
  
  ## import ptqobs file header, extract attribute
  # import
  xattr <- readLines(filename,n = 1)
  # extract obsids
  sbd <- as.integer(strsplit(xattr, split = "\t")[[1]][-1])
  
  # read the data
  x <- fread(filename,  na.strings = "-9999", sep = "\t", header = T, data.table = F, nrows = nrows)
  #colClasses = c("NA", rep("numeric", length(sbd))))
  
  attr(x, which = "obsid") <- sbd
  
  # date conversion 
  xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
  x[, 1] <- xd
  #x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
  #  print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])
  # })
  
  return(x)
}
}

{
##
## WritePTQobs
##
WritePTQobs <- function (x, filename, dt.format = "%Y-%m-%d", digits = 3, nsmall = 1, obsid = NULL) {
  
  ## check if consistent header information is available, obsid arguments take precedence before attribute
  if(!is.null(obsid)) {
    if (length(obsid) == ncol(x) - 1) {
      header <- c("DATE", obsid)
    } else {
      stop("Length of function argument 'obsid' does not match number of obsid columns in export object.")
    }
  } else if (!is.null(attr(x, which = "obsid"))) {
    if (length(attr(x, which = "obsid")) == ncol(x) - 1) {
      header <- c("DATE", attr(x, which = "obsid"))
    } else {
      stop("Length of attribute 'obsid' does not match number of obsid columns in export object.")
    }
  } else {
    stop("No information available from 'obsid' argument or 'obsid' attribute to construct export header.")
  }
  
  # date conversion, conditional on that the date column is a posix class
  if (any(class(x[, 1]) == "POSIXct")) {
    x[, 1] <- format(x[, 1], format = dt.format)
  } else {
    warning("First column in export data frame is not of class 'POSIXct', will be exported unchanged.")
  }
  
  # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  x[is.na(x)] <- -9999
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = header)
  
}
}

{
WriteForcKey <- function(x, filename = "ForcKey.txt") {
  write.table(x, filename, col.names = T, sep = "\t", quote = F, row.names = F)
}
}

  # Generate polygons and point layers for GFD grids
  {
    # Make spatial layers for the GFD gridpoints (point and polygon)
    makeGFDSpatialLayers <- function(CRU_elevation.nc=NULL,dsn=NULL,layer.point=NULL,layer.poly=NULL){
      # read CRU_elevation netcdf file
      {
        # open nc file
        nc.elev = nc_open(CRU_elevation.nc)
        
        # lon = dim[1], lat = dim[2]
        nc.elev.nlon=nc.elev$dim[[1]]$len
        nc.elev.nlat=nc.elev$dim[[2]]$len
        
        # lon and lat values
        nc.elev.lon=nc.elev$dim[[1]]$vals
        nc.elev.lat=nc.elev$dim[[2]]$vals
        
        # elevation values
        nc.elev.var = nc.elev$var[["elevation"]]
        nc.elev.val = ncvar_get(nc = nc.elev,varid = nc.elev.var,start = c(1,1,1), count=c(nc.elev.nlon,nc.elev.nlat,1))
      }
      
      # Point layer at GFD grid centre coordinates
      {
        # matrices with data on same form as CRU_elevation matrix
        gfd.lon   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.lat   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.row   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.col   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.id    = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.elev  = nc.elev.val
        gfd.id[1:(nc.elev.nlon*nc.elev.nlat)]=1:(nc.elev.nlon*nc.elev.nlat)
        for(i in 1:nc.elev.nlon){
          gfd.lat[i,] =  nc.elev.lat
          gfd.col[i,] =  i
        }
        for(i in 1:nc.elev.nlat){
          gfd.lon[,i] =  nc.elev.lon
          gfd.row[,i] =  i
        }
        
        # data frame for the centre-point shapefile
        gfd.point = data.frame(
          "lon"=gfd.lon[1:(nc.elev.nlon*nc.elev.nlat)],
          "lat"=gfd.lat[1:(nc.elev.nlon*nc.elev.nlat)],
          "row"=gfd.row[1:(nc.elev.nlon*nc.elev.nlat)],
          "col"=gfd.col[1:(nc.elev.nlon*nc.elev.nlat)],
          "id"=gfd.id[1:(nc.elev.nlon*nc.elev.nlat)],
          "elev"=gfd.elev[1:(nc.elev.nlon*nc.elev.nlat)],
          "x"=gfd.lon[1:(nc.elev.nlon*nc.elev.nlat)],
          "y"=gfd.lat[1:(nc.elev.nlon*nc.elev.nlat)])
        coordinates(gfd.point)<-c("x","y")
        crs(gfd.point)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        
        # write to file
        if(!is.null(layer.point) & !is.null(dsn)){
          writeOGR(obj = gfd.point,dsn = dsn,layer = layer.point,driver = "ESRI Shapefile",overwrite_layer = T)
        }
      }
      
      # Polygon layer representing grid cells
      {
        # make a raster
        nc.elev.raster=raster(nrows=nc.elev.nlat,ncols=nc.elev.nlon,xmn=-180,xmx=180,ymn=-90,ymx=90,vals=NULL)
        
        # swap 2nd dimension in order to write correctly to the raster object
        nc.elev.val[,1:nc.elev.nlat]=nc.elev.val[,nc.elev.nlat:1]
        
        # assign values in raster object
        values(nc.elev.raster) = nc.elev.val[1:(nc.elev.nlon*nc.elev.nlat)]
        #plot(nc.elev.raster)
        
        # make polygon from raster
        gfd.poly = rasterToPolygons(x = nc.elev.raster,na.rm = F)
        
        # sort the polygons in the same way as the points layer
        gfd.poly.xy = coordinates(gfd.poly)
        targetVal = gfd.point@data$lon*1E4 + gfd.point@data$lat
        sortVal   = gfd.poly.xy[,1]*1E4 + gfd.poly.xy[,2]
        iSort     = match(targetVal,sortVal)
        gfd.poly = gfd.poly[iSort,]
        
        gfd.poly@data = gfd.point@data
        
        # write to file
        if(!is.null(layer.poly) & !is.null(dsn)){
          writeOGR(obj = gfd.poly,dsn = dsn,layer = layer.poly,driver = "ESRI Shapefile",overwrite_layer = T)
        }
      }
      # return 0
      return(0)
    }
  }
  
  # Some error handling functions (subbasin polygons might generate errors)
  {
    # Intersecting area error handling function (some HYPE subbasin geometries causes errors)
    IntersectionArea <- function (x,y) {
      out <- tryCatch(gArea(gIntersection(x,y)), error = function(e) NULL)
      return(out)
    }
    
    # Over error handling function (some HYPE subbasin geometries causes errors)
    testOver <- function (x,y) {
      out <- tryCatch(over(x,y), error = function(e) NULL)
      return(out)
    }
    
  }
  
  # find suitable UTM zone and proj4 string for centre coordinate
  {
    utmZone<-function(longitude){
      return(floor((longitude + 180) / 6) + 1)
    }
    
    utmCRS<-function(longitude){
      utmZ = utmZone(longitude)
      return(paste("+proj=utm +zone=",as.character(utmZ)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep=""))
    }
    
  }
  
  # Function to link GFD gridcells to HYPE subbasin polygons
  {
    # Find GFD grid cells overlaying an Area Of Interest
    gfdCellsOverAOI <- function(aoi,gfd.poly,CRSproj=NULL,doPlot=F){
      
      # initiate list array for output
      gfdCellListArray = array(list(NULL),c(length(aoi),1))
      
      # make sure aoi and gfd.poly have same CRS
      if(as.character(proj4string(aoi))!=as.character(proj4string(gfd.poly))){
        aoi=spTransform(aoi,proj4string(gfd.poly))
      }
      
      gfd.coord=coordinates(gfd.poly)
      
      # loop over aoi
      for(j in 1:length(aoi)){
        
        # Extent of aoi
        aoi.ext = extent(aoi[j,])
        
        # GFD grid cells covering the extent
        iExt = which(gfd.coord[,1]>aoi.ext[1]-0.25 & gfd.coord[,1]<aoi.ext[2]+0.25 & 
                       gfd.coord[,2]>aoi.ext[3]-0.25 & gfd.coord[,2]<aoi.ext[4]+0.25)
        
        # aoi polygon(s) over gfd polygons
        gfdOverAoi = testOver(gfd.poly[iExt,],aoi[j,])
        if(!is.null(gfdOverAoi)){
          # find overlaying gfd cells
          iOver = iExt[which(!is.na(gfdOverAoi[,1]))]
        }else{
          iOver = iExt
        }
        
        # also determine nearest GFD gridpoint to the aoi centre coordinate
        if(is.null(CRSproj)){
          aoi.xy=coordinates(aoi[j,])
          CRSprojTemp = utmCRS(aoi.xy[1])
        }else{
          CRSprojTemp = CRSproj
        }
        aoi.proj = spTransform(aoi[j,],CRSprojTemp)
        gfd.proj = spTransform(gfd.poly[iOver,],CRSprojTemp)
        aoi.xy=coordinates(aoi.proj)
        gfd.xy=coordinates(gfd.proj)
        xyDiff=((aoi.xy[1]-gfd.xy[,1])^2 + (aoi.xy[2]-gfd.xy[,2])^2)^0.5
        
        iNearest=iOver[which(xyDiff==min(xyDiff))[1]]
        
        # data frame with overlaying gridcell information
        gfdCellsData = as.data.frame(gfd.poly@data[iOver,])
        
        # intersection area, km2
        gfdCellsData$intarea=0
        
        for(i in 1:nrow(gfdCellsData)){
          testArea = IntersectionArea(aoi.proj,gfd.proj[i,])
          if(!is.null(testArea)){
            gfdCellsData$intarea[i]=testArea
          }else{
            gfdCellsData$intarea[i]=-1
          }
        }
        gfdCellsData$intarea=gfdCellsData$intarea*1E-6
        
        # handle failed intersection area
        iFail = which(gfdCellsData$intarea<0)
        if(length(iFail)>0){
          iOk=which(gfdCellsData$intarea>0)
          if(length(iOk)>0){
            gfdCellsData$intarea[iFail]=mean(gfdCellsData$intarea[iOk])
          }else{
            gfdCellsData$intarea=1
          }
        }
        
        # weight
        gfdCellsData$weight=gfdCellsData$intarea/sum(gfdCellsData$intarea)
        
        gfdCellListArray[[j]]=list("aoi"=aoi[j,],"cellData"=gfdCellsData,"gfd.poly"=gfd.poly[iOver,],"iOver"=iOver,"iNearest"=iNearest,"nearestData"=gfdCellsData[which(iOver==iNearest),])
        
        # plot
        if(doPlot){
          plot(gfd.poly[iOver,],border="red")
          plot(gfd.poly[iNearest,],border="green",add=T)
          plot(aoi[j,],border="blue",add=T)
        }
      }
      # return list array
      return(gfdCellListArray)
    }
  }
  
  # Functions to read the GFD netCDF files
  {
    # Get time axis from a GFD/GCM netCDF file in POSIXct format (used for initialization of HYPE PTQobs data frame)
    getGFDGCMtimeAxis<-function(gfd.file){
      # open nc file
      {
        nc = nc_open(gfd.file)
      }
      
      # nc file dimensions
      {
        ndim= nc$ndims
        dimName=nc$dim[[1]]$name
        for(i in 2:ndim){dimName=c(dimName,nc$dim[[i]]$name)}
        
        # seems like dimension order has changed on the web presence: time, lat, lon
        iLon = which(dimName=="lon")
        iLat = which(dimName=="lat")
        iTime = which(dimName=="time")
        
        nc.nlon=nc$dim[[iLon]]$len
        nc.nlat=nc$dim[[iLat]]$len
        nc.nTime=nc$dim[[iTime]]$len
        
        nc.lon=nc$dim[[iLon]]$vals
        nc.lat=nc$dim[[iLat]]$vals
        
      }
      # POSIX time axis
      {
        # timeZero from file
        tzPos = regexpr(pattern = "-",text = nc$dim$time$units)[1]-4
        timeZero = substr(nc$dim$time$units,tzPos,tzPos+9)
        
        # timeScale from file
        if(substr(nc$dim$time$units,1,5)=="hours"){
          timeScale = 3600
        }else{
          timeScale = 86400
        }
        
        # timeAxis in POSIX
        timeVals = nc$dim[[iTime]]$vals
        timeAxis = as.POSIXct(timeZero,tz = "GMT")+(timeVals)*timeScale
        
        # adjust to zero hours
        timeAxis = as.POSIXct(as.character(timeAxis,format="%Y-%m-%d"),tz="GMT")
      }
      
      # close nc file
      {
        nc_close(nc)
      }
      
      return(timeAxis)
      
    }
    
    # Read GFD/GCM netCDF file: time-series data and/or spatial dimensions, for special area of interest if requested.
    readGFDGCMnetcdf<-function(gfdFile=NULL,           # nc filename
                               gfdCells=NULL,          # vector with gfd cells of interest (default all cells are returned)
                               dataOut=T,              # flag to return time series data data frame
                               aoi=NULL,               # area of interest (if provided, the nearest GFD gridpoints will be returned, set to NULL if all grids are wanted for weighted output)
                               gfd.poly=NULL,          # polygon layer
                               gfd.point=NULL,
                               lldiffmax = (2 * 0.25^2)^0.5+1e-8,
                               dataOffset=0,
                               dataScale=1,
                               ncvarname=NULL){
      # open nc file
      {
        nc = nc_open(gfdFile)
      }
      
      # nc file dimensions
      {
        ndim= nc$ndims
        dimName=nc$dim[[1]]$name
        for(i in 2:ndim){dimName=c(dimName,nc$dim[[i]]$name)}
        
        iLon = which(dimName=="lon")
        iLat = which(dimName=="lat")
        iTime = which(dimName=="time")
        
        nc.nlon=nc$dim[[iLon]]$len
        nc.nlat=nc$dim[[iLat]]$len
        nc.nTime=nc$dim[[iTime]]$len
        
        nc.lon=nc$dim[[iLon]]$vals
        nc.lat=nc$dim[[iLat]]$vals
        
      }
      
      # prepare spatial key, xyijk.nc
      {
        xyijk.nc = mat.or.vec(nr = nc.nlat*nc.nlon,5)
        k=0
        for(i in 1:nc.nlon){
          for(j in 1:nc.nlat){
            xyijk.nc[(i-1)*nc.nlat+j,] = c(nc.lon[i],nc.lat[j],i,j,1e6 + i*1e3 + j)
          }
        }
        xyijk.nc = as.data.frame(xyijk.nc)
        colnames(xyijk.nc)=c("lon.nc","lat.nc","col.nc","row.nc","id.nc")
        xyijk.nc$X=xyijk.nc$lon.nc
        xyijk.nc$Y=xyijk.nc$lat.nc
        coordinates(xyijk.nc)<-c("X","Y")
        proj4string(xyijk.nc)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        
        # add elevation and global ID from global point data set
        if(!is.null(gfd.point)){
          xyijk.nc$elev.gfd = NA
          xyijk.nc$id.gfd    = NA
          xy.points = coordinates(gfd.point)
          for(i in 1:length(xyijk.nc)){
            igfd = which(xy.points[,1]==xyijk.nc$lon.nc[i] & xy.points[,2]==xyijk.nc$lat.nc[i])
            xyijk.nc$elev.gfd[i]=gfd.point@data$elev[igfd]
            xyijk.nc$id.gfd[i]=gfd.point@data$id[igfd]
          }
        }else{
          xyijk.nc$elev.gfd     = NA
          xyijk.nc$id.gfd    = NA
        }
        
        # extract polygon data from the global polygons if available
        if(!is.null(gfd.poly)){
          nc.poly = gfd.poly[match(xyijk.nc$id.gfd,gfd.poly@data$id),]
        # add "gfd" column names 
          colnames(nc.poly@data)<-c("lon.gfd","lat.gfd","col.gfd","row.gfd","id.gfd","elev.gfd")
        # add the "nc" metadata to the polygon metadata
          nc.poly@data$lon.nc = xyijk.nc$lon.nc
          nc.poly@data$lat.nc = xyijk.nc$lat.nc
          nc.poly@data$col.nc = xyijk.nc$col.nc
          nc.poly@data$row.nc = xyijk.nc$row.nc
          nc.poly@data$id.nc = xyijk.nc$id.nc
        }else{
          nc.poly=NULL
        }
        
      }
      
      # select GFD cells by Area of Interest or by input argument, or all (if aoi and input is missing)
      {
        if(is.null(gfdCells) & is.null(aoi)){         # select all
          gfdCells = 1:(nc.nlon*nc.nlat)
          xyijk.aoi = xyijk.nc
          xyijk.aoi@data$lon=xyijk.nc@data$lon.nc
          xyijk.aoi@data$lat=xyijk.nc@data$lat.nc
        }else{
          if(is.null(gfdCells) & !is.null(aoi)){      # by area of interest
            xyijk.aoi = aoi
            xyijk.aoi@data$lon=NA
            xyijk.aoi@data$lat=NA
            xyijk.aoi@data$lon.nc=NA
            xyijk.aoi@data$lat.nc=NA
            xyijk.aoi@data$col.nc=NA
            xyijk.aoi@data$row.nc=NA
            xyijk.aoi@data$id.nc=NA
            xyijk.aoi@data$elev.gfd=NA
            xyijk.aoi@data$id.gfd=NA
            
            xy=coordinates(aoi)
            xyijk.aoi@data$lon=xy[,1]
            xyijk.aoi@data$lat=xy[,2]
            for(i in 1:length(aoi)){
              lldiff = ((xyijk.aoi@data$lon[i]-xyijk.nc@data$lon.nc)^2 + (xyijk.aoi@data$lat[i]-xyijk.nc@data$lat.nc)^2)^0.5
              inear  = which(lldiff==min(lldiff))
              if(lldiff[inear]<=lldiffmax){
                xyijk.aoi@data$col.nc[i]=xyijk.nc@data$col.nc[inear]
                xyijk.aoi@data$row.nc[i]=xyijk.nc@data$col.nc[inear]
                xyijk.aoi@data$id.nc[i]=xyijk.nc@data$id.nc[inear]
                xyijk.aoi@data$lon.nc[i]=xyijk.nc@data$lon.nc[inear]
                xyijk.aoi@data$lat.nc[i]=xyijk.nc@data$lat.nc[inear]
                xyijk.aoi@data$elev.gfd[i]=xyijk.nc@data$elev.gfd[inear]
                xyijk.aoi@data$id.gfd[i]=xyijk.nc@data$id.gfd[inear]
              }
            }
          }else{
            # by input argument gfdCells
            xyijk.aoi = xyijk.nc[gfdCells,]           
            xyijk.aoi@data$lon=xyijk.aoi@data$lon.nc
            xyijk.aoi@data$lat=xyijk.aoi@data$lat.nc
          }
        }
      }
      
      # get time series data (as HYPE obs-data data frame)
      if(dataOut){
        # POSIX time axis
        {
          # timeZero from file
          tzPos = regexpr(pattern = "-",text = nc$dim$time$units)[1]-4
          timeZero = substr(nc$dim$time$units,tzPos,tzPos+9)
          
          # timeScale from file
          if(substr(nc$dim$time$units,1,5)=="hours"){
            timeScale = 3600
          }else{
            timeScale = 86400
          }
          
          # timeAxis in POSIX
          timeVals = nc$dim[[iTime]]$vals
          timeAxis = as.POSIXct(timeZero,tz = "GMT")+(timeVals)*timeScale
          
          # adjust to zero hours
          timeAxis = as.POSIXct(as.character(timeAxis,format="%Y-%m-%d"),tz="GMT")
        }
        
        # intialize data frame to collect the data
        {
          obsData = data.frame("date"=timeAxis)
          
          idData   = which(!is.na(xyijk.aoi@data$id.nc))
          nData   = length(idData)
          
          obsData = cbind(obsData,NA * mat.or.vec(length(timeAxis),nData))
          
          # use local id (id.nc) since it is always available
          colnames(obsData) <- c("date",as.character(xyijk.aoi@data$id.nc[idData]))
          
          attr(obsData,"obsid")=xyijk.aoi@data$id.nc[idData]
        }
        
        # Read data
        {
          # check which variable to read
          if(!is.null(ncvarname)){
            ncvarid = 0
            for(i in 1:length(nc$var)){
              if(nc$var[[i]]$name==ncvarname){
                ncvarid = i
              }
            }
          }else{
            ncvarid=length(nc$var)
          }
          # first read all data from nc file
          alldata = ncvar_get(nc = nc, varid = nc$var[[ncvarid]],start = c(1,1,1),count = c(nc.nlon,nc.nlat,nc.nTime))

          # offset and scale
          alldata = (alldata + dataOffset) * dataScale

          # then reshape into the obs matrix
        for(i in 1:nData){
            # output variable name
            varName=as.character(xyijk.aoi@data$id.nc[idData[i]])

            # write to data frame
            obsData[,varName] = alldata[xyijk.aoi@data$col.nc[idData[i]],xyijk.aoi@data$row.nc[idData[i]],1:nc.nTime]
            }
	}
      }else{
        obsData = NULL  
      }
      
      # close nc file
      {
        nc_close(nc)
      }
      
      # Return list with time series data and/or spatial data
      {
        return(list("obsData"=obsData,"gfdFile"=gfdFile,"aoi"=aoi,"xyijk.nc"=xyijk.nc,"xyijk.aoi"=xyijk.aoi,"nc.poly"=nc.poly))
      }
      
    }
    
    # initialize data frame for merged multi-file outputs
    initializeMergedGFDObs<-function(gfd.files,obsid){
      # loop over GFD/GCM files and collect timeAxis
      print("make time axis")
      for(i in 1:length(gfd.files)){
        newTimeAxis = getGFDGCMtimeAxis(gfd.file = gfd.files[i])
        if(i==1){
          mergedTimeAxis = newTimeAxis
        }else{
          mergedTimeAxis = c(mergedTimeAxis,newTimeAxis)
        }
      }
      # initialize PTQobs data frame
      print("initialize data frame")
      mergedObs = data.frame("date"=mergedTimeAxis)
      if(!is.null(obsid)){
        # obsid is given on input
        mergedObs = cbind(mergedObs,mat.or.vec(length(mergedTimeAxis),length(obsid))*NA)
        colnames(mergedObs)<-c("date",as.character(obsid))
        attr(mergedObs,"obsid")=obsid
      }else{
        # obsid is not given
        return(-1)
      }
      return(mergedObs)
    }
    
    # Read GFD/GCM files
    readGFDGCMfiles<-function(gfd.files=NULL,
                           gfd.point=NULL,
                           gfd.poly=NULL,
                           aoi=NULL,
                           dataOffset = 0,
                           dataScale = 1,
                           gfdLink = NULL, # @Victor gfdLink is the list array with the GFD gridcell weights for each subbasin polygon
                           obsid = NULL,
                           ncvarname=NULL){
      
      # loop over GFD/GCM files
      print("loop over GFD/GCM files")
      for(i in 1:length(gfd.files)){
        
        # print file name
        print(gfd.files[i])

        # read file
        if(is.null(gfdLink) & i==1){
          # read full spatial information for the first file, but only if gfdLink is missing
          newObs=readGFDGCMnetcdf(gfdFile=gfd.files[i],aoi=aoi,gfd.point=gfd.point,gfd.poly=gfd.poly,dataOffset = dataOffset, dataScale = dataScale)
        }else{
          # otherwise, read only data without spatial information
          newObs=readGFDGCMnetcdf(gfdFile=gfd.files[i],aoi=aoi,gfd.point=NULL,gfd.poly=NULL,dataOffset = dataOffset, dataScale = dataScale)
        }
        
        # make weighted data if gfdLink is provided
        {
          if(!is.null(gfdLink)){
            print("make weighted data")
            newObsData = data.frame("date" = newObs$obsData[,1])
            if(is.null(obsid)){
              obsid=1:length(gfdLink)
            }
            for(j in 1:length(gfdLink)){
              newVar = as.character(obsid[j])
              newObsData[,newVar]=NA
              # find columns in newObs matching the gfd cells to be weighted by the id.nc identifier
              iCol = match(gfdLink[[j]]$cellData$id.nc,attr(newObs$obsData,"obsid"))+1
              # weighted sum
              newObsData[,newVar]=newObs$obsData[,iCol[1]]*gfdLink[[j]]$cellData$weight[1]
              if(length(gfdLink[[j]]$cellData$id.nc)>1){
                for(k in 2:length(gfdLink[[j]]$cellData$id.nc)){
                  newObsData[,newVar]=newObsData[,newVar] + newObs$obsData[,iCol[k]]*gfdLink[[j]]$cellData$weight[k]
                }
              }
            }
            # use the obsids' provided in the input
            attr(newObsData,"obsid")=obsid
          }
        }
        
        # merge data from multiple files
        print("merge data")
        if(length(gfd.files)>1){
          if(i==1){
            # initialize output data frame
            {
              print("initialize output data frame")
              if(!is.null(gfdLink)){
                mergedObs = initializeMergedGFDObs(gfd.files = gfd.files, obsid = attr(newObsData,"obsid"))
              }else{
                mergedObs = initializeMergedGFDObs(gfd.files = gfd.files, obsid = attr(newObs$obsData,"obsid"))
              }
            }
            # save first newObs
            {
              firstObs=newObs
            }
          }
          if(!is.null(gfdLink)){
            iTime = match(newObsData$date,mergedObs$date)
            mergedObs[iTime ,2:ncol(mergedObs)] = newObsData[,2:ncol(newObsData)]
          }else{
            iTime = match(newObs$obsData$date,mergedObs$date)
            mergedObs[iTime ,2:ncol(mergedObs)] = newObs$obsData[,2:ncol(newObs$obsData)]
          }
        }else{
          if(!is.null(gfdLink)){
            mergedObs = newObsData
          }else{
            mergedObs = newObs$obsData
          }
          # save first newObs
          {
            firstObs=newObs
          }
        }
      }

      # return
      return(list("firstObs"=firstObs,"mergedObs"=mergedObs))
    }
    
    
  }
  
  # Functions to write HYPE input files (Pobs, Tobs, ..., ForcKey.txt)
  {
    # Make ForcKey.txt for weighted GFD data - input is a gfdLink object and a list of subbasin identifiers (subid)
    makeWeightedForcKey<-function(pobs=T, tobs = T, tmaxobs=T, tminobs=T, 
                                  gfdLink = NULL,subid = NULL, obsid = NULL,
                                  aoi.target = "Obs",eDigits = 1){
      # output folder
      if(!dir.exists(aoi.target)){
        dir.create(path = aoi.target,recursive = T)
      }
      # make ForcKey
      {
        forcKey = data.frame("SUBID"=subid)
        if(!is.null(pobs)){
          forcKey$POBSID    = obsid
        }
        if(!is.null(tobs)){
          forcKey$TOBSID    = obsid
        }
        if(!is.null(tmaxobs)){
          forcKey$TMAXOBSID    = obsid
        }
        if(!is.null(tminobs)){
          forcKey$TMINOBSID    = obsid
        }
        if(!is.null(tobs)|!is.null(tmaxobs)|!is.null(tminobs)){
          forcKey$TOBSELEV = NA
          # make weighted elevations for each subbasin
          for(i in 1:length(gfdLink)){
            forcKey$TOBSELEV[i] = round(sum(gfdLink[[i]]$cellData$elev * gfdLink[[i]]$cellData$weight),digits = eDigits)
          }
        }
      }
      # write ForcKey
      {
        WriteForcKey(x = forcKey,filename = paste(aoi.target,"ForcKey.txt",sep="/"))
      }
      
      return(forcKey)
    }
    
    # -------------------------------------------------------------------------------
    # function to write HYPE model forcing input files to a specific output folder
    # -------------------------------------------------------------------------------
    #  - Pobs.txt, Tobs.txt, TMINobs.txt, and TMAXobs.txt are written with writePTQobs() from the HYPEtools package
    #  - ForcKey.txt is written if gfdLink and subid are provided
    #  - number of digits can be specified for T, P, and elevation separately
    #  - some diagnostic plots are printed if the subbasin polygons are provided
    obsdata2hype<-function(pobs=NULL,tobs=NULL,tminobs=NULL,tmaxobs=NULL,outputfolder="hype"
                           ,pDigits=5,tDigits=6,eDigits = 1 
                           ,gfdLink=NULL,subid=NULL
                           ,sub.poly=NULL,map.subid.column=1,doPlot=T){

      options(bitmapType='cairo')

      # check/creat output folder
      if(!dir.exists(outputfolder)){
        dir.create(path = outputfolder,recursive = T)
      }
      obsid=NULL
      # write obsfiles to output folder
      if(!is.null(pobs)){
        WritePTQobs(x = pobs,filename = paste(outputfolder,"/Pobs.txt",sep=""),digits = pDigits)
        obsid=attr(pobs,"obsid")
        isPobs=T
      }else{
        isPobs=F
      }
      if(!is.null(tobs)){
        WritePTQobs(x = tobs,filename = paste(outputfolder,"/Tobs.txt",sep=""),digits = tDigits)
        obsid=attr(pobs,"obsid")
        isTobs=T
      }else{
        isTobs=F
      }
      if(!is.null(tminobs)){
        WritePTQobs(x = tminobs,filename = paste(outputfolder,"/TMINobs.txt",sep=""),digits = tDigits)
        obsid=attr(pobs,"obsid")
        isTMINobs=T
      }else{
        isTMINobs=F
      }
      if(!is.null(tmaxobs)){
        WritePTQobs(x = tmaxobs,filename = paste(outputfolder,"/TMAXobs.txt",sep=""),digits = tDigits)
        obsid=attr(pobs,"obsid")
        isTMAXobs=T
      }else{
        isTMAXobs=F
      }
      # write ForcKey.txt if gfdLink and subid are provided
      if(!is.null(gfdLink) & !is.null(subid) & (isPobs|isTobs|isTMINobs|isTMAXobs)){
        forcKey = makeWeightedForcKey(pobs=isPobs, tobs = isTobs, tmaxobs=isTMAXobs, tminobs=isTMINobs, 
                                      gfdLink = gfdLink,subid = subid, obsid = obsid,
                                      aoi.target = outputfolder,eDigits = eDigits)
      }
      # print some diagnostic plots if sub.shp is provided
      if(!is.null(sub.poly) & doPlot){

        options(bitmapType='cairo')

        if(isPobs){
          png(filename = paste(outputfolder,"/pr.png",sep=""),width = 1000,height = 1000, type="cairo")
          PlotMapOutput(x = data.frame("SUBID"=sub.poly@data$SUBID,"Pmean"=colMeans(pobs[,2:ncol(pobs)],na.rm = T)*365.24)
                        ,map = sub.poly,plot.scale = F,map.subid.column = map.subid.column,var.name = "Prec (mean, mm/yr)",col.ramp.fun = "ColPrec")
          dev.off()
        }
        if(isTobs){
          png(filename = paste(outputfolder,"/tas.png",sep=""),width = 1000,height = 1000, type="cairo")
          PlotMapOutput(x = data.frame("SUBID"=sub.poly@data$SUBID,"Tmean"=colMeans(tobs[,2:ncol(tobs)],na.rm = T))
                        ,map = sub.poly,plot.scale = F,map.subid.column = map.subid.column,var.name = "T daily mean (mean, C)",col.ramp.fun = "ColTemp")
          dev.off()
        }
        if(isTMINobs){
          png(filename = paste(outputfolder,"/tasmin.png",sep=""),width = 1000,height = 1000, type="cairo")
          PlotMapOutput(x = data.frame("SUBID"=sub.poly@data$SUBID,"Tmin"=colMeans(tminobs[,2:ncol(tminobs)],na.rm = T))
                        ,map = sub.poly,plot.scale = F,map.subid.column = map.subid.column,var.name = "T daily min (mean, C)",col.ramp.fun = "ColTemp")
          dev.off()
        }
        if(isTMAXobs){
          png(filename = paste(outputfolder,"/tasmax.png",sep=""),width = 1000,height = 1000, type="cairo")
          PlotMapOutput(x = data.frame("SUBID"=sub.poly@data$SUBID,"Tmax"=colMeans(tmaxobs[,2:ncol(tmaxobs)],na.rm = T))
                        ,map = sub.poly,plot.scale = F,map.subid.column = map.subid.column,var.name = "T daily max (mean, C)",col.ramp.fun = "ColTemp")
          dev.off()
        }
        if(!is.null(gfdLink) & !is.null(subid) & (isPobs|isTobs|isTMINobs|isTMAXobs)){
          png(filename = paste(outputfolder,"/elev-gfd.png",sep=""),width = 1000,height = 1000, type="cairo")
          PlotMapOutput(x = data.frame("SUBID"=sub.poly@data$SUBID,"elev"=forcKey$TOBSELEV[match(sub.poly@data$SUBID,forcKey$SUBID)])
                        ,map = sub.poly,plot.scale = F,map.subid.column = map.subid.column,var.name = "GFD weighted elevation (m.a.s.l)",col.ramp.fun = "ColNitr")
          dev.off()
        }
      }
      
      return(0)
    }
    

  }

}

  




