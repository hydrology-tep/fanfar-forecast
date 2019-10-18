# netcdf_to_obs_utils.R
#
# R script with utility functions to generate HYPE model forcing data based on HydroGFD/GCM NetCDF data
#
# It could also be used to read netcdf files with other data as well, if the file structure is similar to HydroGFD.
#
# Inputs:
#
#   - point and polygon layer shapefiles with lat, long, elevation, row (lat), col (lon), ... (generated if missing)
#   - HYPE model subbasin polygon shapefile (with subbasin identifier SUBID in attribute data)
#   - (HydroGFD) nc-files with meteodata (P, T, TMIN, TMAX): for instance
#
#        daily precipitation        pr_[...].nc
#        daily mean temperature     tas_[...].nc
#        daily minimum temperature  tasmin_[...].nc
#        daily maximum temperature  tasmax_[...].nc
#
#     nb.1: netcdf file naming convention is part of function inputs (see netcdf_to_obs_example.R)
#     nb.2: the temporal coverage of files are investigated, and files are automatically sorted in temporal order.
#     nb.3: a requested time span for the output files can be set, and only files needed for it will be read. 
#
# output data:
#   - Pobs.txt, Tobs.txt, TMINobs.txt, TMAXobs.txt, ForcKey.txt, 
#   - Rdata binary data file with coupling between model subbasins and nc grid cells and weights (gridLink.Rdata)  
#
# usage:
#   - see examples (for interactive use) in netcdf_to_obs_example.R
#
# Last updated 2019-10-08, David Gustafsson, SMHI
#
# Updates (2019-10-08):
#    + new functions for reading/writing in order to manage the large global HydroGFDv2.0 files (v3 will be even larger)
#    + supposedly, it should now work with any set of netcdf files with similar structure as the HydroGFDv2.0
#    + improved speed for weighted output thanks to several optimization measures in the reading/writing and weighting loops.
#    + reading/writing is now very fast: ArcticHYPE, 33000 subbasins, 4 obs-files, 1961-2015, with weighting in ~2 hours
#    + temporal content of input files is checked, and files are sorted automatically before reading
#    + only files needed for requested output time span are read (however, overlap is not checked)
#
# To-do (2019-10-08):
#    - generation of the gridLink data is still rather slow and it is recommended to re-use existing gridLink.Rdata files
#    - speedup/parallellization of the gridLink generation possible? Necesssary if gridLink is re-generated on a daily basis!
#    - more control of temporal consistency when appending data from multiple files and into existing files?
#    - check functionality with FANFAR netcdf!
#
# ---------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
# Initialisations
# -------------------------------------------------------------------------------------------
{
  # Packages (we need to load certain packages - install if you dont have them. HYPEtools can be found on github)
  {
    #library(HYPEtools) # to work with HYPE data formats
    library(ncdf4)     # to read the GFD netcdf files # add via conda install r-ncdf4
    library(sp)        # spatial data base package
    library(rgdal)     # GDAL functions
    library(rgeos)     # GEOS functions
    library(raster)    # raster functions
    library(maptools)  # maptools (needed to clean geometry errors)
    #library(cleangeo)  # cleangeo (also needed to clean geometry errors) # not found via conda
    library(foreign)   # to write dbf files (workaround annoying limitations in writeOGR)
    library(data.table) # Used by functions from HYPEtools (instead of the complete HYPEtools and its dependencies)
  }

   #### source the utility file (assumed to be located in the working directory)
  {
    fileToSource <- paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/netcdf_to_obs_utils_hypetools.R",sep="/")
    if (! file.exists(fileToSource)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",fileToSource))
        q(save="no", status = 0) # 77
    }
    source(fileToSource)
  }

  # Define CRS for WGS84 lat-long (projected CRS should now be provided by the user (see run example scripts))
  {
    CRSlatlon = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  }
}

# -------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------
{
  # Functions to clean and write HYPE model shapefiles
  {
    # Save HYPE subbasin shapefile without all the annoying warnings from writeOGR
    writeHypeSubbasinShapeFile<-function(x=NULL,dsn=NULL,layer=NULL,areaDigits=0,makeDir=F,updateArea=F,crsProj=NULL,writeProj=F){
      # Check input
      {
        if(is.null(x)|is.null(dsn)|is.null(layer)){
          print("ERROR: NULL in x, dsn, or layer inputs not allowed!")
          return(-1)
        }
        if(!dir.exists(dsn) & !makeDir){
          print("ERROR: dsn folder does not exist, and makeDir=FALSE")
          return(-1)
        }
        if(!dir.exists(dsn) & makeDir){
          dir.create(dsn,recursive = T)
          if(!dir.exists(dsn)){
            print("ERROR: failed to create dsn folder")
            return(-1)
          }
        }
      }
      
      # Step 1: adjust SUBID, HAROID and MAINDOWN to integer
      {
        colNames = colnames(x@data)
        iSubid   = which(tolower(colNames)=="subid")
        if(length(iSubid)>0){
          x@data[,iSubid]=as.integer(x@data[,iSubid])
        }
        iHaroid   = which(tolower(colNames)=="haroid")
        if(length(iHaroid)>0){
          x@data[,iHaroid]=as.integer(x@data[,iHaroid])
        }
        iMain   = which(tolower(colNames)=="maindown")
        if(length(iMain)>0){
          x@data[,iMain]=as.integer(x@data[,iMain])
        }
      }
      
      # Step 2: update area and uparea
      if(updateArea){
        
        # Area
        newArea = NA * x@data$AREA
        for(i in 1:length(x)){
          print(i)
          shp.proj.c = spTransform(x = x[i,],CRSobj = crsProj)
          newArea[i] = round(gArea(shp.proj.c),digits = areaDigits)
        }
        
        # Uparea (neglecting any branches)
        gd=data.frame("SUBID"=x@data$SUBID,"HAROID"=x@data$HAROID,"MAINDOWN"=x@data$MAINDOWN,"AREA"=newArea,"UPAREA"=NA*newArea)
        for(i in 1:nrow(gd)){
          print(i)
          iUp = match(AllUpstreamSubids(subid = gd$SUBID[i],gd = gd),gd$SUBID)
          gd$UPAREA[i] = sum(gd$AREA[iUp])-gd$AREA[i]
        }
        
        x@data$AREA = newArea
        x@data$UPAREA = round(gd$UPAREA,digits = areaDigits)
        
      }
      
      # Step 3: round the area and uparea to 0 digits (assuming m2)
      {
        iArea   = which(tolower(colNames)=="area")
        iUpArea   = which(tolower(colNames)=="uparea")
        x@data[,iArea] = round(x@data[,iArea],digits = areaDigits)
        x@data[,iUpArea] = round(x@data[,iUpArea],digits = areaDigits)
      }
      
      # Step 3: write dbf file to temporary filename
      {
        dbfTmpFile = paste(dsn,"/",layer,"-tmp.dbf",sep="")
      }
      
      # Step 4: write shapefile with areas in km2 to avoid all error messages (project if requested)
      {
        x@data$AREA=x@data$AREA*1e-6
        x@data$UPAREA=x@data$UPAREA*1e-6
        if(writeProj & !is.null(crsProj)){
          x = spTransform(x = x,CRSobj = crsProj)
        }
        writeOGR(obj = x,dsn = dsn,layer = layer,driver = "ESRI Shapefile")
      }
      
      # Step 5: copy temporary dbf with areas in m2 to the layer file name
      {
        dbfFile = paste(dsn,"/",layer,".dbf",sep="")
        file.copy(from = dbfTmpFile,to = dbfFile,overwrite = T)
        file.remove(dbfTmpFile)  
      }
      
      # step 6: Adjust areas back to m2 and return updated spatial layer object
      {
        x@data$AREA=x@data$AREA*1e6
        x@data$UPAREA=x@data$UPAREA*1e6
        return(x)
      }
    }
    
    # Function to clean typical HYPE subbasin shapefile gemoetry errors
    cleanHYPEGeometryErrors<-function(x,verbose=F,reportList=F,crsProj=NULL,areaChangeMax=0.001,usePolygonation=F){
      
      # initialize cleaning report counters
      ngeo = length(x)
      nclean   = 0
      ncleaned = 0
      nfailed  = 0
      iclean = NA
      icleaned = NA
      ifailed = NA
      
      # loop over geometries
      for(i in 1:ngeo){
        # check geometry validity
        if(!clgeo_IsValid(x[i,])){
          
          if(verbose){
            print(c(i,ngeo))
          }
          
          # extract the erroneous feature
          newSPDF = x[i,]
          
          # error type
          error.report = clgeo_GeometryReport(newSPDF)
          
          # first check for orphaned holes (see https://r.789695.n4.nabble.com/SpatialPolygonsDataFrame-holes-problem-td3655810.html)
          if(!is.na(error.report$issue_type)){
            if(error.report$issue_type=="ORPHANED_HOLE"){
              newSPDF = testCheckHoles(newSPDF)
              if(is.null(newSPDF)){
                newSPDF = x[i,]
              }
            }
          }
          
          # second, check for linearRing error >> go direct to advanced cleaning strategy
          if(!is.na(error.report$error_msg)){
            if(error.report$error_msg=="rgeos_crdMat2LinearRing: linearRing not created"){
              newSPDF = clgeo_Clean(newSPDF)
            }
          }
          
          
          # third, clean using the BUFFER method and check difference in area
          simplifyAreaError=F
          if(!clgeo_IsValid(newSPDF)){
            
            newSPDF2 = clgeo_Clean(newSPDF,strategy="BUFFER")
            
            if(!is.null(newSPDF2)){
              if(is.null(crsProj)){
                xy=coordinates(newSPDF)
                crsProjtmp = utmCRS(xy[1])
              }else{
                crsProjtmp = crsProj
              }
              
              newSPDF2.proj  = spTransform(newSPDF2,CRSobj = crsProjtmp)
              newSPDF.proj   = spTransform(newSPDF,CRSobj = crsProjtmp)
              
              area1 = gArea(newSPDF.proj)
              area2 = gArea(newSPDF2.proj)  
              
              if(abs(1-area2/area1) <= areaChangeMax){
                newSPDF = newSPDF2
              }else{
                simplifyAreaError=T
              }
            }else{
              simplifyAreaError=T
            }
          }
          
          # thirdly, if geometry is still not valid, try cleaning
          if((!clgeo_IsValid(newSPDF)|simplifyAreaError) & usePolygonation){
            newSPDF = clgeo_Clean(newSPDF)
          }
          
          # finally, merge with the data
          if(clgeo_IsValid(newSPDF)){
            # merge the adjusted geometry with the data
            if(i>1){
              newSPDF   = rbind(x[1:(i-1),],newSPDF,makeUniqueIDs = TRUE)
            }
            if(i<ngeo){
              x = rbind(newSPDF,x[(i+1):ngeo,],makeUniqueIDs = TRUE) 
            }
            if(verbose){print("SUCCESS")}
            ncleaned = ncleaned + 1
            if(ncleaned==1){
              icleaned = i
            }else{
              icleaned = c(icleaned,i)
            }
          }else{
            if(verbose){print("---- FAILURE --------------")}
            nfailed = nfailed + 1
            if(nfailed==1){
              ifailed = i
            }else{
              ifailed = c(ifailed,i)
            }
          }
        }else{
          nclean = nclean + 1
          if(nclean==1){
            iclean=i
          }else{
            iclean=c(iclean,i)
          }
        }
      }
      if(verbose & nfailed>0){
        print("Number of FAILED geometry repair > 0  !")
      }
      
      if(reportList){
        return(list("spdf"=x,"ngeo"=ngeo,"nclean"=nclean,"iclean"=iclean,"ncleaned"=ncleaned,"icleaned"=icleaned,"nfailed"=nfailed,"ifailed"=ifailed))
      }else{
        return(x)
      }
    }
    
  }
  
  # Generate polygons and point layers for GFD grids
  {
    # Make spatial layers for the GFD gridpoints (point and polygon)
    makeGFDSpatialLayers <- function(grid.nc=NULL,dsn=NULL,layer.point=NULL,layer.poly=NULL,xName="lon",yName="lat",zName="elevation"){
      # read netcdf file with the grid dimensions and (optionally) 
      {
        if(!is.null(grid.nc)){
          if(!file.exists(grid.nc)){
            print("ERROR: grid.nc input is NULL (or file missing) - unable to generate point and polygon layer without input geometry file!")
            print("SOLUTION: >>provide netcdf file with same geometry as the data you want to process")
            print("          >>elevation data is optional - if missing it will be set to 0")
            print("          >>x, y dimension names should be provided on input (xName,yName)")
            print("          >>elevation variable name should be provided on input (zName) - if not existing in the grid.nc")
            return(-1)
          }
          # open nc file with grid dimension and (optionally) elevation
          nc.elev = nc_open(grid.nc)
          
          # check dimension names
          nc.dim.names = names(nc.elev$dim)
          ilon = which(substr(nc.dim.names,1,3)==xName)
          ilat = which(substr(nc.dim.names,1,3)==yName)
          
          # lon = dim[1], lat = dim[2]
          nc.elev.nlon=nc.elev$dim[[ilon]]$len
          nc.elev.nlat=nc.elev$dim[[ilat]]$len
          
          # lon and lat values
          nc.elev.lon=round(nc.elev$dim[[ilon]]$vals, 4)
          nc.elev.lat=round(nc.elev$dim[[ilat]]$vals, 4)
          
          # elevation values
          nc.var.names = names(nc.elev$var)
          ielev = which(substr(nc.var.names,1,nchar(zName))==zName)
          if(length(ielev)>0){
            nc.var.name = nc.var.names[ielev[1]]
          }else{
            nc.var.name = NULL
          }
          if(!is.null(nc.var.name)){
            nc.elev.var = nc.elev$var[[nc.var.name]]
            nc.elev.val = ncvar_get(nc = nc.elev,varid = nc.elev.var,start = c(1,1,1), count=c(nc.elev.nlon,nc.elev.nlat,1))
          }else{
            nc.elev.var = NULL
            nc.elev.val = NULL
          }
        }else{
          print("ERROR: grid.nc input is NULL (or file missing) - unable to generate point and polygon layer without input geometry file!")
          print("SOLUTION: >>provide netcdf file with same geometry as the data you want to process")
          print("          >>elevation data is optional - if missing it will be set to 0")
          print("          >>x, y dimension names should be provided on input (xName,yName)")
          print("          >>elevation variable name should be provided on input (zName) - if not existing in the grid.nc")
          return(-1)
        }
      }
      
      # Point layer at GFD grid centre coordinates
      {
        # matrices with data on same form as CRU_elevation matrix
        gfd.lon   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.lat   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.row   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.col   = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        gfd.id    = mat.or.vec(nr = nc.elev.nlon,nc = nc.elev.nlat)
        if(is.null(nc.elev.val)){
          gfd.elev = 0 * gfd.lon
        }else{
          gfd.elev  = nc.elev.val
        }
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
        crs(gfd.point)=CRSlatlon
        
        # write to file
        if(!is.null(layer.point) & !is.null(dsn)){
          writeOGR(obj = gfd.point,dsn = dsn,layer = layer.point,driver = "ESRI Shapefile",overwrite_layer = T)
        }
      }
      
      # Polygon layer representing grid cells
      {
        
        # 1. Determine raster extent and resolution from the incoming netcdf:
        xresolution = (max(nc.elev.lon) - min(nc.elev.lon)) / (nc.elev.nlon-1)
        yresolution = (max(nc.elev.lat) - min(nc.elev.lat)) / (nc.elev.nlat-1)
        
        
        xmin = min(nc.elev.lon) - 0.5*xresolution
        xmax = max(nc.elev.lon) + 0.5*xresolution
        
        ymin = min(nc.elev.lat) - 0.5*yresolution
        ymax = max(nc.elev.lat) + 0.5*yresolution
        
        # 2. make raster
        nc.elev.raster=raster(nrows=nc.elev.nlat,ncols=nc.elev.nlon,xmn=xmin,xmx=xmax, ymn=ymin,ymx=ymax,vals=NULL)
        crs(nc.elev.raster) <- CRSlatlon
        
        # swap 2nd dimension in order to write correctly to the raster object
        nc.elev.val[,1:nc.elev.nlat]=nc.elev.val[,nc.elev.nlat:1]
        
        # assign values in raster object
        values(nc.elev.raster) = nc.elev.val[1:(nc.elev.nlon*nc.elev.nlat)]

        # make polygon from raster
        gfd.poly = rasterToPolygons(x = nc.elev.raster,na.rm = F)

        # sort the polygons in the same way as the points layer
        gfd.poly.xy = coordinates(gfd.poly)

        targetVal = round(gfd.point@data$lon*1E4, 0)*1E6 + round(gfd.point@data$lat*1E4, 0)
        sortVal   = round(gfd.poly.xy[,1]*1E4, 0)*1E6 + round(gfd.poly.xy[,2]*1E4, 0)
        iSort     = match(targetVal,sortVal)

        gfd.poly = gfd.poly[iSort,]
        
        gfd.poly@data = gfd.point@data
        
        # write to file
        if(!is.null(layer.poly) & !is.null(dsn)){
          writeOGR(obj = gfd.poly,dsn = dsn,layer = layer.poly,driver = "ESRI Shapefile",overwrite_layer = T)
        }
      }
      return(0)
    }
  }
  
  # Some error handling functions (subbasin polygons might generate errors)
  {
    # Intersecting area error handling function (some HYPE subbasin geometries causes errors)
    #  >> improvements to better handle some topology error with overlapping features 
    IntersectionArea <- function (x,y) {
      # first the intersection
      out <- tryCatch(gArea(gIntersection(x,y,byid=TRUE),byid=TRUE), error = function(e) NULL)
      return(out)
    }
    
    # Over/Simplify error handling function (some HYPE subbasin geometries causes errors)
    testOver <- function (x,y) {
      out <- tryCatch(over(x,y), error = function(e) NULL)
      return(out)
    }
    testSimplify <- function (x,tolval,topoPres=FALSE) {
      out <- tryCatch(gSimplify(x,tol = tolval,topologyPreserve = topoPres), error = function(e) NULL)
      return(out)
    }
    testCheckHoles <- function (x) {
      y=x
      out <- tryCatch(slot(y, "polygons") <- lapply(slot(y, "polygons"), checkPolygonsHoles), error = function(e) NULL)
      if(!is.null(out)){
        slot(x, "polygons") <- lapply(slot(x, "polygons"), checkPolygonsHoles)
        out=x
      }
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
  
  # Functions to link GFD gridcells to HYPE subbasin polygons
  {
    # Find GFD grid cells overlaying an Area Of Interest
    gfdCellsOverAOI <- function(CRU_elevation.nc, aoi, gfd.poly, CRSproj=NULL,doPlot=F){
      
      # open nc file
      nc.elev = nc_open(CRU_elevation.nc)
      
      # check dimension names
      nc.dim.names = names(nc.elev$dim)
      ilon = which(substr(nc.dim.names,1,3)=="lon")
      ilat = which(substr(nc.dim.names,1,3)=="lat")
      
      # lon = dim[1], lat = dim[2]
      nc.elev.nlon=nc.elev$dim[[ilon]]$len
      nc.elev.nlat=nc.elev$dim[[ilat]]$len
      
      # lon and lat values
      nc.elev.lon=round(nc.elev$dim[[ilon]]$vals, 4)
      nc.elev.lat=round(nc.elev$dim[[ilat]]$vals, 4)
      
      # 1. Determine raster extent and resolution from the incoming netcdf:
      xresolution = (max(nc.elev.lon) - min(nc.elev.lon)) / (nc.elev.nlon-1)
      yresolution = (max(nc.elev.lat) - min(nc.elev.lat)) / (nc.elev.nlat-1)
      
      xmin = min(nc.elev.lon) - 0.5*xresolution
      xmax = max(nc.elev.lon) + 0.5*xresolution
      
      ymin = min(nc.elev.lat) - 0.5*yresolution
      ymax = max(nc.elev.lat) + 0.5*yresolution
      
      # initiate list array for output
      gfdCellListArray = array(list(NULL),c(length(aoi),1))
      
      # make sure aoi and gfd.poly have same CRS
      if(as.character(proj4string(aoi))!=as.character(proj4string(gfd.poly))){
        aoi=spTransform(aoi,proj4string(gfd.poly))
      }
      gfd.coord=coordinates(gfd.poly)
      
      # loop over aoi
      print("----------------------------------------------------------------")
      print("loop over sub-basins and generate netcdf grid links and weights:")
      print("----------------------------------------------------------------")
      for(j in 1:length(aoi)){
        print(j)
        
        # Extent of aoi
        aoi.ext = extent(aoi[j,])
        
        # GFD grid cells covering the extent
        iExt = which(gfd.coord[,1]>aoi.ext[1]-0.5*xresolution & gfd.coord[,1]<aoi.ext[2]+0.5*xresolution & 
                       gfd.coord[,2]>aoi.ext[3]-0.5*yresolution & gfd.coord[,2]<aoi.ext[4]+0.5*yresolution)
        
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
          # print(i)
          testArea = IntersectionArea(aoi.proj,gfd.proj[i,])
          if(!is.null(testArea)){
            gfdCellsData$intarea[i]=testArea
          }else{
            gfdCellsData$intarea[i]=-1
          }
        }
        gfdCellsData$intarea=gfdCellsData$intarea*1E-6
        
        # if the subid only exist in out grid area set its area to 1
        # this is needed for points which has 0 area
        if(length(gfdCellsData$intarea)==1){
          gfdCellsData$intarea = 1
        }
        
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
    
    # Main wrapper function to prepare the "gridLink.Rdata" file
    gridLinkPreparation<-function(grid.path,grid.pattern,grid.elev,var.name,
                                  grid.meta=NULL,output.path="./",redoGridLink=F,model.shape=NULL,
                                  cleanGeometry=F,crsProj=NULL,areaChangeMax = 0.005){
      
      # Parse the output folder input
      {
        if(output.path!="."){
          output.folder = output.path
          if(!dir.exists(output.folder)){
            dir.create(output.folder,recursive = T)
          }
        }else{
          output.folder = getwd()
        }
      }
      
      # Parse the grid data inputs
      {
        if(!dir.exists(grid.path)){
          print("Path to netcdf grids does not exist")
          return(1)
        }
        
        if(is.null(grid.meta)){
          grid.meta = output.folder
        }
        
        if(!dir.exists(grid.meta)){
          dir.create(grid.meta)
        }
        grid.shapes.dsn = grid.meta
      }
      
      # Generate gridLink if missing 
      {
        gridLink.file = paste(output.folder,"gridLink.Rdata",sep="/")
        if(!file.exists(gridLink.file)|redoGridLink){
          # a. Read model shapefile
          {
            # check existence of model shapefile
            {
              if(is.null(model.shape)){
                print("model shapefile has NULL input")
                return(1)
              }
              if(!file.exists(model.shape)){
                print("model shapefile is missingt")
                return(1)
              }
            }
            
            # parse the shapefile arguments
            {
              model.shape.dsn    <- dirname(model.shape)
              model.shape.layer  <- basename(model.shape)
              model.shape.layer  <- substr(model.shape.layer, 1, nchar(model.shape.layer)-4) 
            }
            
            # read model shapefile
            {
              subbasin.shp = readOGR(dsn = model.shape.dsn,layer = model.shape.layer,GDAL1_integer64_policy=T,stringsAsFactors = F)
            }
            
            # transform to WGS84 longlat for safety (this need to be improved)
            {
              subbasin.shp <- spTransform(subbasin.shp,crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))          
            }
            
            # check and clean shapefile geometry (if requested)
            if(cleanGeometry){
              print("- - - - - - - - - - - - - - - - - - - - - - - ")
              print("      Check/clean model subbasin geometry     ")
              print("- - - - - - - - - - - - - - - - - - - - - - - ")
              clean.subbasin.shp = cleanHYPEGeometryErrors(x = subbasin.shp,reportList = T,verbose=T,areaChangeMax = areaChangeMax,crsProj = crsProj,usePolygonation = F)

              # subbasin spatial polygon data fram from list output from cleaning function
              subbasin.shp = clean.subbasin.shp$spdf
              
              ### export cleaned shapefile with special function to avoid warning messages from GDAL driver
              dummy = writeHypeSubbasinShapeFile(x=subbasin.shp,dsn=output.folder,layer=paste(model.shape.layer,"-cleaned",sep=""),areaDigits=0,updateArea=T)
            }
          }
          
          # b. Generate and/or Read netcdf point and polygon layers
          {
            if(!file.exists(paste(grid.shapes.dsn,"grid.point.shp",sep="/"))|
               !file.exists(paste(grid.shapes.dsn,"grid.polygon.shp",sep="/"))){
              
              if(file.exists(grid.elev)){
                # generate grid layer shapefiles
                makeGFDSpatialLayers(grid.nc = grid.elev
                                     ,dsn = grid.shapes.dsn
                                     ,layer.point = "grid.point"
                                     ,layer.poly = "grid.polygon")
                # read files
                grid.point = readOGR(dsn = grid.shapes.dsn,layer = "grid.point")
                grid.poly  = readOGR(dsn = grid.shapes.dsn,layer = "grid.polygon")
              }else{
                print("ERROR grid.point.shp and/or grid.polygon.shp is missing AND grid.elev is also missing!")
                return(1)
              }
            }else{
              # read files
              grid.point = readOGR(dsn = grid.shapes.dsn,layer = "grid.point")
              grid.poly  = readOGR(dsn = grid.shapes.dsn,layer = "grid.polygon")
            }
          }
          
          # c. Read geometry of one NC-file to get spatial coverage 
          #    of the netcdf grid domain (may be smaller than the global files for which 
          #    we read the elevation above)
          {
            # list NC files in grid file path
            grid_file_path = dir(path = grid.path,pattern = grid.pattern)
            
            if(length(grid_file_path)==0){
              print ("No valid files found in grid.path")
              return(1)
            }
            # read file
            data = readGFDGCMnetcdf(gfdFile = paste(grid.path,"/",grid_file_path[1],sep="")
                             ,dataOut = F
                             ,aoi = NULL
                             ,gfd.poly = grid.poly
                             ,gfd.point = grid.point
                             ,dataOffset = 0
                             ,dataScale = 1
                             ,ncvarname = var.name)
            
#            data = readGFDGCMfiles(gfd.files = paste(grid.path,"/",grid_file_path[1],sep="")
#                                   ,gfd.point = grid.point
#                                   ,gfd.poly = grid.poly
#                                   ,dataOffset = 0
#                                   ,dataScale = 1,
#                                   ncvarname=var.name,
#                                   skipData=T)
          }
          
          # d. generate new gfdLink list array
          {
            gridLink = gfdCellsOverAOI(
              CRU_elevation.nc = grid.elev
              ,aoi = subbasin.shp
              ,gfd.poly = data$nc.poly
              ,CRSproj = crsProj
              ,doPlot = F)
          }
          
          # f. save to file
          {
            save(list = "gridLink",file = gridLink.file)
          }
          return(0)
        }else{
          return(0)
        }
      }
    }
  }
  
  # Function to read GFD (and similar) netCDF files (old, but still used to generate gridLink)
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
        } else if(substr(nc$dim$time$units,1,7) == "seconds") {
          timeScale = 1
        } else{
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
    #
    # this function is not used anymore to read the actual time series data
    #
    # however, it is still used when generating the gridLink!!!
    #
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
        
        nc.lon=round(nc$dim[[iLon]]$vals, 4)
        nc.lat=round(nc$dim[[iLat]]$vals, 4)
        
        # make a raster to extract cell numbers for faster netcdf readout
        xres=abs(nc.lon[2]-nc.lon[1])
        yres=abs(nc.lat[2]-nc.lat[1])
        nc.raster = raster(nrows=nc.nlat,ncols=nc.nlon,xmn=min(nc.lon)-0.5*xres,xmx=max(nc.lon)+0.5*xres,ymn=min(nc.lat)+0.5*yres,ymx=max(nc.lat)+0.5*yres)
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
        proj4string(xyijk.nc)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        
        # cellnumber for faster extraction from netcdf files
        xyijk.nc@data$cell.nc = cellFromRowCol(object = nc.raster,row = xyijk.nc@data$row.nc,col = xyijk.nc@data$col.nc)
        
        # add elevation and global ID from global point data set
        if(!is.null(gfd.point)){
          xyijk.nc$elev.gfd = NA
          xyijk.nc$id.gfd    = NA
          xy.points = coordinates(gfd.point)
          # added
          xy.points[,1] = round(xy.points[,1], 4)
          xy.points[,2] = round(xy.points[,2] ,4)

          # the for-loop is too slow for large nc-domains ...
          # make a new compiled variable for the matching
          # make lat from 0 to 180 and lon from 0 to 360
          ncxy = round((xyijk.nc$lon.nc + 180)*1e4) + round((xyijk.nc$lat.nc + 90)*1e4)*1e6
          ptxy = round((xy.points[,1] + 180)*1e4) + round((xy.points[,2] + 90)*1e4)*1e6
          iMatch = match(ncxy,ptxy)
          xyijk.nc$elev.gfd=gfd.point@data$elev[iMatch]
          xyijk.nc$id.gfd=gfd.point@data$id[iMatch]
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
          nc.poly@data$cell.nc = xyijk.nc$cell.nc
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
                xyijk.aoi@data$cell.nc[i]=xyijk.nc@data$cell.nc[inear]
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
          } else if(substr(nc$dim$time$units,1,7) == "seconds") {
            timeScale = 1
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
  }
  
  # Functions to write HYPE input files (Pobs, Tobs, ..., ForcKey.txt)
  {
    ##
    ## WritePTQobs with append option
    ##
    WritePTQobsAppend <- function (x, filename, dt.format = "%Y-%m-%d", nsmall=1, digits = 3,appendToExisting=F) {
      ## header
      header <- c("DATE", attr(x, which = "obsid"))

      # date conversion, conditional on that the date column is a posix class
      if (any(class(x[, 1]) == "POSIXct")) {
        x[, 1] <- format(x[, 1], format = dt.format)
      } else {
        warning("First column in export data frame is not of class 'POSIXct', will be exported unchanged.")
      }
      
      # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
      x[is.na(x)] <- -9999
      
      # export
      if(appendToExisting){
      write.table(x, 
                  file = filename, 
                  quote = FALSE, 
                  sep = "\t", 
                  row.names = FALSE, 
                  col.names = FALSE,
                  append=appendToExisting)
      }else{
        write.table(x, 
                    file = filename, 
                    quote = FALSE, 
                    sep = "\t", 
                    row.names = FALSE, 
                    col.names = header,
                    append=appendToExisting)
      }
    }
    
    # Make ForcKey.txt for weighted GFD data - input is a gridLink object and a list of subbasin identifiers (subid)
    makeWeightedForcKey<-function(pobs=T, tobs = T, tmaxobs=T, tminobs=T, 
                                  gridLink = NULL,subid = NULL, obsid = NULL,
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
          for(i in 1:length(gridLink)){
            forcKey$TOBSELEV[i] = round(sum(gridLink[[i]]$cellData$elev * gridLink[[i]]$cellData$weight),digits = eDigits)
          }
        }
      }
      # write ForcKey
      {
        WriteForcKey(x = forcKey,filename = paste(aoi.target,"ForcKey.txt",sep="/"))
      }
      
      return(forcKey)
    }
  }
  
  # (NEW) functions to read netcdf files from a folder and write HYPE obs files and forckey file to output folder
  {
    ### Read netcdf grid files and write single HYPE PT-obs file
    readGridsAndWriteSingleObsFile<-function(grid.files=NULL,
                                             gridLink = NULL, 
                                             dataOffset = 0,
                                             dataScale = 1,
                                             ncvarname=NULL,
                                             obsid = NULL,
                                             timeStart=NULL,
                                             timeEnd=NULL,
                                             weightedOrNearest=0,
                                             obsFile="./HypeObs.txt",
                                             obsDigits=1,
                                             overwrite=T,
                                             gridcells=NULL,
                                             sub.gridcells=NULL){
      
      # check inputs and return error if critical inputs are missing
      {
        if(is.null(grid.files)|is.null(gridLink)){
          print("grid.files and/or gridLink is NULL on input")
          return(1)
        }
      }
      
      # check order of files, and if needed timeStart and timeEnd, determine which files to read
      {
        print(" ... checking time period in netcdf files - updating read order if needed")
        for(i in 1:length(grid.files)){
          fileTimeAxis = getGFDGCMtimeAxis(gfd.file = grid.files[i])
          if(i==1){
            filesStart  = fileTimeAxis[1]
            filesEnd    = fileTimeAxis[length(fileTimeAxis)]
          }else{
            filesStart  = c(filesStart,fileTimeAxis[1])
            filesEnd    = c(filesEnd,fileTimeAxis[length(fileTimeAxis)])
          }
        }
        
        # update start and end of output
        if(is.null(timeStart)){
          timeStart = min(filesStart)
        }
        if(is.null(timeEnd)){
          timeEnd = max(filesEnd)
        }
        # files needed for requested time period
        iFiles = which(filesStart<=timeEnd & filesEnd>=timeStart)
        
        # sort files in temporal order
        sortedDates = sort(filesStart[iFiles],decreasing = F)
        iFiles = iFiles[match(sortedDates,filesStart[iFiles])]
      }
      
      # gridcell numbers from gridLink (if needed)
      {
        if(weightedOrNearest==0){
          # weighted
          if(is.null(gridcells)){
            print("... prep. gridcells + sub.gridcell list array")
            for(i in 1:length(gridLink)){
              if(i==1){
                gridcells = gridLink[[i]]$cellData$cell.nc
              }else{
                gridcells = c(gridcells,gridLink[[i]]$cellData$cell.nc)
              }
            }
            gridcells = unique(gridcells)
          }
          if(is.null(sub.gridcells)){
            sub.gridcells=array(list(NULL),c(length(gridLink),1))
            for(i in 1:length(gridLink)){
              sub.gridcells[[i]] = list("iData"=match(gridLink[[i]]$cellData$cell.nc,gridcells))
            }
          }
        }else{
          # nearest
          if(is.null(gridcells)){
            for(i in 1:length(gridLink)){
              if(i==1){
                gridcells = gridLink[[i]]$nearestData$cell.nc
              }else{
                gridcells = c(gridcells,gridLink[[i]]$nearestData$cell.nc)
              }
            }
            # here we keep all to speed up the reading - no need to use maxChunks (yet)
          }
        }
      }
      
      # loop over NETCDF files
      print("...reading NETCDF files...")
      for(i in 1:length(iFiles)){
        
        # print file name
        print(grid.files[iFiles[i]])
        
        # open nc file
        {
          nc = nc_open(grid.files[iFiles[i]])
        }
        
        # nc file dimensions
        {
          ndim= nc$ndims
          dimName=nc$dim[[1]]$name
          for(j in 2:ndim){dimName=c(dimName,nc$dim[[j]]$name)}
          
          iLon = which(dimName=="lon")
          iLat = which(dimName=="lat")
          iTim = which(dimName=="time")
          
          nc.nlon=nc$dim[[iLon]]$len
          nc.nlat=nc$dim[[iLat]]$len
          nc.nTime=nc$dim[[iTim]]$len
          
          nc.lon=round(nc$dim[[iLon]]$vals, 4)
          nc.lat=round(nc$dim[[iLat]]$vals, 4)
        }
        
        # check which time steps should be read
        {
          # timeZero from file
          tzPos = regexpr(pattern = "-",text = nc$dim$time$units)[1]-4
          timeZero = substr(nc$dim$time$units,tzPos,tzPos+9)
          
          # timeScale from file
          if(substr(nc$dim$time$units,1,5)=="hours"){
            timeScale = 3600
          } else if(substr(nc$dim$time$units,1,7) == "seconds") {
            timeScale = 1
          } else{
            timeScale = 86400
          }
          
          # timeAxis in POSIX
          timeVals = nc$dim[[iTim]]$vals
          timeAxis = as.POSIXct(timeZero,tz = "GMT")+(timeVals)*timeScale
          
          # adjust to zero hours
          timeAxis = as.POSIXct(as.character(timeAxis,format="%Y-%m-%d"),tz="GMT")
      
          # which timesteps to read for this file
          iTimeRead = which(timeAxis>=timeStart & timeAxis<=timeEnd)
          
        }

        # check which variable to read
        {
          if(!is.null(ncvarname)){
            ncvarid = 0
            for(j in 1:length(nc$var)){
              if(nc$var[[j]]$name==ncvarname){
                ncvarid = j
              }
            }
          }else{
            ncvarid=length(nc$var)
          }
        }
        
        # matrices to temporary store data (to speedup)
        tempData = NA_real_ * mat.or.vec(length(iTimeRead),length(gridcells))
        obsData1 = NA_real_ * mat.or.vec(length(iTimeRead),length(obsid))
        
        # loop over time steps and read the data
        for(j in 1:length(iTimeRead)){
          # read gridcell data for one time step from netcdf file
          alldata = ncvar_get(nc = nc, varid = nc$var[[ncvarid]],start = c(1,1,iTimeRead[j]),count = c(nc.nlon,nc.nlat,1))[gridcells]

          # offset and scale
          alldata = (alldata + dataOffset) * dataScale
        
          # save to temporary matrix
          tempData[j,] = alldata
        }
        
        # weighted or nearest
        if(weightedOrNearest==0){
          print("... weighting data")
          # weighted
          for(k in 1:length(obsid)){
            # index in the data read from nc file
            iData=sub.gridcells[[k]]$iData
            # weights
            iWeights=gridLink[[k]]$cellData$weight
            # weighted sum
            sumData = tempData[,iData[1]]*iWeights[1]
            if(length(iData)>1){
              for(l in 2:length(iData)){
                sumData = sumData + tempData[,iData[l]]*iWeights[l]
              }
            }
            # check for missing data
            iMissing = which(is.na(sumData))
            if(length(iMissing)>0){
              for(m in 1:length(iMissing)){
                kk=iMissing[m]
                inData = tempData[kk,iData]
                isData = which(!is.na(inData))
                if(length(isData)>0){
                  sumData[kk] = sum(inData[isData]*iWeights[isData])/sum(iWeights[isData])
                }
              }
            }
            # update output data
            obsData1[,k] = sumData
          }
          print("... rounding")
          obsData1 = round(obsData1,digits = obsDigits)
        }else{
          # nearest
          print("... rounding")
          obsData1 = round(tempData,digits = obsDigits)
        }
        # data frame for file output (do it here to speedup)
        obsData2 = data.frame("DATE"=timeAxis[iTimeRead])
        obsData = cbind(obsData2,obsData1)
        colnames(obsData) <- c("date",as.character(obsid))
        attr(obsData,"obsid")<-obsid
        # write to file (this is now the main remaining time consumer for large files)
        if(i==1 & overwrite){
          print("... printing to file")
          WritePTQobsAppend(x = obsData,filename = obsFile,dt.format = "%Y-%m-%d",digits = obsDigits,appendToExisting = F)
        }else{
          print("... appending to file")
          WritePTQobsAppend(x = obsData,filename = obsFile,dt.format = "%Y-%m-%d",digits = obsDigits,appendToExisting = T)
        }
        # close nc file
        nc_close(nc)
      }
      # return sucess
      return(0)
    }
          
    ### main wrapper function that will read one or several set of netcdf files into one or several obs-files
    readGridsAndWriteObs<-function(grid.path,grid.pattern,var.name,obs.type,obs.scale=1,obs.offset=0,obs.digits=1,
                                   gridLink.path=NULL,output.path=".",overwrite=T,doForcKey=T,elev.digits=1,
                                   time.start=NULL,time.end=NULL,weightedOrNearest=0,saveCopyOfExistingObsFiles=T,
                                   model.subset=NULL){
      print("NETCDF_TO_OBS:  readGridsAndWriteObs...")

      # parse output path input
      print("... parsing inputs")
      {
        if(output.path!="."){
          if(!dir.exists(output.path)){
            dir.create(output.path,recursive = T)
          }
        }else{
          output.path = getwd()
        }
      }
      
      # Read gridLink
      print("... reading gridLink.Rdata")
      {
        if(!is.null(gridLink.path)){
          if(file.exists(gridLink.path)){
            load(file = gridLink.path)
          }else{
            print("gridLink.Rdata is missing")
            return(1)
          }
        }else{
          print("gridLink.Rdata is NULL on input")
          return(1)
        }
      }
      
      # Check and flag obs.types
      {
        iPobs = which(tolower(obs.type)=="pobs")
        if(length(iPobs)>0){
          isPobs=T
        }else{
          isPobs=F
        }
        iTobs = which(tolower(obs.type)=="tobs")
        if(length(iTobs)>0){
          isTobs=T
        }else{
          isTobs=F
        }
        iTMINobs = which(tolower(obs.type)=="tminobs")
        if(length(iTMINobs)>0){
          isTMINobs=T
        }else{
          isTMINobs=F
        }
        iTMAXobs = which(tolower(obs.type)=="tmaxobs")
        if(length(iTMAXobs)>0){
          isTMAXobs=T
        }else{
          isTMaXobs=F
        }
      }
      
      # subid, obsid taking model.subset into account
      print(" ... preparing subid and obsid")
      {
        subid = 0*(1:length(gridLink))
        for(i in 1:length(gridLink)){
          subid[i] = gridLink[[i]]$aoi$SUBID
        }
        subidMax = max(subid)
        subidAdd = 10^(floor(log10(subidMax))+1)
        obsid = subid+subidAdd
        
        if(!is.null(model.subset)){
          subid = subid[model.subset]
          obsid = obsid[model.subset]
          gridLink = gridLink[model.subset]
        }
      }
      
      # gridcell numbers from gridLink (to speedup weighting for large n:o subbasins)
      {
        if(weightedOrNearest==0){
          # weighted
          print("... preparing gridcells list array to speed-up gridcell WEIGHTING")
          for(i in 1:length(gridLink)){
            if(i==1){
              gridcells = gridLink[[i]]$cellData$cell.nc
            }else{
              gridcells = c(gridcells,gridLink[[i]]$cellData$cell.nc)
            }
          }
          gridcells = unique(gridcells)
          sub.gridcells=array(list(NULL),c(length(gridLink),1))
          for(i in 1:length(gridLink)){
            sub.gridcells[[i]] = list("iData"=match(gridLink[[i]]$cellData$cell.nc,gridcells))
          }
        }else{
          # nearest
          print("... preparing gridcells to speed-up reading NEAREST gridcells")
          for(i in 1:length(gridLink)){
            if(i==1){
              gridcells = gridLink[[i]]$nearestData$cell.nc
            }else{
              gridcells = c(gridcells,gridLink[[i]]$nearestData$cell.nc)
            }
          }
          # here we keep all to speed up the reading - no need to use maxChunks (yet)
        }
      }
      
      # Make and Write ForcKey
      if(doForcKey){
        forcKeyFile = paste(output.path,"/","ForcKey.txt",sep="")
        if(overwrite | !file.exists(forcKeyFile)){
          if(!is.null(gridLink) & !is.null(subid) & (isPobs|isTobs|isTMINobs|isTMAXobs)){
            print("... writing ForcKey.txt")
            forcKey = makeWeightedForcKey(pobs=isPobs, tobs = isTobs, tmaxobs=isTMAXobs, tminobs=isTMINobs, 
                                          gridLink = gridLink,subid = subid, obsid = obsid,
                                          aoi.target = output.path,eDigits = elev.digits)
          }
        }
      }
      
      # Read Netcdf and write *obs.txt
      print("... START reading/writing NETCDF to OBS...")
      {
        # make sure grid.path has same length as grid.pattern
        {
          if(length(grid.path)<length(grid.pattern)){
            grid.path.vector=grid.pattern
            for(i in 1:length(grid.pattern)){
              grid.path.vector[i]=grid.path[1]
            }
          }else{
            grid.path.vector=grid.path
          }
        }
        
        # make sure obs.scale,obs.offset, and obs.digits have same length as grid pattern
        {
          if(length(obs.scale)<length(grid.pattern)){
            obs.scale=obs.scale[1]*(1 + 0*(1:length(grid.pattern))) 
          }
          if(length(obs.offset)<length(grid.pattern)){
            obs.offset=obs.offset[1] + (0*(1:length(grid.pattern))) 
          }
          if(length(obs.digits)<length(grid.pattern)){
            obs.digits=obs.digits[1]*(1 + 0*(1:length(grid.pattern))) 
          }
        }
        
        # loop over grid.pattern (obs.type)
        for(i in 1:length(grid.pattern)){
          
          # list files
          gridFiles = dir(path = grid.path.vector[i],pattern = grid.pattern[i])

          # read grid files and export as obsfile directly with new function
          if(length(gridFiles)>0){
            # save copy of existing files before overwrite or append
            obsFile = paste(output.path,"/",obs.type[i],".txt",sep="")
            if(saveCopyOfExistingObsFiles & file.exists(obsFile)){
              print("... backup of existing OBS-file")
              obsFileCopy = paste(output.path,"/",obs.type[i],"_saved_",format(Sys.time(), "%Y%m%d_%H%M%S"),".txt",sep="")
              file.rename(from = obsFile,to = obsFileCopy)
              file.copy(from = obsFileCopy,to = obsFile)
            }
            
            # read/write new data
            isObsData = readGridsAndWriteSingleObsFile(
                                         grid.files = paste(grid.path.vector[i],"/",gridFiles,sep="")
                                        ,gridLink = gridLink
                                        ,dataOffset = obs.offset[i]
                                        ,dataScale = obs.scale[i]
                                        ,ncvarname = var.name[i]
                                        ,obsid = obsid
                                        ,timeStart=time.start
                                        ,timeEnd=time.end
                                        ,weightedOrNearest=weightedOrNearest
                                        ,obsFile = paste(output.path,"/",obs.type[i],".txt",sep="")
                                        ,obsDigits = obs.digits[i]
                                        ,overwrite = overwrite
                                        ,gridcells=gridcells
                                        ,sub.gridcells=sub.gridcells)
            if(isObsData!=0){
              print(paste("ERROR writing obsfile: ",paste(output.path,"/",obs.type[i],".txt",sep=""),sep=""))
              return(1)
            }else{
              print(paste("SUCCESS writing obsfile: ",paste(output.path,"/",obs.type[i],".txt",sep=""),sep=""))
            }
          }
        }
      }
      
      # return 0 on success
      return(0)
    }
  }
}






