
# Additional includes that the sourced R-files uses
library(ncdf4)

## ------------------------------------------------------------------------------
# Based on 'test/netcdf_to_obs_example.R'
netcdf_to_obs_gridLinkPreparation <- function(pathToNetcdfToObs,
                                              workDir, # TMPDIR/netcdf_to_obs
                                              ncRootDir, # Path to netcdf files
                                              ncSubDir, # False-one dir, True-separate dir for each variable
                                              resourceDir, # Path to resources (shapefiles with (gfd) grid points and polygons)
                                              gridElevPath, # Path to netcdf file with elevation
                                              shapeFilePath, # Path to model shapefile
                                              outPath, # Path for output files, gridLink.Rdata etc.
                                              startDate, # yyyy-mm-dd
                                              endDate, # yyyy-mm-dd
                                              currentSystem='tep',
                                              verbose=F,
                                              verboseVerbose=F)
{
    if (verbose) {
        print('netcdf_to_obs_gridLinkPreparation():')
        print(paste0("pathToNetcdfToObs: ",pathToNetcdfToObs))
        print(paste0("workDir: ",workDir))
        print(paste0("ncRootDir: ",ncRootDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("resourceDir: ",resourceDir))
        print(paste0("gridElevPath: ",gridElevPath))
        print(paste0("shapeFilePath: ",shapeFilePath))
        print(paste0("outPath: ",outPath))
        print(paste0("startDate: ",startDate))
        print(paste0("endDate: ",endDate))
        print(paste0("currentSystem: ",currentSystem))
    }

    # Output
    status <- 0

    # Create workDir
    if (! dir.exists(workDir)){
        dir.create(workDir)
    }
    # currentDir <- getwd()
    # print('currentDir:')
    # print(currentDir)

    # Sourced code assumes all references to other source code from that dir
    currentDir <- setwd(pathToNetcdfToObs)

    # Source the utility file
    # if (currentSystem == 'tep'){
    #     fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_hgfd3_utils.R")
    # }else{
    #     #fileToSource <- paste0(currentDir,'/netcdf_to_obs', '/netcdf_to_obs_hgfd3_utils.R')
    fileToSource <- paste0(pathToNetcdfToObs,'/netcdf_to_obs_hgfd3_utils.R')
    # }
    if (! file.exists(fileToSource)){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",fileToSource),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs hgfd3 - file missing: ",fileToSource)) # Change to cmn.log()
        q(save="no", status = 1)
    }
    source(fileToSource)

    # Define input/output data
    if (ncSubDir == TRUE){
        grid_dir_path <- c(paste0(ncRootDir,"/pr"),
                           paste0(ncRootDir,"/tas"),
                           paste0(ncRootDir,"/tasmin"),
                           paste0(ncRootDir,"/tasmax")
                          )
    }else{
        grid_dir_path <- ncRootDir
    }

    resource_dir_path <- resourceDir # NULL => code sets output to output.path
    if (! file.exists(resource_dir_path)){ # dir.exists
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",resource_dir_path),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs - file missing: ",resource_dir_path)) # Change to cmn.log()
        #q(save="no", status = 0)
    }

    grid_elev_path <- gridElevPath
    if (! file.exists(grid_elev_path)){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",grid_elev_path),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs - file missing: ",grid_elev_path)) # Change to cmn.log()
        #q(save="no", status = 0)
    }

    nc_var_name     <- c("pr","tas","tasmin","tasmax")
    hype_obs_type   <- c("Pobs","Tobs","TMINobs","TMAXobs")
    if (currentSystem == 'server'){
        # hgfd3: symlinks or files  ends with '_pr.nc'
        nc_file_pattern <- c("_pr\\.","_tas\\.","_tasmin\\.","_tasmax\\.")
    }else if (currentSystem == 'tep'){
        # hgfd3: symlinks or files ends with _pr_fanfar_SMHI.nc'
        nc_file_pattern <- c("_pr_","_tas_","_tasmin_","_tasmax_")
    }else{
        # hgfd2: starts with 'pr_'
        nc_file_pattern <- c("pr_","tas_","tasmin_","tasmax_")
    }
    print(nc_file_pattern)

    obsScale  <- c(86400,1,1,1)
    obsOffset <- c(0,-273.15,-273.15,-273.15)
    obsDigits <- c(3,1,1,1)

    # Model input
    shape_file_path <- shapeFilePath
    if (! file.exists(shape_file_path)){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",shape_file_path),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs - file missing: ",shape_file_path)) # Change to cmn.log()
        #q(save="no", status = 0)
    }

    # Output options
    out_path <- outPath
    if (! dir.exists(out_path)){
        dir.create(out_path)
    }

    # Processing options
    # force re-generation of gridLink (it will always be generated if [out_path]/gridLink.Rdata is missing)
    redoGridLink = F

    # force re-generation of grid.point.shp and grid.polygon.shp
    redoGridLayers = F

    # prefered projected crs to use for calculating areal weights and distances
    #  - if not given, UTM will be used, with UTM zone adapted for each subbasin
    crsProjProc = NULL

    # Check and clean subbasin polygons for corrupted geometries?
    #  - preferably this should be done in advance and not as part of operational use
    cleanGeometry = F

    # nearest(1) or weighted(0)
    weightedOrNearest=0

    # Time period options (optional)
    timeStart.1=as.POSIXct(startDate,tz = "GMT")
    timeEnd.1=as.POSIXct(endDate,tz = "GMT")

    #print("PATHS:")
    #print(paste0("grid.path: ",grid_dir_path[1]))
    # print(paste0("grid.elev: ",grid_elev_path))
    # print(paste0("grid.meta: ",resource_dir_path))
    # print(paste0("output.path: ",out_path))
    # print(paste0("model.shape: ",shape_file_path))
    #print(paste0(": ",))

    # Change to workDir
    #currentDir <- setwd(workDir)

    # Check/re-generate gridLink
    isGridLink = gridLinkPreparation.hgfd3(
                    #grid.path = grid_dir_path[1]
                    #,grid.pattern = nc_file_pattern[1]
                    #,grid.elev = grid_elev_path
                    #,var.name = nc_var_name[1]
                    grid.meta = resource_dir_path
                    ,output.path = out_path
                    ,redoGridLink = redoGridLink
                    ,model.shape = shape_file_path
                    ,cleanGeometry = cleanGeometry
                    ,crsProj = crsProjProc)
    if (isGridLink > 0){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - gridLinkPreparation(): ",isGridLink),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs - gridLinkPreparation(): ",isGridLink)) # Change to cmn.log()
        #q(save="no", status = 76)
        #status <- status + 1
    }

    # Change back to previous dir where src code resides
    if (! is.null(currentDir)){
        setwd(currentDir)
    }

    return (status)
} # netcdf_to_obs_gridLinkPreparation


netcdf_to_obs_readGridsAndWriteObs <- function(pathToNetcdfToObs,
                                               workDir, # TMPDIR/netcdf_to_obs
                                               ncRootDir, # Path to netcdf files
                                               ncSubDir, # False-one dir, True-separate dir for each variable
                                               gridLinkFile, # paste(out_path,"/gridLink.Rdata" # Existing file or path+name of file to be created
                                               outPath, # Path for output files, calling function to handle publish?
                                               startDate, # yyyy-mm-dd
                                               endDate, # yyyy-mm-dd
                                               currentSystem='tep',
                                               verbose=F,
                                               verboseVerbose=F)
{
    if (verbose) {
        print('netcdf_to_obs_readGridsAndWriteObs():')
        print(paste0("pathToNetcdfToObs: ",pathToNetcdfToObs))
        print(paste0("workDir: ",workDir))
        print(paste0("ncRootDir: ",ncRootDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("gridLinkFile: ",gridLinkFile))
        print(paste0("outPath: ",outPath))
        print(paste0("startDate: ",startDate))
        print(paste0("endDate: ",endDate))
        print(paste0("currentSystem: ",currentSystem))
    }

    # Output
    status <- 0

    # Create workDir
    if (! dir.exists(workDir)){
        dir.create(workDir)
    }
    #currentDir <- getwd()
    #print('currentDir:')
    #print(currentDir)

    # Sourced code assumes all references to other source code from that dir
    currentDir <- setwd(pathToNetcdfToObs)

    # Source the utility file
    # if (currentSystem == 'tep'){
    #     fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_hgfd3_utils.R")
    # }else{
    #     #fileToSource <- paste0(currentDir,'/netcdf_to_obs', '/netcdf_to_obs_hgfd3_utils.R')
    fileToSource <- paste0(pathToNetcdfToObs,'/netcdf_to_obs_hgfd3_utils.R')
    #}
    if (! file.exists(fileToSource)){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",fileToSource),nameOfSrcFile_PN)
        print(paste0("Aborting netcdf to obs hgfd3 - file missing: ",fileToSource)) # Change to cmn.log()
        #q(save="no", status = 1)
    }
    source(fileToSource)

    # Define input/output data
    if (ncSubDir == TRUE){
        grid_dir_path <- c(paste0(ncRootDir,"/pr"),
                           paste0(ncRootDir,"/tas"),
                           paste0(ncRootDir,"/tasmin"),
                           paste0(ncRootDir,"/tasmax")
                          )
    }else{
        grid_dir_path <- ncRootDir
    }

    # netcdf variables (can be pr, tas, tasmin and/or tasmax for GFD data)
    nc_var_name <- c("pr","tas","tasmin","tasmax")

    # hype obs type (pobs, tobs, etc)
    hype_obs_type   <- c("Pobs","Tobs","TMINobs","TMAXobs")

    # file patterns (for instance "pr_GLB_" or "tas_GLB_")
    if (currentSystem == 'server'){
        # hgfd3: symlinks or files  ends with '_pr.nc'
        nc_file_pattern <- c("_pr\\.","_tas\\.","_tasmin\\.","_tasmax\\.")
    }else if (currentSystem == 'tep'){
        # hgfd3: symlinks or files ends with _pr_fanfar_SMHI.nc'
        nc_file_pattern <- c("_pr_","_tas_","_tasmin_","_tasmax_")
    }else{
        # hgfd2
        nc_file_pattern <- c("pr_","tas_","tasmin_","tasmax_")
    }
    print(nc_file_pattern)

    # scale and offsets to be applied on the netcdf data
    obsScale  = c(1,1,1,1)
    obsOffset = c(0,-273.15,-273.15,-273.15)
    
    # number of digits in output
    obsDigits = c(3,1,1,1)

    # Output options
    out_path <- outPath
    if (! dir.exists(out_path)){
        dir.create(out_path)
    }

    # Processing options
    # force re-generation of gridLink (it will always be generated if [out_path]/gridLink.Rdata is missing)
    redoGridLink = F

    # force re-generation of grid.point.shp and grid.polygon.shp
    redoGridLayers = F

    # prefered projected crs to use for calculating areal weights and distances
    #  - if not given, UTM will be used, with UTM zone adapted for each subbasin
    crsProjProc = NULL

    # Check and clean subbasin polygons for corrupted geometries?
    #  - preferably this should be done in advance and not as part of operational use
    cleanGeometry = F

    # nearest(1) or weighted(0)
    weightedOrNearest=0

    # Time period options (optional)
    timeStart.1=as.POSIXct(startDate,tz = "GMT")
    timeEnd.1=as.POSIXct(endDate,tz = "GMT")

    # Change to workDir
    # currentDir <- setwd(workDir)

    # Read netcdf data from the grid.data folder and generate (new) PT-obs files and ForcKey.txt using a gridLink.Rdata
    # Also possible to read a new time period and merge with existing data (see file mentioned above)
    readWriteResult = readGridsAndWriteObs.hgfd3.forecast( # Assumes nc files in subdirs year/month/<file.nc>
    #readWriteResult = readGridsAndWriteObs.hgfd3(
                           grid.path         = grid_dir_path
                          ,grid.pattern      = nc_file_pattern
                          ,var.name          = nc_var_name
                          ,obs.type          = hype_obs_type
                          ,obs.scale         = obsScale
                          ,obs.offset        = obsOffset
                          ,obs.digits        = obsDigits
                          ,gridLink.path     = gridLinkFile, #paste(out_path,"/gridLink.Rdata",sep="")
                          ,output.path       = out_path
                          ,overwrite         = T
                          ,doForcKey         = T
                          ,elev.digits       = 1
                          ,time.start        = timeStart.1
                          ,time.end          = timeEnd.1
                          ,weightedOrNearest = weightedOrNearest
                          ,saveCopyOfExistingObsFiles=F)
    # readWriteResult = readGridsAndWriteObs.hgfd3(
    #                        grid.path         = grid_dir_path
    #                       ,grid.pattern      = nc_file_pattern
    #                       ,var.name          = nc_var_name
    #                       ,obs.type          = hype_obs_type
    #                       ,obs.scale         = obsScale
    #                       ,obs.offset        = obsOffset
    #                       ,obs.digits        = obsDigits
    #                       ,gridLink.path     = gridLinkFile, #paste(out_path,"/gridLink.Rdata",sep="")
    #                       ,output.path       = out_path
    #                       ,overwrite         = T
    #                       ,doForcKey         = T
    #                       ,elev.digits       = 1
    #                       ,time.start        = timeStart.1
    #                       ,time.end          = timeEnd.1
    #                       ,weightedOrNearest = weightedOrNearest)
    if (readWriteResult > 0){
        #rciop.log("INFO", paste0("Aborting netcdf to obs - readGridsAndWriteObs(): ",readWriteResult),nameOfSrcFile_PN) # Change to cmn.log()
        print(paste0("Aborting netcdf to obs - readGridsAndWriteObs(): ",readWriteResult)) # Change to cmn.log()
        #q(save="no", status = 76)
        #status <- status + 2
    }

    # Change back to previous dir where src code resides
    if (! is.null(currentDir)){
        setwd(currentDir)
    }

    return (status)
} # netcdf_to_obs_readGridsAndWriteObs
## ------------------------------------------------------------------------------
