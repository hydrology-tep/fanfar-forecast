#! /usr/bin/Rscript

# Constants
verbose <- TRUE
#verbose <- FALSE
#verboseX2 <- TRUE
verboseX2 <- FALSE

## ------------------------------------------------------------------------------
# For information on local variables and call sequence within this function,
# see 'test/netcdf_to_obs_example.R'
run_netcdf_to_obs_gridLinkPreparation <- function(workDir, # TMPDIR/netcdf_to_obs
                                                  ncRootDir, # Path to netcdf files
                                                  ncSubDir, # False-one dir, True-separate dir for each variable
                                                  resourceDir, # Path to resources (shapefiles with (gfd) grid points and polygons)
                                                  gridElevPath, # Path to netcdf file with elevation
                                                  shapeFilePath, # Path to model shapefile
                                                  outPath, # Path for output files, calling function to handle publish?
                                                  startDate, # yyyy-mm-dd
                                                  endDate)# yyyy-mm-dd
{
    if (verbose == TRUE) {
        print('run_netcdf_to_obs_gridLinkPreparation():')
        print(paste0("workDir: ",workDir))
        print(paste0("ncRootDir: ",ncRootDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("resourceDir: ",resourceDir))
        print(paste0("gridElevPath: ",gridElevPath))
        print(paste0("shapeFilePath: ",shapeFilePath))
        print(paste0("outPath: ",outPath))
        print(paste0("startDate: ",startDate))
        print(paste0("endDate: ",endDate))
    }

    # Output
    status <- 0

    # Change to workDir
    if (! dir.exists(workDir)){
        dir.create(workDir)
    }
    currentDir <- setwd(workDir)

    # Source the utility file
    fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_utils.R")
    if (! file.exists(fileToSource)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",fileToSource))
        q(save="no", status = 0) # 77
    }
    #ToDo   source(fileToSource)

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
    # if (! file.exists(resource_dir_path)){
    #     rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",resource_dir_path))
    #     q(save="no", status = 0)
    # }
    
    grid_elev_path <- gridElevPath
    if (! file.exists(grid_elev_path)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",grid_elev_path))
        q(save="no", status = 0)
    }

    nc_var_name     <- c("pr","tas","tasmin","tasmax")
    hype_obs_type   <- c("Pobs","Tobs","TMINobs","TMAXobs")
    nc_file_pattern <- c("pr_","tas_","tasmin_","tasmax_")

    obsScale  <- c(86400,1,1,1)
    obsOffset <- c(0,-273.15,-273.15,-273.15)
    obsDigits <- c(3,1,1,1)

    # Model input
    shape_file_path <- shapeFilePath
    if (! file.exists(shape_file_path)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",shape_file_path))
        q(save="no", status = 0)
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

    # Check/re-generate gridLink
    #ToDo
    # isGridLink = gridLinkPreparation(grid.path = grid_dir_path[1]
    #                                 ,grid.pattern = nc_file_pattern[1]
    #                                 ,grid.elev = grid_elev_path
    #                                 ,var.name = nc_var_name[1]
    #                                 ,grid.meta = resource_dir_path
    #                                 ,output.path = out_path
    #                                 ,redoGridLink = redoGridLink
    #                                 ,model.shape = shape_file_path
    #                                 ,cleanGeometry = cleanGeometry
    #                                 ,crsProj = crsProjProc)
    # if (isGridLink > 0){
    #     rciop.log("INFO", paste0("Aborting netcdf to obs - gridLinkPreparation(): ",isGridLink))
    #     #q(save="no", status = 76)
    #     #status <- status + 1
    # }

    # Change back to previous work dir
    if (! is.null(currentDir)){
        setwd(currentDir)
    }

    return (status)
} # run_netcdf_to_obs_gridLinkPreparation


prepare_and_run_netcdf_to_obs <- function(workDir, # TMPDIR/netcdf_to_obs
                                          ncRootDir, # Path to netcdf files
                                          ncSubDir, # False-one dir, True-separate dir for each variable
                                          resourceDir, # Path to resources (shapefiles with (gfd) grid points and polygons)
                                          gridElevPath, # Path to netcdf file with elevation
                                          shapeFilePath, # Path to model shapefile
                                          outPath, # Path for output files, calling function to handle publish?
                                          startDate, # yyyy-mm-dd
                                          endDate)# yyyy-mm-dd
{
    if (verbose == TRUE) {
        print('prepare_and_run_netcdf_to_obs():')
        print(paste0("workDir: ",workDir))
        print(paste0("ncRootDir: ",ncRootDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("resourceDir: ",resourceDir))
        print(paste0("gridElevPath: ",gridElevPath))
        print(paste0("shapeFilePath: ",shapeFilePath))
        print(paste0("outPath: ",outPath))
        print(paste0("startDate: ",startDate))
        print(paste0("endDate: ",endDate))
    }

    # Output
    status <- 0

    # Change to workDir
    if (! dir.exists(workDir)){
        dir.create(workDir)
    }
    currentDir <- setwd(workDir)

    # Source the utility file
    fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_utils.R")
    if (! file.exists(fileToSource)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",fileToSource))
        q(save="no", status = 77)
    }
    #ToDo   source(fileToSource)

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

    resource_dir_path <- resourceDir
    if (! file.exists(resource_dir_path)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",resource_dir_path))
        q(save="no", status = 77)
    }
    
    grid_elev_path <- gridElevPath
    if (! file.exists(grid_elev_path)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",grid_elev_path))
        q(save="no", status = 77)
    }

    nc_var_name     <- c("pr","tas","tasmin","tasmax")
    hype_obs_type   <- c("Pobs","Tobs","TMINobs","TMAXobs")
    nc_file_pattern <- c("pr_","tas_","tasmin_","tasmax_")

    obsScale  <- c(86400,1,1,1)
    obsOffset <- c(0,-273.15,-273.15,-273.15)
    obsDigits <- c(3,1,1,1)

    # Model input
    shape_file_path <- shapeFilePath
    if (! file.exists(shape_file_path)){
        rciop.log("INFO", paste0("Aborting netcdf to obs - file missing: ",shape_file_path))
        q(save="no", status = 77)
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

    # # Check/re-generate gridLink
    # isGridLink = gridLinkPreparation(grid.path = grid_dir_path[1]
    #                                 ,grid.pattern = nc_file_pattern[1]
    #                                 ,grid.elev = grid_elev_path
    #                                 ,var.name = nc_var_name[1]
    #                                 ,grid.meta = resource_dir_path
    #                                 ,output.path = out_path
    #                                 ,redoGridLink = redoGridLink
    #                                 ,model.shape = shape_file_path
    #                                 ,cleanGeometry = cleanGeometry
    #                                 ,crsProj = crsProjProc)
    # if (isGridLink > 0){
    #     rciop.log("INFO", paste0("Aborting netcdf to obs - gridLinkPreparation(): ",isGridLink))
    #     #q(save="no", status = 76)
    #     status <- status + 1
    # }

    # Read netcdf data from the grid.data folder and generate (new) PT-obs files and ForcKey.txt using a gridLink.Rdata
    # Also possible to read a new time period and merge with existing data (see file mentioned above)
    # readWriteResult = readGridsAndWriteObs(
    #                        grid.path         = grid_dir_path
    #                       ,grid.pattern      = nc_file_pattern
    #                       ,var.name          = nc_var_name
    #                       ,obs.type          = hype_obs_type
    #                       ,obs.scale         = obsScale
    #                       ,obs.offset        = obsOffset
    #                       ,obs.digits        = obsDigits
    #                       ,gridLink.path     = paste(out_path,"/gridLink.Rdata",sep="")
    #                       ,output.path       = out_path
    #                       ,overwrite         = T
    #                       ,doForcKey         = T
    #                       ,elev.digits       = 1
    #                       ,time.start        = timeStart.1
    #                       ,time.end          = timeEnd.1
    #                       ,weightedOrNearest = weightedOrNearest)
    # if (readWriteResult > 0){
    #     rciop.log("INFO", paste0("Aborting netcdf to obs - readGridsAndWriteObs(): ",readWriteResult))
    #     #q(save="no", status = 76)
    #     status <- status + 2
    # }

    # Change back to previous work dir
    if (! is.null(currentDir)){
        setwd(currentDir)
    }

    return (status)
} # prepare_and_run_netcdf_to_obs
## ------------------------------------------------------------------------------

# Return TRUE if leap year, else FALSE
leap_year <- function(year)
{
  leap <- ifelse( (year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, TRUE, FALSE)
  return (leap)
}
 
# Return number of days in requested month
# in  : year  - to determine leap year
# in  : month - 1 to 12
# out : days 28 to 31
days_per_month <- function(year, month)
{
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  is_leap_year <- leap_year(year)
  if (is_leap_year == TRUE) {
    month_days[2] <- 29
  }
  day <- month_days[month]
  if (verboseX2 == TRUE) {
    print('days_per_month: year month lastdayofmonth')
    print(year)
    print(month)
    print(day)
  }
  return (day)
} 


# In/Out - objects of posix date classes
determine_interval_hindcast <- function(forecastIssueDate,
                                        hindcastDays, # Integer/Numeric
                                        hypeStateDate)
{
    # Hindcast start date
    hindcast.startDate      <- forecastIssueDate
    hindcast.startDate$mday <- hindcast.startDate$mday - hindcastDays

    # Base hindcast start date depeding on HYPE state file
    if (hindcast.startDate <= hypeStateDate) {
      # Earlier than HYPE state date
      hindcast.startDate      <- hypeStateDate
      hindcast.startDate$mday <- hindcast.startDate$mday + 0 # + 1
    }else {
      # Later than HYPE state date

      # Limit hindcast start date to the first day of the month
      mday_as_char <- strftime(hindcast.startDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)
      if (mday_as_num > 1) {
        #hindcast.startDate$mday <- 1 # This seems to reset subtraction of hindcast.Days above...
        #hindcast.startDate$mday <- hindcast.startDate$mday - hindcast.startDate$mday + 1 # The same...
        hindcast.startDate$mday <- hindcast.startDate$mday - (mday_as_num - 1) # Ok, this worked...
      }

      # or: Limit hindcast start date to the first day of the calendar (jan 1)
      # or: Limit hindcast start date to the first day of the hydrological year (sep 1 ->aug 31)
    }

    hindcast.endDate <- forecastIssueDate
    hindcast.endDate$mday <- hindcast.endDate$mday - 1

    if (verbose == TRUE) {
      print('Hindcast:')
      print(hindcast.startDate)
      print(hindcast.endDate)
      print('')
    }

    output <- list("hindcast.startDate"=hindcast.startDate,
                   "hindcast.endDate"=hindcast.endDate)

    return (output)
} # determine_interval_hindcast


# In/Out - objects of posix date classes
determine_interval_hydrogfdei <- function(hindcastStartDate,
                                          forecastIssueDate,
                                          in_reforecast)
{
    hydrogfdei.startDate <- hindcastStartDate
    if (in_reforecast == FALSE) {
      # Operational

      # Should be: last day in the most recent hydrogfdei file (as new input parameter)
      # Now the last file is 201906
      hydrogfdei.endDate <- "2019-06-30"
      hydrogfdei.endDate <- as.Date(hydrogfdei.endDate)
      hydrogfdei.endDate <- as.POSIXlt(hydrogfdei.endDate)
    }else {
      # Re-forecast
      hydrogfdei.endDate <- forecastIssueDate
      
      mday_as_char <- strftime(hydrogfdei.endDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)
      if (mday_as_num < 10) {
          hydrogfdei.endDate$mon <- hydrogfdei.endDate$mon - 4
      }else {
          hydrogfdei.endDate$mon <- hydrogfdei.endDate$mon - 3
      }
            
      # Round off to end of this month
      mon_as_char  <- strftime(hydrogfdei.endDate,format="%m")
      mon_as_num   <- as.numeric(mon_as_char)
      year_as_char <- strftime(hydrogfdei.endDate,format="%Y")
      year_as_num  <- as.numeric(year_as_char)
      
      mday_last <- days_per_month(year_as_num,mon_as_num)
      hydrogfdei.endDate$mday <- hydrogfdei.endDate$mday + (mday_last - hydrogfdei.endDate$mday)
    }
    
    if (verbose == TRUE) {
      print('HydroGFDEI:')
      print(hydrogfdei.startDate)
      print(hydrogfdei.endDate)
      print('')
    }

    output <- list("hydrogfdei.startDate"=hydrogfdei.startDate,
                   "hydrogfdei.endDate"=hydrogfdei.endDate)

    return (output)
} # determine_interval_hydrogfdei


# In/Out - objects of posix date classes
determine_interval_hydrogfdod <- function(hydrogfdeiEndDate,
                                          forecastIssueDate,
                                          in_reforecast)
{
    hydrogfdod.startDate      <- hydrogfdeiEndDate
    hydrogfdod.startDate$mday <- hydrogfdod.startDate$mday + 1 # day
    if (in_reforecast == FALSE) {
      # Operational
      
      # Should be: last day in the most recent hydrogfdod file (as new input parameter)
      # Now the last file is 201908
      hydrogfdod.endDate <- "2019-08-31"
      hydrogfdod.endDate <- as.Date(hydrogfdod.endDate)
      hydrogfdod.endDate <- as.POSIXlt(hydrogfdod.endDate)
    }else {
      # Re-forecast
      hydrogfdod.endDate <- forecastIssueDate

      mday_as_char <- strftime(hydrogfdod.endDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)
      if (mday_as_num < 10) {
        hydrogfdod.endDate$mon <- hydrogfdod.endDate$mon - 2
      }else {
        hydrogfdod.endDate$mon <- hydrogfdod.endDate$mon - 1
      }

      # Round off to end of this month
      mon_as_char <- strftime(hydrogfdod.endDate,format="%m")
      mon_as_num  <- as.numeric(mon_as_char)
      year_as_char <- strftime(hydrogfdod.endDate,format="%Y")
      year_as_num  <- as.numeric(year_as_char)
      
      mday_last <- days_per_month(year_as_num,mon_as_num)
      hydrogfdod.endDate$mday <- hydrogfdod.endDate$mday + (mday_last - hydrogfdod.endDate$mday)
    }
    
    if (verbose == TRUE) {
      print('HydroGFDOD:')
      print(hydrogfdod.startDate)
      print(hydrogfdod.endDate)
      print('')
    }

    output <- list("hydrogfdod.startDate"=hydrogfdod.startDate,
                   "hydrogfdod.endDate"=hydrogfdod.endDate)

    return (output)
} # determine_interval_hydrogfdod


# In/Out - objects of posix date classes
determine_interval_od_daily <- function(hydrogfdodEndDate,
                                        hindcastEndDate)
{
    od.startDate      <- hydrogfdodEndDate
    od.startDate$mday <- od.startDate$mday + 1

    od.endDate        <- hindcastEndDate
    
    if (verbose == TRUE) {
      print('OD:')
      print(od.startDate)
      print(od.endDate)
      print('')
    }

    output <- list("od.startDate"=od.startDate,
                   "od.endDate"=od.endDate)

    return (output)
} # determine_interval_od_daily


# In/Out - objects of posix date classes
determine_interval_ecoper <- function(forecastIssueDate)
{
    ecoper.startDate    <- forecastIssueDate

    ecoper.endDate      <- forecastIssueDate
    ecoper.endDate$mday <- ecoper.endDate$mday + 9
    
    if (verbose == TRUE) {
      print('ECOPER:')
      print(ecoper.startDate)
      print(ecoper.endDate)
      print('')
    }

    output <- list("ecoper.startDate"=ecoper.startDate,
                   "ecoper.endDate"=ecoper.endDate)

    return (output)
} # determine_interval_ecoper


prepare_hindcast_intervals <- function(in_hindcastDays, # positive integer
                                       in_forecastIssueDate, # character string with dashes
                                       in_reforecast = TRUE,
                                       in_hypeStateFileDate) # character string without dashes
{
    # Dates internally in function uses class posixlt (list)

    ## ------------------------------------------------------------------------------
    # Handle inputs
    #hindcast.Days <- in_hindcastDays
    hindcast.Days <- as.numeric(in_hindcastDays)
    if (hindcast.Days < 0) {
      hindcast.Days <- 0
    }
    
    # Convert to posix date format
    forecast.IssueDate <- as.Date(in_forecastIssueDate)
    forecast.IssueDate <- as.POSIXlt(forecast.IssueDate)

    # ToDo: Handle no file, NULL or incorrect date
    dateLen <- nchar(as.character(in_hypeStateFileDate))
    if (dateLen > 6) {
      yyyy <- substr(in_hypeStateFileDate,1,4)
      mm   <- substr(in_hypeStateFileDate,5,6)
      dd   <- substr(in_hypeStateFileDate,7,8)
    }else {
      yyyy <- substr(in_hypeStateFileDate,1,4)
      mm   <- substr(in_hypeStateFileDate,5,6)
      dd   <- "01"
    }
    hypeState.Date <- paste(yyyy,mm,dd,sep="-")
    hypeState.Date <- as.Date(hypeState.Date) # No time
    hypeState.Date <- as.POSIXlt(hypeState.Date) # Still no time
    
    if (verbose == TRUE) {
      print('Handle inputs:')
      print('hindcast.Days:')
      print(hindcast.Days)
      print('forecast.IssueDate:')
      print(forecast.IssueDate)
      print('in_reforecast:')
      print(in_reforecast)
      print('hypeState.Date:')
      print(hypeState.Date)
      print('')
    }
    
    ## ------------------------------------------------------------------------------
    # Hindcast general
    intervalHindcast <- determine_interval_hindcast(forecast.IssueDate,
                                                    hindcast.Days,
                                                    hypeState.Date)

    ## ------------------------------------------------------------------------------
    # HydroGFDEI (monthly)
    intervalHydrogfdei <- determine_interval_hydrogfdei(intervalHindcast$hindcast.startDate,
                                                        forecast.IssueDate,
                                                        in_reforecast)

    ## ------------------------------------------------------------------------------
    # HydroGFDOD (monthly)
    intervalHydrogfdod <- determine_interval_hydrogfdod(intervalHydrogfdei$hydrogfdei.endDate,
                                                        forecast.IssueDate,
                                                        in_reforecast)

    ## ------------------------------------------------------------------------------
    # OD (daily)
    intervalOdDaily <- determine_interval_od_daily(intervalHydrogfdod$hydrogfdod.endDate,
                                                   intervalHindcast$hindcast.endDate)

    ## ------------------------------------------------------------------------------
    # Do not return the posixlt class objects
    # Convert data to character strings in the format yyyymm or yyyymmdd
    
    if (verbose == TRUE) {
      tmp <- strftime(intervalHindcast$hindcast.startDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(intervalHindcast$hindcast.endDate,format="%Y%m")
      print(tmp)

      tmp <- strftime(intervalHydrogfdei$hydrogfdei.startDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(intervalHydrogfdei$hydrogfdei.endDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(intervalHydrogfdod$hydrogfdod.startDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(intervalHydrogfdod$hydrogfdod.endDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(intervalOdDaily$od.startDate,format="%Y%m%d")
      print(tmp)
      tmp <- strftime(intervalOdDaily$od.endDate,format="%Y%m%d")
      print(tmp)
    }

    hindcastStartDateSearch <- strftime(intervalHindcast$hindcast.startDate,format="%Y-%m-%d")
    hindcastEndDateSearch   <- strftime(intervalHindcast$hindcast.endDate,format="%Y-%m-%d")

    hydrogfdeiStartDateSearch   <- strftime(intervalHydrogfdei$hydrogfdei.startDate,format="%Y-%m-%d")
    hydrogfdeiEndDateSearch     <- strftime(intervalHydrogfdei$hydrogfdei.endDate,format="%Y-%m-%d")
    hydrogfdeiStartDateFilename <- strftime(intervalHydrogfdei$hydrogfdei.startDate,format="%Y%m")
    hydrogfdeiEndDateFilename   <- strftime(intervalHydrogfdei$hydrogfdei.endDate,format="%Y%m")
    
    hydrogfdodStartDateSearch   <- strftime(intervalHydrogfdod$hydrogfdod.startDate,format="%Y-%m-%d")
    hydrogfdodEndDateSearch     <- strftime(intervalHydrogfdod$hydrogfdod.endDate,format="%Y-%m-%d")
    hydrogfdodStartDateFilename <- strftime(intervalHydrogfdod$hydrogfdod.startDate,format="%Y%m")
    hydrogfdodEndDateFilename   <- strftime(intervalHydrogfdod$hydrogfdod.endDate,format="%Y%m")
    
    odStartDateSearch   <- strftime(intervalOdDaily$od.startDate,format="%Y-%m-%d")
    odEndDateSearch     <- strftime(intervalOdDaily$od.endDate,format="%Y-%m-%d")
    odStartDateFilename <- strftime(intervalOdDaily$od.startDate,format="%Y%m%d")
    odEndDateFilename   <- strftime(intervalOdDaily$od.endDate,format="%Y%m%d")
    
    castIntervals <- list("hindcastStartDateSearch"=hindcastStartDateSearch,
                          "hindcastEndDateSearch"=hindcastEndDateSearch,

                          "hydrogfdeiStartDateSearch"=hydrogfdeiStartDateSearch,
                          "hydrogfdeiEndDateSearch"=hydrogfdeiEndDateSearch,
                          "hydrogfdeiStartDateFilename"=hydrogfdeiStartDateFilename,
                          "hydrogfdeiEndDateFilename"=hydrogfdeiEndDateFilename,

                          "hydrogfdodStartDateSearch"=hydrogfdodStartDateSearch,
                          "hydrogfdodEndDateSearch"=hydrogfdodEndDateSearch,
                          "hydrogfdodStartDateFilename"=hydrogfdodStartDateFilename,
                          "hydrogfdodEndDateFilename"=hydrogfdodEndDateFilename,

                          "odStartDateSearch"=odStartDateSearch,
                          "odEndDateSearch"=odEndDateSearch,
                          "odStartDateFilename"=odStartDateFilename,
                          "odEndDateFilename"=odEndDateFilename
                          )
    return(castIntervals)
    
} # prepare_hindcast_intervals


prepare_forecast_intervals <- function(in_forecastIssueDate) # character string with dashes
{
    # Convert to posix date format
    forecast.IssueDate <- as.Date(in_forecastIssueDate)
    forecast.IssueDate <- as.POSIXlt(forecast.IssueDate)
    
    if (verbose == TRUE) {
      print('Handle inputs:')
      print('forecast.IssueDate:')
      print(forecast.IssueDate)
      print('')
    }

    ## ------------------------------------------------------------------------------
    # ECOPER (daily)
    intervalEcoper <- determine_interval_ecoper(forecast.IssueDate)

    ## ------------------------------------------------------------------------------
    # Do not return the posixlt class objects
    # Convert data to character strings in the format yyyymm or yyyymmdd
    
    if (verbose == TRUE) {
      tmp <- strftime(intervalEcoper$ecoper.startDate,format="%Y%m%d")
      print(tmp)
      tmp <- strftime(intervalEcoper$ecoper.endDate,format="%Y%m%d")
      print(tmp)
    }

    ecoperStartDateSearch   <- strftime(intervalEcoper$ecoper.startDate,format="%Y-%m-%d")
    ecoperEndDateSearch     <- strftime(intervalEcoper$ecoper.endDate,format="%Y-%m-%d")
    ecoperStartDateFilename <- strftime(intervalEcoper$ecoper.startDate,format="%Y%m%d")
    ecoperEndDateFilename   <- strftime(intervalEcoper$ecoper.endDate,format="%Y%m%d")
    
    castIntervals <- list("ecoperStartDateSearch"=ecoperStartDateSearch,
                          "ecoperEndDateSearch"=ecoperEndDateSearch,
                          "ecoperStartDateFilename"=ecoperStartDateFilename,
                          "ecoperEndDateFilename"=ecoperEndDateFilename
                          )
    return(castIntervals)
    
} # prepare_forecast_intervals


# Common
search_and_download <- function(urlNC,
                                query,
                                ncRootDir,
                                ncSubDir)
{
  # Constants
  osClientApp <- "opensearch-client"

  local.netcdfDir <- paste(ncRootDir,ncSubDir,sep="/")
  if (! dir.exists(local.netcdfDir)){
      dir.create(local.netcdfDir)
  }
  opensearchCmd=paste0(osClientApp," '",urlNC,query,"'"," enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  if (length(input_enclosure >= 1)) {
      for (url in 1:length(input_enclosure)) {
         rciop.copy(input_enclosure[url],local.netcdfDir)
      }
  }

} # search_and_download


# Common
search_and_download_netcdf <- function(urlNC,
                                       query,
                                       startDate,
                                       stopDate,
                                       ncRootDir,
                                       ncSubDir)
{
  # Constants
  osClientApp <- "opensearch-client"
  secStartDay <- "T00:00:01"
  secEndDay   <- "T23:59:59"
  variables   <- c("pr","tas","tasmin","tasmax")

  for (var in 1:length(variables)){
      if (ncSubDir == TRUE){
          local.netcdfDir <- paste(ncRootDir,variables[var],sep="/")
          # Append query with variable
          varQuery <- paste0(',',variables[var],']')
          catQuery <- gsub(']',varQuery,query)
      }else{
          local.netcdfDir <- ncRootDir
          # All files into one dir
          catQuery <- query
      }
      if (! dir.exists(local.netcdfDir)) { dir.create(local.netcdfDir) }
      opensearchCmd=paste0(osClientApp," '",urlNC,"'"," -p ","'count=unlimited'"," -p ","'cat=",catQuery,"'"," -p ","'start=",startDate,secStartDay,"'"," -p ","'stop=",stopDate,secEndDay,"'"," enclosure")
      message(opensearchCmd)
      input_enclosure <- system(command = opensearchCmd,intern = T)
      if (length(input_enclosure >= 1)) {
          #if (! dir.exists(local.netcdfDir)) { dir.create(local.netcdfDir) }
          for (url in 1:length(input_enclosure)) {
              rciop.copy(input_enclosure[url],local.netcdfDir)
          }
      }
      if (ncSubDir == FALSE){
          if (var == 1){
            break # for loop
          }
      }
  }

} # search_and_download_netcdf


# Common
check_date_interval_netcdf <- function(startDate,
                                       endDate,
                                       ncRootDir,
                                       ncSubDir,
                                       filePrefix,
                                       fileSuffix)
{
  #tasmin_hydrogfdei_201906_fanfar_SMHI.nc
  #tasmin_hydrogfdod_201908_fanfar_SMHI.nc
  #tas_od-daily_20190917_fanfar_SMHI.nc
  #tasmin_ecoper_2019091800_fanfar_SMHI.nc # 00_fanfar_SMHI.nc
  nMissingFiles <- 0

  # Constants
  variables <- c("pr","tas","tasmin","tasmax")

  interval <- NULL
  dateFormat <- NULL
  if (grepl("hydrogfdei",filePrefix,fixed=TRUE)) {
      interval <- "month"
      dateFormat <- "%Y%m"
  } else if (grepl("hydrogfdod",filePrefix,fixed=TRUE)) {
      interval <- "month"
      dateFormat <- "%Y%m"
  } else if (grepl("od-daily",filePrefix,fixed=TRUE)) {
      interval <- "day"
      dateFormat <- "%Y%m%d"
  } else if (grepl("ecoper",filePrefix,fixed=TRUE)) {
      interval <- "day"
      dateFormat <- "%Y%m%d"
  }

  sDate <- as.Date(startDate)
  eDate <- as.Date(endDate)

  # List of date objects. Assumes end date later than start date
  seqTimeStamps <- seq.Date(from=sDate,to=eDate,by=interval)

  for (month_or_day in 1:length(seqTimeStamps)){
      fileDate <- strftime(seqTimeStamps[month_or_day],format=dateFormat)
      for (var in 1:length(variables)){
          if (ncSubDir == TRUE){
              local.netcdfDir <- paste(ncRootDir,variables[var],sep="/")
          }else{
              # All files into one dir
              local.netcdfDir <- ncRootDir
          }

          expFilename <- paste0(variables[var],filePrefix,fileDate,fileSuffix)
          #print(expFilename)
          localPath <- paste(local.netcdfDir,expFilename,sep="/")
          if (! file.exists(localPath)){
              nMissingFiles <- nMissingFiles + 1
              #print(paste0("File missing: ",expFilename))
              print(paste0("File missing: ",localPath))
          }
      }
  }

  return (nMissingFiles)
} # check_date_interval_netcdf


# Common
# This handles the sequence for each x obs  file (iterates opensearch, download each meterological GFD, call function to convert to obs) 
# Retrieve files (pr,tas,tasmin,tasmax) via opensearch and rciop.copy
# Configurable option to download all files into one dir or individual sub-dirs
# for pr, tas, tasmin and tasmax.
download_netcdf <- function(modelConfig,    # sub-dir to use for local download dir, url, query pattern etc 
                            xCastsInterval, # dates for search start/stop, expected date in filename to check against
                            netcdfDir,      # base dir to download files too
                            ncSubDir,       # False-one dir, True-separate dir for each variable
                            xCast=NULL)     # hindcast, forecast, elevation
{
  # Other inputs are:
  # path_to_store_netcdf_files - temporary dir
  # path to store obs files - res dir
  # expected file name for retrived netcdf files - to compare with after download
  
  if (verbose == TRUE) {
    print("download_netcdf:")
    print(modelConfig)
    print(xCastsInterval)
    print(xCast)
  }
  
  # Constants
  variables   <- c("pr","tas","tasmin","tasmax")
  fileSuffix <- "_fanfar_SMHI.nc"

  if (! dir.exists(netcdfDir)){
    dir.create(netcdfDir)
  }

  if (xCast == "hindcast"){
      ## ------------------------------------------------------------------------------
      # Handle hydrogfdei
      urlNC     <- modelConfig$gfdHydrogfdeiUrl
      query     <- modelConfig$gfdHydrogfdeiQuery
      startDate <- xCastsInterval$hydrogfdeiStartDateSearch
      stopDate  <- xCastsInterval$hydrogfdeiEndDateSearch
      
      search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      nMissing <- check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                            "_hydrogfdei_",fileSuffix)
      if (nMissing > 0){
          print(paste0(nMissing," file(s) missing for hydrogfdei"))
          rciop.log("INFO",paste0(nMissing," file(s) missing for hydrogfdei"))
      }
      
      ## ------------------------------------------------------------------------------
      # Handle hydrogfdod
      urlNC     <- modelConfig$gfdHydrogfdodUrl
      query     <- modelConfig$gfdHydrogfdodQuery
      startDate <- xCastsInterval$hydrogfdodStartDateSearch
      stopDate  <- xCastsInterval$hydrogfdodEndDateSearch

      search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      # Check retrieved filenames
      nMissing <- check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                            "_hydrogfdod_",fileSuffix)
      if (nMissing > 0){
          print(paste0(nMissing," file(s) missing for hydrogfdod"))
          rciop.log("INFO",paste0(nMissing," file(s) missing for hydrogfdod"))
      }
      
      ## ------------------------------------------------------------------------------
      # Handle od (od-daily)
      urlNC     <- modelConfig$gfdOdDailyUrl
      query     <- modelConfig$gfdOdDailyQuery
      startDate <- xCastsInterval$odStartDateSearch
      stopDate  <- xCastsInterval$odEndDateSearch

      search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      nMissing <- check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                            "_od-daily_",fileSuffix)
      if (nMissing > 0){
          print(paste0(nMissing," file(s) missing for od-daily"))
          rciop.log("INFO",paste0(nMissing," file(s) missing for od-daily"))
      }
  } # hindcast
  if (xCast == "forecast"){
      ## ------------------------------------------------------------------------------
      # Handle ecoper
      urlNC     <- modelConfig$gfdEcoperUrl
      query     <- modelConfig$gfdEcoperQuery
      startDate <- xCastsInterval$ecoperStartDateSearch
      stopDate  <- xCastsInterval$ecoperEndDateSearch

      search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      nMissing <- check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                            "_ecoper_",paste0("00",fileSuffix))
      if (nMissing > 0){
          print(paste0(nMissing," file(s) missing for ecoper"))
          rciop.log("INFO",paste0(nMissing," file(s) missing for ecoper"))
      }
  } # forecast
  if (xCast == "elevation"){
      ## ------------------------------------------------------------------------------
      # Handle elevation
      urlNC     <- modelConfig$gfdElevationUrl
      query     <- modelConfig$gfdElevationQuery
      subDir    <- modelConfig$gfdElevationSubDir
      #startDate <- xCastsInterval$ecoperStartDateSearch
      #stopDate  <- xCastsInterval$ecoperEndDateSearch

      search_and_download(urlNC,query,ncRootDir=netcdfDir,ncSubDir=subDir)
      expFilename <- paste(netcdfDir,subDir,"HydroGFD2elevation.nc",sep="/")
      if (! file.exists(expFilename)){
          rciop.log("INFO",paste0("Missing file: ",expFilename))
      }
  } # elevation
  ## ------------------------------------------------------------------------------
  # All meterological gfd parts downloaded to local tmp dir
  
} # download_netcdf


# Possibly split into two functions
# one for hindcast sequence and
# one for forcast sequence

# External function
# Wrapper for netcdf2obs sequence
process_hindcast_netcdf2obs <- function(modelConfig, # Misc config data, now 
                                        modelDataConfig, # Misc model config data, paths to state files, forcing, shape files
                                        forecastIssueDate, # yyyy-mm-dd
                                        hindcastPeriodLength, # Days
                                        netcdfDir, # Dir to store netcdf files
                                        ncSubDir, # False-one dir, True-separate dir for each variable
                                        gridMetaDir, # Dir to store grid weight files
                                        obsDir) # Dir to store obs files
{
  # Prepare hindcast and forecast intervals, start and end dates
  prepHindcastInterval <- prepare_hindcast_intervals(hindcastPeriodLength,
                                                     forecastIssueDate,
                                                     in_reforecast = TRUE,
                                                     "19700101") # ToDo: Get date from latest Hype state filename
  # Download hydrogfd elevation netcdf file
  download_netcdf(modelConfig,
                  xCastsInterval = prepHindcastInterval, # Not used
                  netcdfDir,
                  ncSubDir,
                  xCast="elevation")

  # For the calculated time interval, search and download hydrogfd netcdf files
  download_netcdf(modelConfig,
                  xCastsInterval = prepHindcastInterval, # Now different types... either have one type only...
                  netcdfDir,
                  ncSubDir,
                  xCast="hindcast")

  # Produce the files Pobs.txt, Tobs.txt, TMINobs.txt and TMAXobs.txt
  # Depending on selected dir, copy the files to run dir
  startDate <- prepHindcastInterval$hindcastStartDateSearch
  endDate   <- prepHindcastInterval$hindcastEndDateSearch
  
  # ToDo: gridLink...
  if (! dir.exists(gridMetaDir)){ # or a certain file
      res <- run_netcdf_to_obs_gridLinkPreparation(workDir=paste0(TMPDIR,"/netcdf_to_obs"),
                                                                  ncRootDir=netcdfDir,
                                                                  ncSubDir=ncSubDir,
                                                                  resourceDir=NULL,
                                                                  gridElevPath=paste(netcdfDir,modelConfig$gfdElevationSubDir,"HydroGFD2elevation.nc",sep="/"),
                                                                  shapeFilePath=modelDataConfig$dirShapeFiles,
                                                                  outPath=gridMetaDir,
                                                                  startDate,
                                                                  endDate
                                                                  )
      if (res > 0){
          rciop.log("INFO",paste0("run_netcdf_to_obs_gridLinkPreparation, exit code=",res))
      }
  }
  
  res <- prepare_and_run_netcdf_to_obs(workDir=paste0(TMPDIR,"/netcdf_to_obs"),
                                                      ncRootDir=netcdfDir,
                                                      ncSubDir=ncSubDir,
                                                      resourceDir=gridMetaDir,
                                                      gridElevPath=paste(netcdfDir,modelConfig$gfdElevationSubDir,"HydroGFD2elevation.nc",sep="/"),
                                                      shapeFilePath=modelDataConfig$dirShapeFiles,
                                                      outPath=obsDir,
                                                      startDate,
                                                      endDate
                                                      )
  if (res > 0){
      rciop.log("INFO",paste0("prepare_and_run_netcdf_to_obs, exit code=",res))
  }

  #if (obsDir != runDir){
  #  # Copy obs files to run dir
  #}

  # Return the same type structure as function getModelForcing() and readXobsData(), but minimal
  bdate <- as.Date(startDate)
  cdate <- as.Date(prepHindcastInterval$hydrogfdeiEndDateSearch)
  edate <- as.Date(endDate)
  hindcast.forcing <- list("bdate"=bdate,
                           "cdate"=cdate,
                           "edate"=edate)
  
  xobsVar   <- c("var xyz") # ToDo
  xobsSubid <- c("subid xyz") # ToDo
  xobs.input <- list("xobsVar"=xobsVar,
                     "xobsSubid"=xobsSubid)

  output <- list("hindcast.forcing"=hindcast.forcing,
                 "xobs.input"=xobs.input)

  return (output)

} # process_hindcast_netcdf2obs


# ToDo: Remove/Replace some inputs, call other func for forecast interval
# External function
# Wrapper for netcdf2obs sequence
process_forecast_netcdf2obs <- function(modelConfig, # Misc config data, now 
                                        modelDataConfig, # Misc model config data, paths to state files, forcing, shape files
                                        forecastIssueDate, # yyyy-mm-dd
                                        netcdfDir, # Dir to store netcdf files
                                        ncSubDir, # False-one dir, True-separate dir for each variable
                                        gridMetaDir, # Dir to read grid weight files
                                        obsDir) # Dir to store obs files
{
  # Prepare hindcast and forecast intervals, start and end dates
  prepForecastInterval <- prepare_forecast_intervals(forecastIssueDate)

  # For the calculated time interval, search and download hydrogfd netcdf files
  download_netcdf(modelConfig,
                  xCastsInterval = prepForecastInterval, # Now different types... either have one type only...
                  netcdfDir,
                  ncSubDir,
                  xCast="forecast")
  
  # Produce the files Pobs.txt, Tobs.txt, TMINobs.txt and TMAXobs.txt
  # Depending on selected dir, copy the files to run dir
  startDate <- prepForecastInterval$ecoperStartDateSearch
  endDate   <- prepForecastInterval$ecoperEndDateSearch
  
  res <- prepare_and_run_netcdf_to_obs(workDir=paste0(TMPDIR,"/netcdf_to_obs"),
                                       ncRootDir=netcdfDir,
                                       ncSubDir=ncSubDir,
                                       resourceDir=gridMetaDir,
                                       gridElevPath=paste(netcdfDir,modelConfig$gfdElevationSubDir,"HydroGFD2elevation.nc",sep="/"),
                                       shapeFilePath=modelDataConfig$dirShapeFiles,
                                       outPath=obsDir,
                                       startDate,
                                       endDate
                                       )
  if (res > 0){
      rciop.log("INFO",paste0("prepare_and_run_netcdf_to_obs, exit code=",res))
  }

  #if (obsDir != runDir){
  #  # Copy obs files to run dir
  #}

  # Return the same type structure as function getModelForcing(), but minimal
  bdate <- as.Date(startDate)
  cdate <- as.Date(startDate)
  edate <- as.Date(endDate)
  forecast.forcing <- list("bdate"=bdate,
                           "cdate"=cdate,
                           "edate"=edate)

  output <- list("forecast.forcing"=forecast.forcing)

  return (output)

} # process_forecast_netcdf2obs
