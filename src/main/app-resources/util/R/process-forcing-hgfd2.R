
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-forcing-common.R",sep="/"))
}else{
    source ('process-forcing-common.R')
}

# Constants
nameOfSrcFile_PF2 <- "/util/R/process-forcing-hgfd2.R"

cLimitHindcastPeriodDays <- 130

#verbose <- TRUE
#verbose <- FALSE
#verboseVerbose <- TRUE
#verboseVerbose <- FALSE

# Includes that the sourced R-files uses
library(ncdf4)

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
    if (verboseVerbose == TRUE) {
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
    fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_hgfd2/netcdf_to_obs_hgfd2_utils.R")
    if (! file.exists(fileToSource)){
        cmn.log(paste0("Aborting netcdf to obs hgfd2 - file missing: ",fileToSource), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        q(save="no", status = 0) # 77
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
        cmn.log(paste0("Aborting netcdf to obs - file missing: ",resource_dir_path), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        #q(save="no", status = 0)
    }
    #print("List files resource_dir_path:")
    #tmpPath=resource_dir_path
    #print(list.files(tmpPath))

    grid_elev_path <- gridElevPath
    if (! file.exists(grid_elev_path)){
        cmn.log(paste0("Aborting netcdf to obs - file missing: ",grid_elev_path), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        #q(save="no", status = 0)
    #}else{
    #  print("grid_elev_path exists")
    #  print(grid_elev_path)
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
        cmn.log(paste0("Aborting netcdf to obs - file missing: ",shape_file_path), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
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

    # print("PATHS:")
    # print(paste0("grid.path: ",grid_dir_path[1]))
    # print(paste0("grid.elev: ",grid_elev_path))
    # print(paste0("grid.meta: ",resource_dir_path))
    # print(paste0("output.path: ",out_path))
    # print(paste0("model.shape: ",shape_file_path))

    # Check/re-generate gridLink
    isGridLink = gridLinkPreparation(grid.path = grid_dir_path[1]
                                    ,grid.pattern = nc_file_pattern[1]
                                    ,grid.elev = grid_elev_path
                                    ,var.name = nc_var_name[1]
                                    ,grid.meta = resource_dir_path
                                    ,output.path = out_path
                                    ,redoGridLink = redoGridLink
                                    ,model.shape = shape_file_path
                                    ,cleanGeometry = cleanGeometry
                                    ,crsProj = crsProjProc)
    if (isGridLink > 0){
        cmn.log(paste0("Aborting netcdf to obs - gridLinkPreparation(): ",isGridLink), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        #q(save="no", status = 76)
        #status <- status + 1
    }

    # Change back to previous work dir
    if (! is.null(currentDir)){
        setwd(currentDir)
    }

    return (status)
} # run_netcdf_to_obs_gridLinkPreparation


prepare_and_run_netcdf_to_obs <- function(workDir, # TMPDIR/netcdf_to_obs
                                          ncRootDir, # Path to netcdf files
                                          ncSubDir, # False-one dir, True-separate dir for each variable
                                          outPath, # Path for output files, calling function to handle publish?
                                          startDate, # yyyy-mm-dd
                                          endDate)# yyyy-mm-dd
{
    if (verboseVerbose == TRUE) {
        print('prepare_and_run_netcdf_to_obs():')
        print(paste0("workDir: ",workDir))
        print(paste0("ncRootDir: ",ncRootDir))
        print(paste0("ncSubDir: ",ncSubDir))
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
    fileToSource <- paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/util/R/netcdf_to_obs_hgfd2/netcdf_to_obs_hgfd2_utils.R")
    if (! file.exists(fileToSource)){
        cmn.log(paste0("Aborting netcdf to obs hgfd2 - file missing: ",fileToSource), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        #q(save="no", status = 77)
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

    nc_var_name     <- c("pr","tas","tasmin","tasmax")
    hype_obs_type   <- c("Pobs","Tobs","TMINobs","TMAXobs")
    nc_file_pattern <- c("pr_","tas_","tasmin_","tasmax_")

    obsScale  <- c(86400,1,1,1)
    obsOffset <- c(0,-273.15,-273.15,-273.15)
    obsDigits <- c(3,1,1,1)

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

    # Read netcdf data from the grid.data folder and generate (new) PT-obs files and ForcKey.txt using a gridLink.Rdata
    # Also possible to read a new time period and merge with existing data (see file mentioned above)
    readWriteResult = readGridsAndWriteObs(
                           grid.path         = grid_dir_path
                          ,grid.pattern      = nc_file_pattern
                          ,var.name          = nc_var_name
                          ,obs.type          = hype_obs_type
                          ,obs.scale         = obsScale
                          ,obs.offset        = obsOffset
                          ,obs.digits        = obsDigits
                          ,gridLink.path     = paste(out_path,"/gridLink.Rdata",sep="")
                          ,output.path       = out_path
                          ,overwrite         = T
                          ,doForcKey         = T
                          ,elev.digits       = 1
                          ,time.start        = timeStart.1
                          ,time.end          = timeEnd.1
                          ,weightedOrNearest = weightedOrNearest)
    if (readWriteResult > 0){
        cmn.log(paste0("Aborting netcdf to obs - readGridsAndWriteObs(): ",readWriteResult), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
        #q(save="no", status = 76)
        #status <- status + 2
    }

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
  if (verboseVerbose == TRUE) {
    print('days_per_month: year month lastdayofmonth')
    print(year)
    print(month)
    print(day)
  }
  return (day)
}


# Common
# Search for the latest item depending on url and query.
# Returns a list with status and date as a character string. On error status = 1
search_and_locate_latest_date <- function(url,
                                          query)
{
    # Constants
    osClientApp <- "opensearch-client"

    # Outputs
    status   <- 1 # Not ok
    yyyymmdd <- ""

    # Latest, without specifying any date
    opensearchCmd=paste0(osClientApp," '",url,"'"," -p ","'count=1'"," -p ","'cat=",query,"'"," enclosure")
    message(opensearchCmd)
    res_enclosure <- system(command = opensearchCmd,intern = T)
    #Example res_enclosure:"https://store.terradue.com/hydro-smhi/fanfar/Operational/hydrogfdei/2019/07/tasmin_hydrogfdei_201907_fanfar_SMHI.nc"

    if (nchar(res_enclosure > 0)) {
        # Locate and extract date from filename in returned http url
        # .*  - match any character
        # ()  - capturing group with expected match for 6 single digits
        # \\1 - reference to first captured pattern
        yearmon <- gsub(pattern=".*([0-9]{6}).*",replace="\\1", res_enclosure)
        if (nchar(yearmon) >= 6) { 
            yyyy <- substr(yearmon,1,4)
            mm   <- substr(yearmon,5,6)
            dd   <- days_per_month(as.numeric(yyyy),as.numeric(mm))
        
            yyyymmdd <- paste(yyyy,mm,dd,sep="-")
            status   <- 0
        }else{
            print("Could not locate date in filename")
        }
    }

    output <- list("status"=status,
                   "date"=yyyymmdd)

    return (output)
} # search_and_locate_latest_date



# Input  - local directory to check for state files
# Output - status=0 and date (posix date class), filename incl. path.
#        - status=1 when file not found or other error, return date 1901-01-01, filename NULL
# check_latest_statefiles <- function(filePath)
# {
#   status   <- 1 # NOK
#   fileDate <- "19010101"
#   fileName <- NULL

#   # ToDo: Add additional categories in filename? At least if files are stored elsewhere.
#   # At least HydroGFD version and hindcast start date?
#   if (dir.exists(filePath)){
#     listFilenames <- list.files(path=filePath,pattern="state_save")
#     if (length(listFilenames) > 0){
#       # Locate and extract date from filename
#       # .*  - match any character
#       # ()  - capturing group with expected match for 8 single digits
#       # \\1 - reference to first captured pattern
#       listDates <- gsub(pattern=".*([0-9]{8}).*",replace="\\1",listFilenames)
#       if (length(listDates) > 0){
#         # Locate the latest date, comparing dates as integers
#         latestDate <- 0
#         indexDate  <- -1
#         for (i in 1:length(listDates)){
#           if (nchar(listDates[i]) == 8){ # Added due to bad regular expression above - the complete filename can be returned as it is now
#               if (as.numeric(listDates[i]) > latestDate){
#                 latestDate <- as.numeric(listDates[i])
#                 indexDate  <- i
#               }
#           }
#         }
    
#         if (indexDate > 0) {
#           fileDate <- listDates[indexDate]
#           fileName <- paste(filePath,listFilenames[indexDate],sep="/")
#           status   <- 0
#         }
#       }
#     }
#   }
  
#   fileDate <- as.Date(fileDate,format="%Y%m%d")
#   fileDate <- as.POSIXlt(fileDate)
  
#   return (list("status"=status,
#                "fileDate"=fileDate,
#                "fileName"=fileName))
# }


# External function
# Input  - meteo type (e.g. GFD, HydroGFD, HydroGFD 2.0), meteo version (e.g. 1.3, 2.0) optional, hindcast start date (bdate).
# Output - formatted string to use as prefix for comparing filenames of state files.
#          Uses naming from the constant cMeteoHindcastVariants.
get_requested_statefile_suffix <- function(meteo,meteoVersion=NULL,hindcastStartDate)
{
  defMet    <- "unknown"
  defMetVer <- "0.0"

  met    <- defMet
  metVer <- defMetVer

  if (is.null(meteoVersion)) {
    # Assume meteo is one of the defined constants (cMeteoHindcastVariantx) with both name and version
    for (v in 1:length(cMeteoHindcastVariants)) {
      m1 <- tolower(cMeteoHindcastVariants[v])
      m2 <- tolower(meteo)
      # Compare with input data
      if (m1 == m2) {
        met    <- gsub(pattern=" ",replacement="",m1)
        metVer <- ""
      }
    }
  }else {
    # Check both name and version against the defined constants (cMeteoHindcastVariantx)
    for (v in 1:length(cMeteoHindcastVariants)) {
      variantList <- as.list(strsplit(cMeteoHindcastVariants[v],'\\s+')[[1]]) # Split constant string on space
      if (length(variantList) > 1) {
        m1    <- tolower(variantList[1])
        m1ver <- variantList[2]
        # Compare with input data
        if ((tolower(meteo) == m1) && (meteoVersion == m1ver)) {
          met    <- m1
          metVer <- m1ver
        }
      }
    }
  }

  hcdate <- gsub(pattern="-",replacement="",hindcastStartDate)

  # Example: -hydrogfd2.0-bdate20100101
  requestedFileNameSuffix <- paste0("-",met,meteoVersion,"-bdate",hcdate)

  return (requestedFileNameSuffix)
} # get_requested_statefile_suffix


# Get forecast issue date from filename(s) matching the requested categories
# Return the filename with the latest forecast issue date
# Input  - local directory to check for state files
# Output - status=0 and date (posix date class), filename incl. path.
#        - status=1 when file not found or other error, return date 9999-01-01, filename NULL
check_latest_statefiles_category <- function(filePath,meteo,meteoVersion,hindcastStartDate) #,forecastIssueDate)
{
  status   <- 1 # NOK
  fileDate <- "99990101"
  fileName <- NULL
  
  #state_save20180101.txt-hydrogfd2.0-bdate20100101
  
  requestedFileNamePrefix <- "state_save"
  requestedFileNameSuffix <- get_requested_statefile_suffix(meteo,meteoVersion,hindcastStartDate)
  requestedFileNameSuffix <- paste0(".txt",requestedFileNameSuffix)
  
  latestDate <- 0
  if (dir.exists(filePath)){
    listFilenames <- list.files(path=filePath,pattern=requestedFileNamePrefix)
    if (length(listFilenames) > 0){
      for (fileIndex in 1:length(listFilenames)) {
        res <- grep(pattern=requestedFileNameSuffix,listFilenames[fileIndex],fixed=TRUE)
        if (length(res) > 0) {
          startOfString <- gsub(pattern=requestedFileNameSuffix,replacement="",listFilenames[fileIndex])
          res <- grep(pattern=requestedFileNamePrefix,startOfString,fixed=TRUE)
          if (length(res) > 0) {
            dateString <- gsub(pattern=requestedFileNamePrefix,replacement="",startOfString)

            # Check that the remaining string is only numbers, should only be one date
            listDates <- gsub(pattern=".*([0-9]{8}).*",replace="\\1",dateString)
            if (length(listDates) > 0){
              for (d in 1:length(listDates)){
                  if (as.numeric(listDates[d]) > latestDate){
                    latestDate <- as.numeric(listDates[d])

                    fileDate <- listDates[d]
                    #fileName <- paste(filePath,listFilenames[fileIndex],sep="/")
                    fileName <- listFilenames[fileIndex] # Without path
                    status   <- 0
                  }
              }
            }
          
          }
        }
      }
    }
  }
  
  # ToDo: Check if found date is later than forecast issue date, return status 1 (NOK)?
  fileDate <- as.Date(fileDate,format="%Y%m%d")
  fileDate <- as.POSIXlt(fileDate)
  
  return (list("status"=status,
               "fileDate"=fileDate,
               "fileName"=fileName))
} # check_latest_statefiles_category


# In/Out - objects of posix date classes
determine_interval_hindcast <- function(forecastIssueDate,
                                        hindcastDays, # Integer/Numeric
                                        stateFileCreation,
                                        meteoHindcastType,
                                        statefileHindcastDate,
                                        pathStateFiles)
{
    # Constants
    # HydroGFD 2 files on the store are available from 1979
    cFilesHydroGFD2EIStartDate <- as.Date("1979-01-01")
    cFilesHydroGFD2EIStartDate <- as.POSIXlt(cFilesHydroGFD2EIStartDate)

    # Hindcast start date
    hindcast.startDate      <- forecastIssueDate
    hindcast.startDate$mday <- hindcast.startDate$mday - hindcastDays

    # Check if state file exists (independent of run type mode)
    resStateFile <- check_latest_statefiles_category(pathStateFiles,
                                                     meteo=meteoHindcastType,
                                                     meteoVersion=NULL,
                                                     hindcastStartDate=statefileHindcastDate)

    limitDateForUseOfStateFile      <- resStateFile$fileDate
    limitDateForUseOfStateFile$mday <- limitDateForUseOfStateFile$mday + cLimitHindcastPeriodDays

    doUseStateFile <- (resStateFile$status == 0)
    doUseStateFile <- doUseStateFile && (forecastIssueDate > limitDateForUseOfStateFile)

    # Base hindcast start date depeding on HYPE state file
    if ((hindcast.startDate <= resStateFile$fileDate) && doUseStateFile) {
      # Start date earlier
      hindcast.startDate      <- resStateFile$fileDate
      hindcast.startDate$mday <- hindcast.startDate$mday + 0 # + 1
      cmn.log(paste0("Using state file: ",resStateFile$fileName), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    }else {
      # Start date later

      # Limit hindcast start date to the first day of the month
      mday_as_char <- strftime(hindcast.startDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)
      if (mday_as_num > 1) {
        hindcast.startDate$mday <- hindcast.startDate$mday - (mday_as_num - 1)
      }

      # or: Limit hindcast start date to the first day of the calendar (jan 1)
      # or: Limit hindcast start date to the first day of the hydrological year (sep 1 ->aug 31)

      doUseStateFile <- FALSE
    }

    # Limited in this function instead of in determine_interval_hydrogfdei() since output parameter
    # may be used by other functions
    if (hindcast.startDate < cFilesHydroGFD2EIStartDate) {
      cmn.log("Limiting hindcast start date due to available files for HydroGFD 2", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      hindcast.startDate <- cFilesHydroGFD2EIStartDate
    }

    # HYPE adaption - one day longer for run type 'state file creation'
    hindcast.endDate <- forecastIssueDate
    if (! stateFileCreation) {
        hindcast.endDate$mday <- hindcast.endDate$mday - 1
    }

    if (! doUseStateFile) {
      # FileDate only used locally
      # Clear output FileName to not trigger later functionality
      resStateFile$fileName <- NULL
    }

    cmn.log("Hindcast:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hindcast.startDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hindcast.endDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

    output <- list("hindcast.startDate"=hindcast.startDate,
                   "hindcast.endDate"=hindcast.endDate,
                   "stateFile"=resStateFile$fileName)

    return (output)
} # determine_interval_hindcast


# In/Out - objects of posix date classes
determine_interval_hydrogfdei <- function(hindcastStartDate,
                                          forecastIssueDate,
                                          in_reforecast,
                                          stateFileCreation,
                                          in_url,
                                          in_query)
{
    hydrogfdei.startDate <- hindcastStartDate

    if (in_reforecast == FALSE) {
      # Operational
      res <- search_and_locate_latest_date(in_url,in_query)
      if (res$status == 0) {
          hydrogfdei.endDate <- res$date
          hydrogfdei.endDate <- as.Date(hydrogfdei.endDate)
          hydrogfdei.endDate <- as.POSIXlt(hydrogfdei.endDate)
          if (hydrogfdei.endDate < hydrogfdei.startDate) {
              cmn.log(paste("Aborting, the latest hydrogfdei netcdf file date preceeds hydrogfdei start date: ",hydrogfdei.endDate), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
              q(save="no", status = 3)
          }
      }else{
          cmn.log("Aborting, not able to locate the latest hydrogfdei netcdf file", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
          q(save="no", status = 3)
      }

    }else {
      # Re-forecast
      hydrogfdei.endDate <- forecastIssueDate

      mday_as_char <- strftime(hydrogfdei.endDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)

      # Handle case for month day 31 (and 29, 30) and resulting months with less days.
      # Month day after a month subtraction may for certain resulting months end up in the month n+1 as day 1 or 2.
      # E.g. idate: yyyy-08-31 -> yyyy-05-01 instead of yyyy-04-30
      #      idate: yyyy-06-30 -> yyyy-03-02 instead of yyyy-02-28 (or 29 for leap years)
      tmpDate      <- hydrogfdei.endDate
      tmpDate$mday <- 15

      if (mday_as_num < 10) {
        tmpDate$mon <- tmpDate$mon - 5 # 4+1 due to round off to end of month below
      }else {
        tmpDate$mon <- tmpDate$mon - 4
      }
      hydrogfdei.endDate <- tmpDate

      # For shorter hindcast period lengths
      # E.g. idate: 2019-10-05, hc per len 123 => hc start: 2019-06-01, hc end: 2019-05-05 !
      if (hydrogfdei.endDate < hydrogfdei.startDate) {
          hydrogfdei.endDate$mon <- hydrogfdei.endDate$mon + 1
      }

      # Round off to end of this month
      mon_as_char  <- strftime(hydrogfdei.endDate,format="%m")
      mon_as_num   <- as.numeric(mon_as_char)
      year_as_char <- strftime(hydrogfdei.endDate,format="%Y")
      year_as_num  <- as.numeric(year_as_char)

      mday_last <- days_per_month(year_as_num,mon_as_num)
      hydrogfdei.endDate$mday <- hydrogfdei.endDate$mday + (mday_last - hydrogfdei.endDate$mday)
    }

    if (stateFileCreation) {
      # Same date as hindcast.endDate
      hydrogfdei.endDate <- forecastIssueDate
    }

    cmn.log("HydroGFDEI:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hydrogfdei.startDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hydrogfdei.endDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

    output <- list("hydrogfdei.startDate"=hydrogfdei.startDate,
                   "hydrogfdei.endDate"=hydrogfdei.endDate)

    return (output)
} # determine_interval_hydrogfdei


# In/Out - objects of posix date classes
determine_interval_hydrogfdod <- function(hydrogfdeiEndDate,
                                          forecastIssueDate,
                                          in_reforecast,
                                          in_url,
                                          in_query)
{
    hydrogfdod.startDate      <- hydrogfdeiEndDate
    hydrogfdod.startDate$mday <- hydrogfdod.startDate$mday + 1 # day

    if (in_reforecast == FALSE) {
      # Operational
      res <- search_and_locate_latest_date(in_url,in_query)
      if (res$status == 0) {
          hydrogfdod.endDate <- res$date
          hydrogfdod.endDate <- as.Date(hydrogfdod.endDate)
          hydrogfdod.endDate <- as.POSIXlt(hydrogfdod.endDate)
          if (hydrogfdod.endDate < hydrogfdod.startDate) {
              cmn.log(paste("Aborting, the latest hydrogfdod netcdf file date preceeds hydrogfdod start date: ",hydrogfdod.endDate), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
              q(save="no", status = 3)
          }
      }else{
          cmn.log("Aborting, not able to locate the latest hydrogfdod netcdf file", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
          q(save="no", status = 3)
      }

      diffDays <- forecastIssueDate - hydrogfdod.endDate
      if (diffDays < 2){
          cmn.log("Aborting, the od daily hindcast period will be to short when using a historical forecast issue date", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
          q(save="no", status = 3)
      }

    }else {
      # Re-forecast
      hydrogfdod.endDate <- forecastIssueDate

      mday_as_char <- strftime(hydrogfdod.endDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)

      # Handle case for month day 31 (and 29, 30), see determine_interval_hydrogfdei()
      tmpDate      <- hydrogfdod.endDate
      tmpDate$mday <- 15

      if (mday_as_num < 10) {
        tmpDate$mon <- tmpDate$mon - 2
      }else {
        tmpDate$mon <- tmpDate$mon - 1
      }
      hydrogfdod.endDate <- tmpDate

      # Round off to end of this month
      mon_as_char <- strftime(hydrogfdod.endDate,format="%m")
      mon_as_num  <- as.numeric(mon_as_char)
      year_as_char <- strftime(hydrogfdod.endDate,format="%Y")
      year_as_num  <- as.numeric(year_as_char)

      mday_last <- days_per_month(year_as_num,mon_as_num)
      hydrogfdod.endDate$mday <- hydrogfdod.endDate$mday + (mday_last - hydrogfdod.endDate$mday)
    }

    cmn.log("HydroGFDOD:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hydrogfdod.startDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(hydrogfdod.endDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

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

    cmn.log("OD:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(od.startDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(od.endDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

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

    cmn.log("ECOPER:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(ecoper.startDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    cmn.log(ecoper.endDate, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

    output <- list("ecoper.startDate"=ecoper.startDate,
                   "ecoper.endDate"=ecoper.endDate)

    return (output)
} # determine_interval_ecoper


prepare_hindcast_intervals <- function(in_hindcastDays, # positive integer
                                       in_forecastIssueDate, # character string with dashes
                                       in_reforecast = TRUE,
                                       in_stateFileCreation = FALSE,
                                       in_meteoHindcastType, # Selected meteo hindcast type at run time, for locating state file
                                       in_statefileHindcastDate, # Hindcast date in state file, for locating state file
                                       in_modelConfig, # url and query to locate netcdf files
                                       in_modelDataConfig) # paths to local dirs with state files etc.
{
    # Dates internally in function uses class posixlt (list)

    ## ------------------------------------------------------------------------------
    # Handle inputs
    if (verbose == TRUE) {
      print('Handle inputs:')
      print('in_hindcastDays:')
      print(in_hindcastDays)
      print('in_forecastIssueDate:')
      print(in_forecastIssueDate)
      print('in_reforecast:')
      print(in_reforecast)
      print('in_stateFileCreation:')
      print(in_stateFileCreation)
      print('in_meteoHindcastType:')
      print(in_meteoHindcastType)
      print('in_statefileHindcastDate:')
      print(in_statefileHindcastDate)
      print('')
    }

    hindcast.Days <- as.numeric(in_hindcastDays)
    if (hindcast.Days < cLimitHindcastPeriodDays) {
        cmn.log("Hindcast period length to short", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
        q(save="no", status=1)
    }

    # Convert to posix date format
    forecast.IssueDate <- as.Date(in_forecastIssueDate)
    forecast.IssueDate <- as.POSIXlt(forecast.IssueDate)

    if (in_stateFileCreation) {
      cmn.log("------------------------------------------------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("Run type mode is 'state file creation'.", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("Only using HydroGFDEI as meteo forcing data", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("for the hindcast run.", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("Ignore time intervals for HydroGFDOD and OD.", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("Forecast run will not be performed.", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      cmn.log("------------------------------------------------", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
    }

    ## ------------------------------------------------------------------------------
    # Hindcast general
    intervalHindcast <- determine_interval_hindcast(forecast.IssueDate,
                                                    hindcast.Days,
                                                    in_stateFileCreation,
                                                    in_meteoHindcastType,
                                                    in_statefileHindcastDate,
                                                    pathStateFiles=paste0(in_modelDataConfig,"/statefiles"))

    ## ------------------------------------------------------------------------------
    # HydroGFDEI (monthly)
    intervalHydrogfdei <- determine_interval_hydrogfdei(intervalHindcast$hindcast.startDate,
                                                        forecast.IssueDate,
                                                        in_reforecast,
                                                        in_stateFileCreation,
                                                        in_modelConfig$gfdHydrogfdeiUrl,   # Operational
                                                        in_modelConfig$gfdHydrogfdeiQuery) # Operational

    ## ------------------------------------------------------------------------------
    # HydroGFDOD (monthly)
    intervalHydrogfdod <- determine_interval_hydrogfdod(intervalHydrogfdei$hydrogfdei.endDate,
                                                        forecast.IssueDate,
                                                        in_reforecast,
                                                        in_modelConfig$gfdHydrogfdodUrl,   # Operational
                                                        in_modelConfig$gfdHydrogfdodQuery) # Operational

    ## ------------------------------------------------------------------------------
    # OD (daily)
    intervalOdDaily <- determine_interval_od_daily(intervalHydrogfdod$hydrogfdod.endDate,
                                                   intervalHindcast$hindcast.endDate)

    ## ------------------------------------------------------------------------------
    # Do not return the posixlt class objects
    # Convert data to character strings in the format yyyymm or yyyymmdd

    if (verboseVerbose == TRUE) {
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
                          "odEndDateFilename"=odEndDateFilename,

                          "stateFile"=intervalHindcast$stateFile
                          )
    return(castIntervals)

} # prepare_hindcast_intervals


prepare_forecast_intervals <- function(in_forecastIssueDate) # character string with dashes
{
    # Convert to posix date format
    forecast.IssueDate <- as.Date(in_forecastIssueDate)
    forecast.IssueDate <- as.POSIXlt(forecast.IssueDate)

    if (verboseVerbose == TRUE) {
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

    if (verboseVerbose == TRUE) {
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


# # Common
# search_and_download <- function(url,
#                                 query,
#                                 rootDir,
#                                 subDir)
# {
#   # Constants
#   osClientApp <- "opensearch-client"

#   local.dir <- paste(rootDir,subDir,sep="/")
#   if (! dir.exists(local.dir)){
#       dir.create(local.dir)
#   }
#   opensearchCmd=paste0(osClientApp," '",url,query,"'"," enclosure")
#   message(opensearchCmd)
#   res_enclosure <- system(command = opensearchCmd,intern = T)
#   if (length(res_enclosure >= 1)) {
#       for (xUrl in 1:length(res_enclosure)) {
#           res_file <- rciop.copy(res_enclosure[xUrl],local.dir)
#           if (res_file$exit.code == 0) {
#               path_plus_filename <- res_file$output
#               # Not used, future?
#           }else {
#               # file was already available in dir (re-downloaded) - $status = character(0)
#               if (length(res_file$output) == 0) {
#                   cmn.log(paste0("File with unknown name are already available in the local dir",local.netcdfDir), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
#               }
#           }
#       }
#   }else {
#       cmn.log(paste("No search result for: ",opensearchCmd), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
#   }

# } # search_and_download


# # Common
# search_and_download_netcdf <- function(urlNC,
#                                        query,
#                                        startDate,
#                                        stopDate,
#                                        ncRootDir,
#                                        ncSubDir)
# {
#   # Constants
#   osClientApp <- "opensearch-client"
#   secStartDay <- "T00:00:01"
#   secEndDay   <- "T23:59:59"
#   variables   <- c("pr","tas","tasmin","tasmax")

#   for (var in 1:length(variables)){
#       if (ncSubDir == TRUE){
#           local.netcdfDir <- paste(ncRootDir,variables[var],sep="/")
#           # Append query with variable
#           varQuery <- paste0(',',variables[var],']')
#           catQuery <- gsub(']',varQuery,query)
#       }else{
#           local.netcdfDir <- ncRootDir
#           # All files into one dir
#           catQuery <- query
#       }
#       if (! dir.exists(local.netcdfDir)) { dir.create(local.netcdfDir) }
#       opensearchCmd=paste0(osClientApp," '",urlNC,"'"," -p ","'count=unlimited'"," -p ","'cat=",catQuery,"'"," -p ","'start=",startDate,secStartDay,"'"," -p ","'stop=",stopDate,secEndDay,"'"," enclosure")
#       message(opensearchCmd)
#       res_enclosure <- system(command = opensearchCmd,intern = T)
#       if (length(res_enclosure >= 1)) {
#           for (xUrl in 1:length(res_enclosure)) {
#               res_file <- rciop.copy(res_enclosure[xUrl],local.netcdfDir)
#               if (res_file$exit.code == 0) {
#                   path_plus_filename <- res_file$output
#                   # Not used, future?
#               }else {
#                   # file was already available in dir (re-downloaded) - $status = character(0)
#                   if (length(res_file$output) == 0) {
#                       cmn.log(paste0("File with unknown name are already available in the local dir",local.netcdfDir), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
#                   }
#               }
#           }
#       }else {
#           cmn.log(paste("No search result for: ",opensearchCmd), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
#       }
      
#       if (ncSubDir == FALSE){
#           if (var == 1){
#             break # for loop
#           }
#       }
#   }

# } # search_and_download_netcdf


# # Common
# check_date_interval_netcdf <- function(startDate,
#                                        endDate,
#                                        ncRootDir,
#                                        ncSubDir,
#                                        filePrefix,
#                                        fileSuffix)
# {
#   #tasmin_hydrogfdei_201906_fanfar_SMHI.nc
#   #tasmin_hydrogfdod_201908_fanfar_SMHI.nc
#   #tas_od-daily_20190917_fanfar_SMHI.nc
#   #tasmin_ecoper_2019091800_fanfar_SMHI.nc # 00_fanfar_SMHI.nc

#   # Constants
#   variables <- c("pr","tas","tasmin","tasmax")
  
#   # Outputs
#   nMissingFiles <- 0

#   interval <- NULL
#   dateFormat <- NULL
#   if (grepl("hydrogfdei",filePrefix,fixed=TRUE)) {
#       interval <- "month"
#       dateFormat <- "%Y%m"
#   } else if (grepl("hydrogfdod",filePrefix,fixed=TRUE)) {
#       interval <- "month"
#       dateFormat <- "%Y%m"
#   } else if (grepl("od-daily",filePrefix,fixed=TRUE)) {
#       interval <- "day"
#       dateFormat <- "%Y%m%d"
#   } else if (grepl("ecoper",filePrefix,fixed=TRUE)) {
#       interval <- "day"
#       dateFormat <- "%Y%m%d"
#   }

#   sDate <- as.Date(startDate)
#   eDate <- as.Date(endDate)

#   # List of date objects. Assumes end date later than start date
#   seqTimeStamps <- seq.Date(from=sDate,to=eDate,by=interval)

#   for (month_or_day in 1:length(seqTimeStamps)){
#       fileDate <- strftime(seqTimeStamps[month_or_day],format=dateFormat)
#       for (var in 1:length(variables)){
#           if (ncSubDir == TRUE){
#               local.netcdfDir <- paste(ncRootDir,variables[var],sep="/")
#           }else{
#               # All files into one dir
#               local.netcdfDir <- ncRootDir
#           }

#           expFilename <- paste0(variables[var],filePrefix,fileDate,fileSuffix)
#           localPath   <- paste(local.netcdfDir,expFilename,sep="/")
#           if (! file.exists(localPath)){
#               nMissingFiles <- nMissingFiles + 1
#               cmn.log(paste0("File missing: ",localPath), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
#           }

#           test_nc_open_file <- FALSE #TRUE
#           if (test_nc_open_file == TRUE){
#               if (expFilename == "pr_hydrogfdei_201906_fanfar_SMHI.nc"){
#                 print("Trying to nc_open file: pr_hydrogfdei_201906_fanfar_SMHI.nc")
#                 print(localPath)
#                 ncfh = nc_open(localPath)
#                 print("after nc_open")
#                 print(ncfh)

#                 print("Trying to close file")
#                 nc_close(ncfh)
#                 print("after nc_close")
#               }
#           }
#       }
#   }

#   return (nMissingFiles)
# } # check_date_interval_netcdf


# Common
# This handles the sequence for each x obs  file (iterates opensearch, download each meterological GFD, call function to convert to obs)
# Retrieve files (pr,tas,tasmin,tasmax) via opensearch and rciop.copy
# Configurable option to download all files into one dir or individual sub-dirs
# for pr, tas, tasmin and tasmax.
# Returns the number of missing files
download_netcdf_hgfd2 <- function(modelConfig,    # sub-dir to use for local download dir, url, query pattern etc
                                  xCastsInterval, # dates for search start/stop, expected date in filename to check against
                                  netcdfDir,      # base dir to download files too
                                  ncSubDir,       # False-one dir, True-separate dir for each variable
                                  xCast=NULL,     # hindcast, forecast, elevation
                                  stateFileCreation=FALSE) # for hindcast
{
  if (verboseVerbose == TRUE) {
    print("download_netcdf_hgfd2:")
    print(modelConfig)
    print(xCastsInterval)
    print(xCast)
    print(stateFileCreation)
  }

  # Constants
  fileSuffix <- "_fanfar_SMHI.nc"

  # Outputs
  nMissingFiles <- 0

  if (! dir.exists(netcdfDir)){
    dir.create(netcdfDir)
  }

  nMissing <- 0
  if (xCast == "hindcast"){
      ## ------------------------------------------------------------------------------
      # Handle hydrogfdei
      urlNC     <- modelConfig$gfdHydrogfdeiUrl
      query     <- modelConfig$gfdHydrogfdeiQuery
      startDate <- xCastsInterval$hydrogfdeiStartDateSearch
      stopDate  <- xCastsInterval$hydrogfdeiEndDateSearch

      process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                     "_hydrogfdei_",fileSuffix)
      if (nMissing > 0){
          cmn.log(paste0(nMissing," file(s) missing for hydrogfdei"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      }
      nMissingFiles <- nMissingFiles + nMissing

      if (! stateFileCreation){
            ## ------------------------------------------------------------------------------
            # Handle hydrogfdod
            urlNC     <- modelConfig$gfdHydrogfdodUrl
            query     <- modelConfig$gfdHydrogfdodQuery
            startDate <- xCastsInterval$hydrogfdodStartDateSearch
            stopDate  <- xCastsInterval$hydrogfdodEndDateSearch

            process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
            # Check retrieved filenames
            nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                           "_hydrogfdod_",fileSuffix)
            if (nMissing > 0){
                cmn.log(paste0(nMissing," file(s) missing for hydrogfdod"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
            }
            nMissingFiles <- nMissingFiles + nMissing

            ## ------------------------------------------------------------------------------
            # Handle od (od-daily)
            urlNC     <- modelConfig$gfdOdDailyUrl
            query     <- modelConfig$gfdOdDailyQuery
            startDate <- xCastsInterval$odStartDateSearch
            stopDate  <- xCastsInterval$odEndDateSearch

            process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
            nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                           "_od-daily_",fileSuffix)
            if (nMissing > 0){
                cmn.log(paste0(nMissing," file(s) missing for od-daily"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
            }
            nMissingFiles <- nMissingFiles + nMissing
      } # stateFileCreation
  } # hindcast
  if (xCast == "forecast"){
      ## ------------------------------------------------------------------------------
      # Handle ecoper
      urlNC     <- modelConfig$gfdEcoperUrl
      query     <- modelConfig$gfdEcoperQuery
      startDate <- xCastsInterval$ecoperStartDateSearch
      stopDate  <- xCastsInterval$ecoperStartDateSearch # 1 file

      process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
      nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                     "_ecoper_",paste0("00",fileSuffix))
      if (nMissing > 0){
          cmn.log(paste0(nMissing," file(s) missing for ecoper"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      }
      nMissingFiles <- nMissingFiles + nMissing
  } # forecast
  if (xCast == "elevation"){
      ## ------------------------------------------------------------------------------
      # Handle elevation
      url       <- modelConfig$gfdElevationUrl
      query     <- modelConfig$gfdElevationQuery
      subDir    <- modelConfig$gfdElevationSubDir
      #startDate <- xCastsInterval$ecoperStartDateSearch
      #stopDate  <- xCastsInterval$ecoperEndDateSearch

      process_search_and_download(url,query,netcdfDir,subDir)
      expFilename <- paste(netcdfDir,subDir,"HydroGFD2elevation.nc",sep="/")
      if (! file.exists(expFilename)){
          nMissing <- 1
          cmn.log(paste0("Missing file: ",expFilename), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      }
      nMissingFiles <- nMissingFiles + nMissing
  } # elevation
  if (xCast == "grid.meta"){
      ## ------------------------------------------------------------------------------
      # Handle grid meta shape files for point and polygon
      url       <- modelConfig$gfdGridUrl
      query     <- modelConfig$gfdGridQuery
      subDir    <- modelConfig$gfdGridSubDir
      #startDate <- xCastsInterval$ecoperStartDateSearch
      #stopDate  <- xCastsInterval$ecoperEndDateSearch

      process_search_and_download(url,query,netcdfDir,subDir) # In dir for netcdf?
      #expFilename <- paste(netcdfDir,subDir,"HydroGFD2elevation.nc",sep="/")
      #if (! file.exists(expFilename)){
      #    nMissing <- 1
      #    cmn.log(paste0("Missing file: ",expFilename), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      #}
      #nMissingFiles <- nMissingFiles + nMissing
  } # elevation
  ## ------------------------------------------------------------------------------
  # All meterological gfd parts downloaded to local tmp dir

  return (nMissingFiles)
} # download_netcdf_hgfd2


# External function
# Wrapper for netcdf2obs sequence
process_forcing_hydrogfd2_hindcast <- function(modelConfig, # Misc config data, urls to search for hydrogfd data etc.
                                               modelFiles, # Misc model config data, paths to state files, forcing, shape files
                                               forecastIssueDate, # yyyy-mm-dd
                                               hindcastPeriodLength, # Days
                                               reforecast, # True - run mode reforecast, False - run mode operational
                                               stateFileCreation, # True - run mode 'Statefile creation'
                                               meteoHindcastType, # Selected meteo hindcast type at run time, for locating state file
                                               statefileHindcastDate, # Hindcast date in state file, for locating state file
                                               configGridLinkFilename, # Name of grid link file from configuration
                                               netcdfDir, # Input dir with hydrogfd netcdf files
                                               ncSubDir, # False-one dir, True-separate dir for each variable
                                               modelFilesRunDir, # HYPE model data files dir
                                               obsDir, # Output dir for obs files
                                               dirNetcdfToObsTmp, # Temporary work dir for netcdf_to_obs
                                               debugPublishFiles=FALSE) # Condition to publish files during development
{
  # Prepare hindcast and forecast intervals, start and end dates
  prepHindcastInterval <- prepare_hindcast_intervals(hindcastPeriodLength,
                                                     forecastIssueDate,
                                                     reforecast,
                                                     stateFileCreation,
                                                     meteoHindcastType,
                                                     statefileHindcastDate,
                                                     modelConfig,
                                                     modelFiles)

  # Download grid meta shape files for point and polygon
  nMissingFiles <- download_netcdf_hgfd2(modelConfig,
                                         xCastsInterval = prepHindcastInterval,
                                         netcdfDir,
                                         ncSubDir,
                                         xCast="grid.meta",
                                         stateFileCreation=FALSE)
  if (nMissingFiles > 0) {
      cmn.log("Aborting due to missing HydroGFD 2 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
      q(save="no", status = 2)
  }

  # Download hydrogfd elevation netcdf file
  nMissingFiles <- download_netcdf_hgfd2(modelConfig,
                                         xCastsInterval = prepHindcastInterval,
                                         netcdfDir,
                                         ncSubDir,
                                         xCast="elevation",
                                         stateFileCreation=FALSE)
  if (nMissingFiles > 0) {
      cmn.log("Aborting due to missing HydroGFD 2 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
      q(save="no", status = 2)
  }

  # For the calculated time interval, search and download hydrogfd netcdf files
  nMissingFiles <- download_netcdf_hgfd2(modelConfig,
                                         xCastsInterval = prepHindcastInterval,
                                         netcdfDir,
                                         ncSubDir,
                                         xCast="hindcast",
                                         stateFileCreation)
  if (nMissingFiles > 0) {
      cmn.log("Aborting due to missing HydroGFD 2 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
      q(save="no", status = 2)
  }

  # Produce the files Pobs.txt, Tobs.txt, TMINobs.txt and TMAXobs.txt
  startDate <- prepHindcastInterval$hindcastStartDateSearch
  endDate   <- prepHindcastInterval$hindcastEndDateSearch

  gridLinkFile       <- paste0(obsDir,"/gridLink.Rdata") # Used by netcdf_to_obs functions
  configGridLinkFile <- "no-filename"
  if (! is.null(configGridLinkFilename)) {
      configGridLinkFile <- paste0(modelFiles,"/shapefiles/",configGridLinkFilename)
  }

  if (file.exists(configGridLinkFile)) {
      if (! dir.exists(obsDir)){
          dir.create(obsDir)
      }
      file.copy(from=configGridLinkFile,to=gridLinkFile,overwrite=TRUE)
      cmn.log(paste0("cp ",configGridLinkFile," to ",gridLinkFile,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  }

  if (! file.exists(gridLinkFile)) {
      res <- run_netcdf_to_obs_gridLinkPreparation(workDir=dirNetcdfToObsTmp,
                                                   ncRootDir=netcdfDir,
                                                   ncSubDir=ncSubDir,
                                                   resourceDir=paste(netcdfDir,modelConfig$gfdGridSubDir,"grid.meta","grid.meta",sep="/"),
                                                   gridElevPath=paste(netcdfDir,modelConfig$gfdElevationSubDir,"HydroGFD2elevation.nc",sep="/"),
                                                   shapeFilePath=paste(modelFiles,"subidshapefile","SUBID_shapefile.shp", sep="/"),
                                                   outPath=obsDir, # Should maybe be a temporary dir for next step, but need to be available for the corresponding functional call during forecast
                                                   startDate,
                                                   endDate)
      if (file.exists(gridLinkFile) && debugPublishFiles) {
          # Publish to let user access the file and later add file to a configuration object
          rciop.publish(path=gridLinkFile,recursive=FALSE,metalink=TRUE)
      }

      if (res > 0){
          cmn.log(paste0("run_netcdf_to_obs_gridLinkPreparation, exit code=",res), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
      }
  }

  res <- prepare_and_run_netcdf_to_obs(workDir=dirNetcdfToObsTmp,
                                       ncRootDir=netcdfDir,
                                       ncSubDir=ncSubDir,
                                       outPath=obsDir,
                                       startDate,
                                       endDate)
  if (res > 0){
      cmn.log(paste0("prepare_and_run_netcdf_to_obs, exit code=",res), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  }

  if (verboseVerbose == TRUE) {
      # List files in output dir
      print(list.files(obsDir))
  }

  # Return the same type structure as function getModelForcing(), but minimal
  bdate <- as.Date(startDate)
  edate <- as.Date(endDate)

  if (! stateFileCreation){
    # Standard
    # cdate = edate - 130 days:
    calc.date <- as.POSIXlt(edate)
    calc.date$mday <- calc.date$mday - cLimitHindcastPeriodDays
    cdate <- as.Date(calc.date)
  }else{
    # State file creation
    cdate <- bdate
  }
  # Limit, minimum
  if (cdate < bdate) {
      cdate <- bdate
  }
  cmn.log(paste0("bdate: ",bdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  cmn.log(paste0("cdate: ",cdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  cmn.log(paste0("edate: ",edate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

  # Copy produced files to HYPE run dir
  process_copy_obs_files(fromDir=obsDir,
                         toDir=modelFilesRunDir,
                         publishFiles=debugPublishFiles,
                         textFilename='netcdf-to-obs-hgfd2-hindcast')
  # Copy state file to run dir
  dstFile <- NULL
  if(! is.null(prepHindcastInterval$stateFile)) {
      requestedFileNameSuffix <- get_requested_statefile_suffix(meteoHindcastType,
                                                                NULL,
                                                                statefileHindcastDate)

      stateFileWithoutSuffix <- gsub(pattern=requestedFileNameSuffix,replacement="",prepHindcastInterval$stateFile)

      srcFile <- paste0(modelFiles,"/statefiles/",prepHindcastInterval$stateFile)
      dstFile <- paste0(modelFilesRunDir,"/",stateFileWithoutSuffix)

      if(file.exists(srcFile)) {
          file.copy(from=srcFile,to=dstFile,overwrite=TRUE)
          cmn.log(paste0("cp ",srcFile," to ",dstFile), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
          #print(list.files(modelFilesRunDir))
      }else{
          dstFile <- NULL
          cmn.log("problem copying state file for upcoming hindcast run", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
      }
  }

  hindcast.forcing <- list("bdate"=bdate,
                           "cdate"=cdate,
                           "edate"=edate,
                           #"stateFile"=locatedStateFile) prepHindcastInterval$pathStateFile # No path
                           "stateFile"=dstFile)
  return (hindcast.forcing)

} # process_forcing_hydrogfd2_hindcast


# External function
# Wrapper for netcdf2obs sequence
process_forcing_hydrogfd2_forecast <- function(modelConfig, # Misc config data, now
                                               #modelFiles, # Misc model config data, paths to state files, forcing, shape files
                                               forecastIssueDate, # yyyy-mm-dd
                                               netcdfDir, # Input dir with hydrogfd netcdf files
                                               ncSubDir, # False-one dir, True-separate dir for each variable
                                               modelFilesRunDir = NULL, # HYPE model data files
                                               obsDir, # Output dir for obs files
                                               dirNetcdfToObsTmp, # Temporary work dir for netcdf_to_obs
                                               debugPublishFiles=FALSE) # Condition to publish files during development
{
  # Prepare hindcast and forecast intervals, start and end dates
  prepForecastInterval <- prepare_forecast_intervals(forecastIssueDate)

  # For the calculated time interval, search and download hydrogfd netcdf files
  nMissingFiles <- download_netcdf_hgfd2(modelConfig,
                                         xCastsInterval = prepForecastInterval, # Now different types... either have one type only...
                                         netcdfDir,
                                         ncSubDir,
                                         xCast="forecast",
                                         stateFileCreation=FALSE)
  if (nMissingFiles > 0) {
      cmn.log("Aborting due to missing HydroGFD 2 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF2)
      q(save="no", status = 2)
  }

  # Produce the files Pobs.txt, Tobs.txt, TMINobs.txt and TMAXobs.txt
  startDate <- prepForecastInterval$ecoperStartDateSearch
  endDate   <- prepForecastInterval$ecoperEndDateSearch

  res <- prepare_and_run_netcdf_to_obs(workDir=dirNetcdfToObsTmp,
                                       ncRootDir=netcdfDir,
                                       ncSubDir=ncSubDir,
                                       outPath=obsDir,
                                       startDate,
                                       endDate)
  if (res > 0){
      cmn.log(paste0("prepare_and_run_netcdf_to_obs, exit code=",res), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  }

  if (verboseVerbose == TRUE) {
      # List files in output dir
      print(list.files(obsDir))
  }

  # Return the same type structure as function getModelForcing(), but minimal
  bdate <- as.Date(startDate)
  cdate <- as.Date(startDate)
  edate <- as.Date(endDate)
  cmn.log(paste0("bdate: ",bdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  cmn.log(paste0("cdate: ",cdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)
  cmn.log(paste0("edate: ",edate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF2)

  # Copy produced files to HYPE run dir
  process_copy_obs_files(fromDir=obsDir,
                         toDir=modelFilesRunDir,
                         publishFiles=debugPublishFiles,
                         textFilename='netcdf-to-obs-hgfd2-forecast')

  forecast.forcing <- list("bdate"=bdate,
                           "cdate"=cdate,
                           "edate"=edate)
  return (forecast.forcing)

} # process_forcing_hydrogfd2_forecast
