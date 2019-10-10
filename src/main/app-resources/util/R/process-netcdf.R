#! /usr/bin/Rscript

# Constants
verbose <- TRUE
#verbose <- FALSE
#verboseX2 <- TRUE
verboseX2 <- FALSE

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


# ToDo: Split contents
prepare_casts_intervals <- function(in_hindcastDays, # positive integer
                                    in_forecastIssueDate, # character string with dashes
                                    in_reforecast = TRUE,
                                    in_hypeStateFileDate) # character string without dashes
  # Return start and end dates for all 4 meterological intervals.
{
    # Dates internally in function uses class posixlt (list)

    # Output variables
    # hydrogfdei.startDate <- NULL
    # hydrogfdei.endDate <- NULL
    # hydrogfdod.startDate <- NULL
    # hydrogfdod.endDate <- NULL
    # od.startDate <- NULL
    # od.endDate <- NULL
    # ecoper.startDate <- NULL
    # ecoper.endDate <- NULL

    # Local variables
    #hypeState.Date
    #forecast.IssueDate
    #potentialHindcastStart
    #hindcast.startDate

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
    # Hindcast start date
    hindcast.startDate      <- forecast.IssueDate
    hindcast.startDate$mday <- hindcast.startDate$mday - hindcast.Days

    # Base hindcast start date depeding on HYPE state file
    if (hindcast.startDate <= hypeState.Date) {
      # Earlier than HYPE state date
      hindcast.startDate      <- hypeState.Date
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

    if (verbose == TRUE) {
      print('Hindcast start date:')
      print(hindcast.startDate)
      print('')
    }
    
    ## ------------------------------------------------------------------------------
    # HydroGFDEI (monthly)
    hydrogfdei.startDate <- hindcast.startDate
    if (in_reforecast == FALSE) {
      # Operational

      # Should be: last day in the most recent hydrogfdei file (as new input parameter)
      # Now the last file is 201906
      hydrogfdei.endDate <- "2019-06-30"
      hydrogfdei.endDate <- as.Date(hydrogfdei.endDate)
      hydrogfdei.endDate <- as.POSIXlt(hydrogfdei.endDate)
    }else {
      hydrogfdei.endDate <- forecast.IssueDate
      
      mday_as_char <- strftime(hydrogfdei.endDate,format="%d")
      mday_as_num  <- as.numeric(mday_as_char)
      if (mday_as_num < 10) {
          hydrogfdei.endDate$mon <- hydrogfdei.endDate$mon - 4
      }else {
          hydrogfdei.endDate$mon <- hydrogfdei.endDate$mon - 3
      }
            
      # Round off to end of this month
      mon_as_char <- strftime(hydrogfdei.endDate,format="%m")
      mon_as_num  <- as.numeric(mon_as_char)
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
    
    ## ------------------------------------------------------------------------------
    # HydroGFDOD (monthly)
    hydrogfdod.startDate      <- hydrogfdei.endDate
    hydrogfdod.startDate$mday <- hydrogfdod.startDate$mday + 1 # day
    if (in_reforecast == FALSE) {
      # Operational
      
      # Should be: last day in the most recent hydrogfdod file (as new input parameter)
      # Now the last file is 201908
      hydrogfdod.endDate <- "2019-08-31"
      hydrogfdod.endDate <- as.Date(hydrogfdod.endDate)
      hydrogfdod.endDate <- as.POSIXlt(hydrogfdod.endDate)
    }else {
      hydrogfdod.endDate <- forecast.IssueDate

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
    
    ## ------------------------------------------------------------------------------
    # OD (daily)
    od.startDate      <- hydrogfdod.endDate
    od.startDate$mday <- hydrogfdod.endDate$mday + 1
    od.endDate        <- forecast.IssueDate
    od.endDate$mday   <- forecast.IssueDate$mday - 1 # Hindcast end date
    
    if (verbose == TRUE) {
      print('OD:')
      print(od.startDate)
      print(od.endDate)
      print('')
    }
    
    ## ------------------------------------------------------------------------------
    # ECOPER (daily)
    ecoper.startDate    <- forecast.IssueDate
    ecoper.endDate      <- forecast.IssueDate
    ecoper.endDate$mday <- forecast.IssueDate$mday + 9
    
    if (verbose == TRUE) {
      print('ECOPER:')
      print(ecoper.startDate)
      print(ecoper.endDate)
      print('')
    }
    
    ## ------------------------------------------------------------------------------
    # Do not return the posixlt class objects
    # Convert data to character strings in the format yyyymm or yyyymmdd
    # Queries of start/stop date for opensearch may need dash, or may not
    # Our hydrogfd filenames contains no dashes
    
    if (verbose == TRUE) {
      #tmp <- strftime(hydrogfdei.startDate,format="%Y-%m")
      tmp <- strftime(hydrogfdei.startDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(hydrogfdei.endDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(hydrogfdod.startDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(hydrogfdod.endDate,format="%Y%m")
      print(tmp)
      tmp <- strftime(od.startDate,format="%Y%m%d")
      print(tmp)
      tmp <- strftime(od.endDate,format="%Y%m%d")
      print(tmp)
      tmp <- strftime(ecoper.startDate,format="%Y%m%d")
      print(tmp)
      tmp <- strftime(ecoper.endDate,format="%Y%m%d")
      print(tmp)
    }

    hydrogfdeiStartDateSearch   <- strftime(hydrogfdei.startDate,format="%Y-%m-%d")
    hydrogfdeiEndDateSearch     <- strftime(hydrogfdei.endDate,format="%Y-%m-%d")
    hydrogfdeiStartDateFilename <- strftime(hydrogfdei.startDate,format="%Y%m")
    hydrogfdeiEndDateFilename   <- strftime(hydrogfdei.endDate,format="%Y%m")
    
    hydrogfdodStartDateSearch   <- strftime(hydrogfdod.startDate,format="%Y-%m-%d")
    hydrogfdodEndDateSearch     <- strftime(hydrogfdod.endDate,format="%Y-%m-%d")
    hydrogfdodStartDateFilename <- strftime(hydrogfdod.startDate,format="%Y%m")
    hydrogfdodEndDateFilename   <- strftime(hydrogfdod.endDate,format="%Y%m")
    
    odStartDateSearch   <- strftime(od.startDate,format="%Y-%m-%d")
    odEndDateSearch     <- strftime(od.endDate,format="%Y-%m-%d")
    odStartDateFilename <- strftime(od.startDate,format="%Y%m%d")
    odEndDateFilename   <- strftime(od.endDate,format="%Y%m%d")
    
    ecoperStartDateSearch   <- strftime(ecoper.startDate,format="%Y-%m-%d")
    ecoperEndDateSearch     <- strftime(ecoper.endDate,format="%Y-%m-%d")
    #ecoperStartDateFilename <- strftime(ecoper.startDate,format="%Y%m%d00") # Will create problem if necessary to convert to date for loop
    #ecoperEndDateFilename   <- strftime(ecoper.endDate,format="%Y%m%d00")
    ecoperStartDateFilename <- strftime(ecoper.startDate,format="%Y%m%d")
    ecoperEndDateFilename   <- strftime(ecoper.endDate,format="%Y%m%d")
    
    # Return dates as characters
    castIntervals <- list("hydrogfdeiStartDateSearch"=hydrogfdeiStartDateSearch,
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
                          "ecoperStartDateSearch"=ecoperStartDateSearch,
                          "ecoperEndDateSearch"=ecoperEndDateSearch,
                          "ecoperStartDateFilename"=ecoperStartDateFilename,
                          "ecoperEndDateFilename"=ecoperEndDateFilename
                          )
    return(castIntervals)
    
} # prepare_casts_intervals



# Check name of a retreived file
# Return TRUE when name match, else FALSE
# check_filename <- function(curFilename, # Name of file to check
#                            metName, # String hydrogfdei, hydrogfdod, od-daily, ecoper (same as model config sub-dir)
#                            variable, # String pr, tas, tasmin or tasmax
#                            date)#, # Expected date in filename, ecoper (00 already part of date for filename)
#                            #onlyPRTAS = FALSE) # When true, ignore tasmin/tasmax
# {
#   # if (onlyPRTAS == TRUE) {
#   #   variables = c("pr", "tas")
#   # }else {
#   #   variables = c("pr", "tas", "tasmin", "tasmax")
#   # }
#   # for (v in variables){
# 
#   expFilename <- paste(variable,metName,date,"fanfar_SMHI.nc",sep="_")
#   if (verbose == TRUE) {
#     print(curFilename)
#     print(expFilename)
#   }
#   
#   # ToDo: Does this work in R?
#   nameMatch <- (curFilename != expFilename)
#   nameMatch <- TRUE # ToDo: remove
#   
#   return (nameMatch)
# 
# }


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
          print(expFilename)
          if (! file.exists(paste(local.netcdfDir,expFilename,sep="/"))){
              nMissingFiles <- nMissingFiles + 1
              print(paste0("File missing: ", expFilename))
              print(paste0("File missing: ", paste(local.netcdfDir,expFilename,sep="/"))
          }
      }
  }

  return (nMissingFiles)
} # check_date_interval_netcdf


# This handles the sequence for each x obs  file (iterates opensearch, download each meterological GFD, call function to convert to obs) 
# Retrieve files (pr,tas,tasmin,tasmax) via opensearch and rciop.copy
# Configurable option to download all files into one dir or individual sub-dirs
# for pr, tas, tasmin and tasmax.
download_netcdf <- function(modelConfig,    # sub-dir to use for local download dir, url, query pattern etc 
                            xCastsInterval, # dates for search start/stop, expected date in filename to check against
                            netcdfDir)      # base dir to download files too
{
  # Other inputs are:
  # path_to_store_netcdf_files - temporary dir
  # path to store obs files - res dir
  # expected file name for retrived netcdf files - to compare with after download
  
  if (verbose == TRUE) {
    print("download_netcdf func input:")
    print(modelConfig)
    print(xCastsInterval)
  }
  
  # Constants
  #osClientApp <- "opensearch-client"
  #secStartDay <- "T00:00:01"
  #secEndDay   <- "T23:59:59"
  variables   <- c("pr","tas","tasmin","tasmax")
  #ncSubDir    <- FALSE
  ncSubDir    <- TRUE # False-one dir, True-separate dir for each variable
  fileSuffix <- "_fanfar_SMHI.nc"

  if (! dir.exists(netcdfDir)){
    dir.create(netcdfDir)
  }

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
      print(paste0(nMissingFiles," file(s) missing for hydrogfdei"))
      rciop.log("INFO",paste0(nMissingFiles," file(s) missing for hydrogfdei"))
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
      print(paste0(nMissingFiles," file(s) missing for hydrogfdod"))
      rciop.log("INFO",paste0(nMissingFiles," file(s) missing for hydrogfdod"))
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
      print(paste0(nMissingFiles," file(s) missing for od-daily"))
      rciop.log("INFO",paste0(nMissingFiles," file(s) missing for od-daily"))
  }
  
  ## ------------------------------------------------------------------------------
  # Handle ecoper (ToDo: add 00 in expected filename during check)
  urlNC     <- modelConfig$gfdEcoperUrl
  query     <- modelConfig$gfdEcoperQuery
  startDate <- xCastsInterval$ecoperStartDateSearch
  stopDate  <- xCastsInterval$ecoperEndDateSearch

  search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
  nMissing <- check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                         "_ecoper_",paste0("00",fileSuffix))
  if (nMissing > 0){
      print(paste0(nMissingFiles," file(s) missing for ecoper"))
      rciop.log("INFO",paste0(nMissingFiles," file(s) missing for ecoper"))
  }

  ## ------------------------------------------------------------------------------
  # All meterological gfd parts downloaded to local tmp dir
  
} # download_netcdf 
 

# External function
# Wrapper for netcdf2obs sequence
process_netcdf2obs <- function(modelConfig,
                               forecastIssueDate,
                               hindcastPeriodLength,
                               netcdfDir)
{
  # Prepare hindcast and forecast intervals, start and end dates
  prepCastInterval <- prepare_casts_intervals(hindcastPeriodLength,
                                              forecastIssueDate,
                                              in_reforecast = TRUE,
                                              "19700101") # ToDo: Get date from latest Hype state filename
  print("Call download_netcdf")
  download_netcdf(modelConfig,
                  xCastsInterval = prepCastInterval,
                  netcdfDir)
  print("After download_netcdf")
  
  # Call Davids func with path etc. and path (run dir?) to let him store the result files
  # ToDo:

  # For Tobs:

  # For Pobs:

  # For Qobs:


} # process_netcdf2obs

# External function
#process_netcdf2obs()
