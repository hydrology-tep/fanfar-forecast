#! /usr/bin/Rscript

# Constants
verbose <- TRUE
#verbose <- FALSE

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
  if (verbose == TRUE) {
    print('days_per_month: year month lastdayofmonth')
    print(year)
    print(month)
    print(day)
  }
  return (day)
} 


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


# This handles the sequence for each x obs  file (iterates opensearch, download each meterological GFD, call function to convert to obs) 
convert_netcdf2obs <- function(modelConfig,    # sub-dir to use for local download dir, url, query pattern etc 
                               xCastsInterval, # dates for search start/stop, expected date in filename to check against
                               netcdfDir)      # base dir to download files too
{
  # Other inputs are:
  # path_to_store_netcdf_files - temporary dir
  # path to store obs files - res dir
  # expected file name for retrived netcdf files - to compare with after download

  # Queries of start/stop date for opensearch may need dash, or may not
  # Our hydrogfd filenames contains no dashes
  
  if (verbose == TRUE) {
    print("convert_netcdf2obs func input:")
    print(modelConfig)
    print(xCastsInterval)
  }
  
  # Constants
  osClientApp <- "opensearch-client"
  secStartDay <- "T00:00:01"
  secEndDay   <- "T23:59:59"
  variables = c("pr", "tas", "tasmin", "tasmax")
  
  ## ------------------------------------------------------------------------------
  # Handle hydrogfdei
  
  # Retrieve files (pr,tas,tasmin,tasmax) via opensearch and rciop.copy
  # Either one file at a time with count=unlimited or all 4 via count=4
  # Choose count 4 for now

  
  # ToDo: Add opensearch code from test code...
  print("Test: Download hydrogfdei netcdf files...")  

    
  #
  # Search
  #
  
  # ToDo: TMPDIR
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdei]' -p 'start=2019-02-07T00:00:01' -p 'stop=2019-02-07T23:59:59' enclosure") # ok, files for 201902 (pr,tas,tasmin,tasmax)
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdei]' -p 'start=2019-01-01T00:00:01' -p 'stop=2019-01-01T23:59:59' enclosure") # ok, files for 201901 (pr,tas,tasmin,tasmax)
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,pr]' -p 'start=2018-01-01T00:00:01' -p 'stop=2018-01-31T23:59:59' enclosure") # ok
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tas]' -p 'start=2017-01-01T00:00:01' -p 'stop=2017-01-31T23:59:59' enclosure") # ok
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tasmin]' -p 'start=2017-11-01T00:00:01' -p 'stop=2017-11-30T23:59:59' enclosure") # ok
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,hydrogfdei,tasmax]' -p 'start=2016-12-01T00:00:01' -p 'stop=2016-12-31T23:59:59' enclosure") # ok
  # opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=unlimited' -p 'cat=[SMHI,operational,monthly,hydrogfdei,tasmax]' -p 'start=2016-12-01T00:00:01' -p 'stop=2016-12-31T23:59:59' enclosure") # ok without cat=monthly
  
  #tmp
  #opensearchCmd=paste("opensearch-client 'https://catalog.terradue.com/hydro-smhi/description' -p 'count=4' -p 'cat=[SMHI,operational,hydrogfdei]' -p 'start=2019-01-01T00:00:01' -p 'stop=2019-01-01T23:59:59' enclosure") # ok, files for 201901 (pr,tas,tasmin,tasmax)
  # ok...
  #tmp
  
  #opensearchCmd=paste0(osClientApp," '",modelConfig$gfdHydrogfdeiUrl,"'"," -p ","'count=4'"," -p ","'cat=",modelConfig$gfdHydrogfdeiQuery,"'"," -p ","'start=",xCastsInterval$hydrogfdeiStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$hydrogfdeiEndDateSearch,secEndDay,"'"," enclosure")
  opensearchCmd=paste0(osClientApp," '",modelConfig$gfdHydrogfdeiUrl,"'"," -p ","'count=unlimited'"," -p ","'cat=",modelConfig$gfdHydrogfdeiQuery,"'"," -p ","'start=",xCastsInterval$hydrogfdeiStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$hydrogfdeiEndDateSearch,secEndDay,"'"," enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  message(input_enclosure)
  if (length(input_enclosure >= 1)) {
    #nc_tmpdir <- paste(netcdfDir,"nc-files",sep="/")
    #if (! dir.exists(nc_tmpdir)) { dir.create(nc_tmpdir) }
    if (! dir.exists(netcdfDir)) { dir.create(netcdfDir) }
    for (url in 1:length(input_enclosure)) {
      #rciop.copy(input_enclosure[url],nc_tmpdir)
      rciop.copy(input_enclosure[url],netcdfDir)
    }
  }

  print("Test: Download complete hydrogfdei...")  

  #q(save = "no", 0)

  # Depending on start stop date interval, we may need to check filename for multiple dates
  # Or... yes
  # For hydrogfdei the filename date is yyyymm, no day
  
  # ToDo: loop start - stop date interval
  # Check retrieved filenames
  #metName <- hydrogfdei # ToDo: get from model config
  
  #dateInt <- (as.numeric(xCastsInterval$hydrogfdeiEndDateFilename) - as.numeric(xCastsInterval$hydrogfdeiStartDateFilename))
  #startDate <- as.numeric(xCastsInterval$hydrogfdeiStartDateFilename)
  #stopDate  <- as.numeric(xCastsInterval$hydrogfdeiEndDateFilename)
  #monthCtr <- 0 # May need to handle year change...
  
  #dateInt <- (as.numeric(xCastsInterval$hydrogfdeiEndDateFilename) - as.numeric(xCastsInterval$hydrogfdeiStartDateFilename))
  
  
  #for (d in )
  #date <- 201904 # from loop start-stop
  #for (v in variables){
  #  expFilename <- paste(v,metName,d,"fanfar_SMHI.nc",sep="_")
  #  
  #}
  
  # With expected filename, check if file exists in local download dir
  #status <- file.exists(expFilename)
  
  ## ------------------------------------------------------------------------------
  # Handle hydrogfdod
  
  #opensearchCmd=paste0(osClientApp," '",modelConfig$gfdHydrogfdodUrl,"'"," -p ","'count=4'"," -p ","'cat=",modelConfig$gfdHydrogfdodQuery,"'"," -p ","'start=",xCastsInterval$hydrogfdodStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$hydrogfdodEndDateSearch,secEndDay,"'"," enclosure")
  opensearchCmd=paste0(osClientApp," '",modelConfig$gfdHydrogfdodUrl,"'"," -p ","'count=unlimited'"," -p ","'cat=",modelConfig$gfdHydrogfdodQuery,"'"," -p ","'start=",xCastsInterval$hydrogfdodStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$hydrogfdodEndDateSearch,secEndDay,"'"," enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  message(input_enclosure)
  if (length(input_enclosure >= 1)) {
    #nc_tmpdir <- paste(netcdfDir,"nc-files",sep="/")
    #if (! dir.exists(nc_tmpdir)) { dir.create(nc_tmpdir) }
    if (! dir.exists(netcdfDir)) { dir.create(netcdfDir) }
    for (url in 1:length(input_enclosure)) {
      #rciop.copy(input_enclosure[url],nc_tmpdir)
      rciop.copy(input_enclosure[url],netcdfDir)
    }
  }
  
  # Check retrieved filenames
  
  ## ------------------------------------------------------------------------------
  # Handle od (od-daily)
  
  #opensearchCmd=paste0(osClientApp," '",modelConfig$gfdOdDailyUrl,"'"," -p ","'count=4'"," -p ","'cat=",modelConfig$gfdOdDailyQuery,"'"," -p ","'start=",xCastsInterval$odStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$odEndDateSearch,secEndDay,"'"," enclosure")
  opensearchCmd=paste0(osClientApp," '",modelConfig$gfdOdDailyUrl,"'"," -p ","'count=unlimited'"," -p ","'cat=",modelConfig$gfdOdDailyQuery,"'"," -p ","'start=",xCastsInterval$odStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$odEndDateSearch,secEndDay,"'"," enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  message(input_enclosure)
  if (length(input_enclosure >= 1)) {
    #nc_tmpdir <- paste(netcdfDir,"nc-files",sep="/")
    #if (! dir.exists(nc_tmpdir)) { dir.create(nc_tmpdir) }
    if (! dir.exists(netcdfDir)) { dir.create(netcdfDir) }
    for (url in 1:length(input_enclosure)) {
      #rciop.copy(input_enclosure[url],nc_tmpdir)
      rciop.copy(input_enclosure[url],netcdfDir)
    }
  }
  
  # Check retrieved filenames
  
  ## ------------------------------------------------------------------------------
  # Handle ecoper (ToDo: add 00 in expected filename)
  
  #opensearchCmd=paste0(osClientApp," '",modelConfig$gfdEcoperUrl,"'"," -p ","'count=4'"," -p ","'cat=",modelConfig$gfdEcoperQuery,"'"," -p ","'start=",xCastsInterval$ecoperStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$ecoperEndDateSearch,secEndDay,"'"," enclosure")
  opensearchCmd=paste0(osClientApp," '",modelConfig$gfdEcoperUrl,"'"," -p ","'count=unlimited'"," -p ","'cat=",modelConfig$gfdEcoperQuery,"'"," -p ","'start=",xCastsInterval$ecoperStartDateSearch,secStartDay,"'"," -p ","'stop=",xCastsInterval$ecoperEndDateSearch,secEndDay,"'"," enclosure")
  message(opensearchCmd)
  input_enclosure <- system(command = opensearchCmd,intern = T)
  message(input_enclosure)
  if (length(input_enclosure >= 1)) {
    #nc_tmpdir <- paste(netcdfDir,"nc-files",sep="/")
    #if (! dir.exists(nc_tmpdir)) { dir.create(nc_tmpdir) }
    if (! dir.exists(netcdfDir)) { dir.create(netcdfDir) }
    for (url in 1:length(input_enclosure)) {
      #rciop.copy(input_enclosure[url],nc_tmpdir)
      rciop.copy(input_enclosure[url],netcdfDir)
    }
  }
  
  # Check retrieved filenames

  ## ------------------------------------------------------------------------------
  # All meterological gfd parts downloaded to local tmp dir
  # Call Davids func with path etc. and path (run dir?) to let him store the result files
  # ToDo:
  
} # convert_netcdf2obs 
 

# Wrapper for netcdf2obs
#process_netcdf2obs <- function(modelConfig)
process_netcdf2obs <- function(modelConfig,
                               forecastIssueDate,
                               hindcastPeriodLength,
                               netcdfDir)
{

  # Select sep/oct due to latest EI/OD set hard when reforcast==false, 2019-06-30,2019-08-30
  
  # Fake model config data for tests
  #modelConfig <- NULL
  
  
  # Prepare hindcast and forecast intervals, start and end dates
  prepCastInterval <- prepare_casts_intervals(hindcastPeriodLength,
                                             forecastIssueDate,
                                             in_reforecast = TRUE,
                                             "19700101")
  print("Call convert_netcdf2obs")
  convert_netcdf2obs(modelConfig,
                     xCastsInterval = prepCastInterval,
                     netcdfDir)
  print("After convert_netcdf2obs")
  
  # Test data for above sequence:
  # out.prep2 <- prepare_casts_intervals(123, "2019-10-05", in_reforecast = TRUE, "20190608")
  # convert_netcdf2obs(xCastsInterval = out.prep2)
  # 
  # out.prep3 <- prepare_casts_intervals(123, "2019-10-28", in_reforecast = TRUE, "20170110") # 2019-10-19
  # convert_netcdf2obs(xCastsInterval = out.prep3)
  # 
  # out.prep4 <- prepare_casts_intervals(123, "2019-10-28", in_reforecast = TRUE, "20190608")
  # convert_netcdf2obs(xCastsInterval = out.prep4)
  # 
  # out.prep1 <- prepare_casts_intervals(123, "2019-10-05", in_reforecast = FALSE, "20170110")
  # convert_netcdf2obs(xCastsInterval = out.prep1)
  # 
  # out.prep2 <- prepare_casts_intervals(123, "2019-10-05", in_reforecast = FALSE, "20190608")
  # convert_netcdf2obs(xCastsInterval = out.prep2)
  # 
  # out.prep3 <- prepare_casts_intervals(123, "2019-10-28", in_reforecast = FALSE, "20170110")
  # convert_netcdf2obs(xCastsInterval = out.prep3)
  # 
  # out.prep4 <- prepare_casts_intervals(123, "2019-10-28", in_reforecast = FALSE, "20190608")
  # convert_netcdf2obs(xCastsInterval = out.prep4)
} # process_netcdf2obs

# External function
#process_netcdf2obs()
