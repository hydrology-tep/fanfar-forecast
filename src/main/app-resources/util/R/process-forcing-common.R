#
# Common forcing utililties
#

# Includes that the sourced R-files uses
library(ncdf4)

# Constants
nameOfSrcFile_PFC <- "/util/R/process-forcing-common.R"


# # Search for the latest item depending on url and query.
# # Returns a list with status and date as a character string. On error status = 1
# process_search_and_locate_latest_date <- function(url,
#                                                   query)
# {
#     # Constants
#     osClientApp <- "opensearch-client"

#     # Outputs
#     status   <- 1 # Not ok
#     yyyymmdd <- ""

#     # Latest, without specifying any date
#     opensearchCmd=paste0(osClientApp," '",url,"'"," -p ","'count=1'"," -p ","'cat=",query,"'"," enclosure")
#     message(opensearchCmd)
#     res_enclosure <- system(command = opensearchCmd,intern = T)
#     #Example res_enclosure:"https://store.terradue.com/hydro-smhi/fanfar/Operational/hydrogfdei/2019/07/tasmin_hydrogfdei_201907_fanfar_SMHI.nc"

#     if (nchar(res_enclosure > 0)) {
#         # Locate and extract date from filename in returned http url
#         # .*  - match any character
#         # ()  - capturing group with expected match for 6 single digits
#         # \\1 - reference to first captured pattern
#         yearmon <- gsub(pattern=".*([0-9]{6}).*",replace="\\1", res_enclosure)
#         if (nchar(yearmon) >= 6) { 
#             yyyy <- substr(yearmon,1,4)
#             mm   <- substr(yearmon,5,6)
#             dd   <- days_per_month(as.numeric(yyyy),as.numeric(mm))
        
#             yyyymmdd <- paste(yyyy,mm,dd,sep="-")
#             status   <- 0
#         }else{
#             paste("Could not locate date in filename")
#         }
#     }

#     output <- list("status"=status,
#                    "date"=yyyymmdd)

#     return (output)
# } # process_search_and_locate_latest_date


process_search_and_download <- function(url,
                                        query,
                                        rootDir,
                                        subDir)
{
  # Constants
  osClientApp <- "opensearch-client"

  local.dir <- paste(rootDir,subDir,sep="/")
  if (! dir.exists(local.dir)){
      dir.create(local.dir)
  }
  opensearchCmd=paste0(osClientApp," '",url,query,"'"," enclosure")
  message(opensearchCmd)
  res_enclosure <- system(command = opensearchCmd,intern = T)
  if (length(res_enclosure >= 1)) {
      for (xUrl in 1:length(res_enclosure)) {
          res_file <- rciop.copy(res_enclosure[xUrl],local.dir)
          if (res_file$exit.code == 0) {
              path_plus_filename <- res_file$output
              # Not used, future?
          }else {
              # file was already available in dir (re-downloaded) - $status = character(0)
              if (length(res_file$output) == 0) {
                  cmn.log(paste0("File with unknown name are already available in the local dir",local.netcdfDir), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
              }
          }
      }
  }else {
      cmn.log(paste("No search result for: ",opensearchCmd), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
  }

} # process_search_and_download


process_search_and_download_netcdf <- function(urlNC,
                                               query,
                                               startDate,
                                               stopDate,
                                               ncRootDir,
                                               ncSubDir)
{
  # Constants
  osClientApp <- "opensearch-client"
  secStartDay <- "T00:00:00"
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

      res_enclosure <- system(command = opensearchCmd,intern = T)
      if (length(res_enclosure >= 1)) {
          for (xUrl in 1:length(res_enclosure)) {
              res_file <- rciop.copy(res_enclosure[xUrl],local.netcdfDir)
              if (res_file$exit.code == 0) {
                  path_plus_filename <- res_file$output
                  # Not used, future?
              }else {
                  # file was already available in dir (re-downloaded) - $status = character(0)
                  if (length(res_file$output) == 0) {
                      cmn.log(paste0("File with unknown name are already available in the local dir",local.netcdfDir), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
                  }
              }
          }
      }else {
          cmn.log(paste("No search result for: ",opensearchCmd), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      }
      
      if (ncSubDir == FALSE){
          if (var == 1){
            break # for loop
          }
      }
  }

} # process_search_and_download_netcdf


process_check_date_interval_netcdf <- function(startDate,
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

  # Constants
  variables <- c("pr","tas","tasmin","tasmax")
  
  # Outputs
  nMissingFiles <- 0

  interval <- NULL
  dateFormat <- NULL

  # HydroGFD 2
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

  # HydroGFD 3
  } else if (grepl("he5tm",filePrefix,fixed=TRUE)) {
      interval <- "month"
      dateFormat <- "%Y%m"
  } else if (grepl("he5td",filePrefix,fixed=TRUE)) {
      interval <- "day"
      dateFormat <- "%Y%m%d"
  } else if (grepl("he5",filePrefix,fixed=TRUE)) {
      interval <- "month"
      dateFormat <- "%Y%m"
  } else if (grepl("odf",filePrefix,fixed=TRUE)) {
      interval <- "day"
      dateFormat <- "%Y%m%d"
  } else if (grepl("od",filePrefix,fixed=TRUE)) {
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
          localPath   <- paste(local.netcdfDir,expFilename,sep="/")
          if (! file.exists(localPath)){
              nMissingFiles <- nMissingFiles + 1
              cmn.log(paste0("File missing: ",localPath), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
          }

        #   test_nc_open_file <- FALSE #TRUE
        #   if (test_nc_open_file == TRUE){
        #       if (expFilename == "pr_hydrogfdei_201906_fanfar_SMHI.nc"){
        #         print("Trying to nc_open file: pr_hydrogfdei_201906_fanfar_SMHI.nc")
        #         print(localPath)
        #         ncfh = nc_open(localPath)
        #         print("after nc_open")
        #         print(ncfh)

        #         print("Trying to close file")
        #         nc_close(ncfh)
        #         print("after nc_close")
        #       }
        #   }
      }
  }

  return (nMissingFiles)
} # process_check_date_interval_netcdf


process_copy_obs_files <- function(fromDir, # Path to produced files
                                   toDir,   # Path to model run dir
                                   publishFiles=F,
                                   textFilename='netcdf-to-obs-hindcast')
{
  pobs    <- paste(fromDir,"Pobs.txt",sep="/")
  tobs    <- paste(fromDir,"Tobs.txt",sep="/")
  tminobs <- paste(fromDir,"TMINobs.txt",sep="/")
  tmaxobs <- paste(fromDir,"TMAXobs.txt",sep="/")
  forckey <- paste(fromDir,"ForcKey.txt",sep="/")
  qobs    <- paste(toDir,"Qobs.txt",sep="/") # publish only
  xobs    <- paste(toDir,"Xobs.txt",sep="/") # publish only
  nFiles  <- 0

  if (file.exists(pobs)) {
      nFiles <- nFiles + 1
      file.copy(from=pobs,to=toDir,overwrite=TRUE)
      cmn.log(paste0("cp ",pobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(fromDir,"/Pobs-",textFilename,".txt")
        file.copy(from=pobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(tobs)) {
      nFiles <- nFiles + 1
      file.copy(from=tobs,to=toDir,overwrite=TRUE)
      cmn.log(paste0("cp ",tobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(fromDir,"/Tobs-",textFilename,".txt")
        file.copy(from=tobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(tminobs)) {
      nFiles <- nFiles + 1
      file.copy(from=tminobs,to=toDir,overwrite=TRUE)
      cmn.log(paste0("cp ",tminobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(fromDir,"/TMINobs-",textFilename,".txt")
        file.copy(from=tminobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(tmaxobs)) {
      nFiles <- nFiles + 1
      file.copy(from=tmaxobs,to=toDir,overwrite=TRUE)
      cmn.log(paste0("cp ",tmaxobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(fromDir,"/TMAXobs-",textFilename,".txt")
        file.copy(from=tmaxobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(forckey)) {
      nFiles <- nFiles + 1
      file.copy(from=forckey,to=toDir,overwrite=TRUE)
      cmn.log(paste0("cp ",forckey," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(fromDir,"/ForcKey-",textFilename,".txt")
        file.copy(from=forckey,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(qobs)) {
      #nFiles <- nFiles + 1
      #file.copy(from=qobs,to=toDir,overwrite=TRUE)
      #cmn.log(paste0("cp ",qobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(toDir,"/Qobs-",textFilename,".txt")
        file.copy(from=qobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (file.exists(xobs)) {
      #nFiles <- nFiles + 1
      #file.copy(from=xobs,to=toDir,overwrite=TRUE)
      #cmn.log(paste0("cp ",xobs," to ",toDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PFC)
      if (publishFiles) {
        toFile = paste0(toDir,"/Xobs-",textFilename,".txt")
        file.copy(from=xobs,to=toFile)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
      }
  }

  if (nFiles < 5) {
    cmn.log("process_hindcast_netcdf2obs(): too few files produced", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PFC)
  }

} # process_copy_obs_files


# # Input  - meteo type (e.g. GFD, HydroGFD, HydroGFD 2.0), meteo version (e.g. 1.3, 2.0) optional, hindcast start date (bdate).
# # Output - formatted string to use as prefix for comparing filenames of state files.
# #          Uses naming from the constant cMeteoHindcastVariants.
# process_get_requested_statefile_suffix <- function(meteo,meteoVersion=NULL,hindcastStartDate)
# {
#   defMet    <- "unknown"
#   defMetVer <- "0.0"

#   met    <- defMet
#   metVer <- defMetVer

#   if (is.null(meteoVersion)) {
#     # Assume meteo is one of the defined constants (cMeteoHindcastVariantx) with both name and version
#     for (v in 1:length(cMeteoHindcastVariants)) {
#       m1 <- tolower(cMeteoHindcastVariants[v])
#       m2 <- tolower(meteo)
#       # Compare with input data
#       if (m1 == m2) {
#         met    <- gsub(pattern=" ",replacement="",m1)
#         metVer <- ""
#       }
#     }
#   }else {
#     # Check both name and version against the defined constants (cMeteoHindcastVariantx)
#     for (v in 1:length(cMeteoHindcastVariants)) {
#       variantList <- as.list(strsplit(cMeteoHindcastVariants[v],'\\s+')[[1]]) # Split constant string on space
#       if (length(variantList) > 1) {
#         m1    <- tolower(variantList[1])
#         m1ver <- variantList[2]
#         # Compare with input data
#         if ((tolower(meteo) == m1) && (meteoVersion == m1ver)) {
#           met    <- m1
#           metVer <- m1ver
#         }
#       }
#     }
#   }

#   hcdate <- gsub(pattern="-",replacement="",hindcastStartDate)

#   # Example: -hydrogfd2.0-bdate20100101
#   requestedFileNameSuffix <- paste0("-",met,meteoVersion,"-bdate",hcdate)

#   return (requestedFileNameSuffix)
# } # process_get_requested_statefile_suffix


# # Get forecast issue date from filename(s) matching the requested categories
# # Return the filename with the latest forecast issue date
# # Input  - local directory to check for state files
# # Output - status=0 and date (posix date class), filename incl. path.
# #        - status=1 when file not found or other error, return date 9999-01-01, filename NULL
# process_check_latest_statefiles_category <- function(filePath,meteo,meteoVersion,hindcastStartDate) #,forecastIssueDate)
# {
#   status   <- 1 # NOK
#   fileDate <- "99990101"
#   fileName <- NULL
  
#   #state_save20180101.txt-hydrogfd2.0-bdate20100101
  
#   requestedFileNamePrefix <- "state_save"
#   requestedFileNameSuffix <- process_get_requested_statefile_suffix(meteo,meteoVersion,hindcastStartDate)
#   requestedFileNameSuffix <- paste0(".txt",requestedFileNameSuffix)
  
#   latestDate <- 0
#   if (dir.exists(filePath)){
#     listFilenames <- list.files(path=filePath,pattern=requestedFileNamePrefix)
#     if (length(listFilenames) > 0){
#       for (fileIndex in 1:length(listFilenames)) {
#         res <- grep(pattern=requestedFileNameSuffix,listFilenames[fileIndex],fixed=TRUE)
#         if (length(res) > 0) {
#           startOfString <- gsub(pattern=requestedFileNameSuffix,replacement="",listFilenames[fileIndex])
#           res <- grep(pattern=requestedFileNamePrefix,startOfString,fixed=TRUE)
#           if (length(res) > 0) {
#             dateString <- gsub(pattern=requestedFileNamePrefix,replacement="",startOfString)

#             # Check that the remaining string is only numbers, should only be one date
#             listDates <- gsub(pattern=".*([0-9]{8}).*",replace="\\1",dateString)
#             if (length(listDates) > 0){
#               for (d in 1:length(listDates)){
#                   if (as.numeric(listDates[d]) > latestDate){
#                     latestDate <- as.numeric(listDates[d])

#                     fileDate <- listDates[d]
#                     #fileName <- paste(filePath,listFilenames[fileIndex],sep="/")
#                     fileName <- listFilenames[fileIndex] # Without path
#                     status   <- 0
#                   }
#               }
#             }
          
#           }
#         }
#       }
#     }
#   }
  
#   # ToDo: Check if found date is later than forecast issue date, return status 1 (NOK)?
#   fileDate <- as.Date(fileDate,format="%Y%m%d")
#   fileDate <- as.POSIXlt(fileDate)
  
#   return (list("status"=status,
#                "fileDate"=fileDate,
#                "fileName"=fileName))
# } # process_check_latest_statefiles_category
