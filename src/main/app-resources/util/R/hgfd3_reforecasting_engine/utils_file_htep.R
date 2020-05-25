#
# File related utililties
#
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_time.R",sep="/"))
}

# Input  - meteo type (e.g. GFD, HydroGFD, HydroGFD 2.0), meteo version (e.g. 1.3, 2.0) optional, hindcast start date (bdate).
# Output - formatted string to use as prefix for comparing filenames of state files.
#          Uses naming from the constant cMeteoHindcastVariants.
utils_file_get_requested_statefile_suffix <- function(meteo,meteoVersion=NULL,hindcastStartDate)
{
    # From constants.R:
    cMeteoHindcastVariant1 = "GFD 1.3"
    cMeteoHindcastVariant2 = "HydroGFD 2.0"
    cMeteoHindcastVariant3 = "HydroGFD 3.0"
    cMeteoHindcastVariants = c(cMeteoHindcastVariant1,cMeteoHindcastVariant2,cMeteoHindcastVariant3)

    defMet    = "unknown"
    defMetVer = "0.0"

    met    = defMet
    metVer = defMetVer

    if (is.null(meteoVersion)) {
        # Assume meteo is one of the defined constants (cMeteoHindcastVariantx) with both name and version
        for (v in 1:length(cMeteoHindcastVariants)) {
            m1 = tolower(cMeteoHindcastVariants[v])
            m2 = tolower(meteo)
            # Compare with input data
            if (m1 == m2) {
                met    = gsub(pattern=" ",replacement="",m1)
                metVer = ""
            }
        }
    }else {
        # Check both name and version against the defined constants (cMeteoHindcastVariantx)
        for (v in 1:length(cMeteoHindcastVariants)) {
            variantList = as.list(strsplit(cMeteoHindcastVariants[v],'\\s+')[[1]]) # Split constant string on space
            if (length(variantList) > 1) {
                m1    = tolower(variantList[1])
                m1ver = variantList[2]
                # Compare with input data
                if ((tolower(meteo) == m1) && (meteoVersion == m1ver)) {
                    met    = m1
                    metVer = m1ver
                }
            }
        }
    }

    hcdate = gsub(pattern="-",replacement="",hindcastStartDate)

    # Example: -hydrogfd2.0-bdate20100101
    requestedFileNameSuffix = paste0("-",met,meteoVersion,"-bdate",hcdate)

    return (requestedFileNameSuffix)
} # utils_file_get_requested_statefile_suffix


# Get forecast issue date from filename(s) matching the requested categories
# Return the filename with the latest forecast issue date
# Input  - local directory to check for state files
# Output - status=0 and date (posix date class), filename incl. path.
#        - status=1 when file not found or other error, return date 9999-01-01, filename NULL
utils_file_check_latest_statefiles_category <- function(filePath,meteo,meteoVersion,hindcastStartDate)
{
    status   = 1 # NOK
    fileDate = "99990101"
    fileName = NULL
    
    #state_save20180101.txt-hydrogfd2.0-bdate20100101
    
    requestedFileNamePrefix = "state_save"
    requestedFileNameSuffix = utils_file_get_requested_statefile_suffix(meteo,meteoVersion,hindcastStartDate)
    requestedFileNameSuffix = paste0(".txt",requestedFileNameSuffix)
    
    latestDate = 0
    if (dir.exists(filePath)){
        listFilenames = list.files(path=filePath,pattern=requestedFileNamePrefix)
        if (length(listFilenames) > 0){
            for (fileIndex in 1:length(listFilenames)) {
                res = grep(pattern=requestedFileNameSuffix,listFilenames[fileIndex],fixed=TRUE)
                if (length(res) > 0) {
                    startOfString = gsub(pattern=requestedFileNameSuffix,replacement="",listFilenames[fileIndex])
                    res = grep(pattern=requestedFileNamePrefix,startOfString,fixed=TRUE)
                    if (length(res) > 0) {
                        dateString = gsub(pattern=requestedFileNamePrefix,replacement="",startOfString)

                        # Check that the remaining string is only numbers, should only be one date
                        listDates = gsub(pattern=".*([0-9]{8}).*",replace="\\1",dateString)
                        if (length(listDates) > 0){
                            for (d in 1:length(listDates)){
                                if (as.numeric(listDates[d]) > latestDate){
                                    latestDate = as.numeric(listDates[d])

                                    fileDate = listDates[d]
                                    #fileName = paste(filePath,listFilenames[fileIndex],sep="/")
                                    fileName = listFilenames[fileIndex] # Without path
                                    status   = 0
                                }
                            }
                        }

                    }
                }
            }
        }
    }
  
    #fileDate = as.Date(fileDate,format="%Y%m%d")
    #fileDate = as.POSIXlt(fileDate)
    
    return (list("status"=status,
                 "fileDate"=fileDate,
                 "fileName"=fileName))
} # utils_file_check_latest_statefiles_category


# Search for the latest item depending on url and query.
# Returns a list with status and date as a character string. On error status = 1
utils_file_search_and_locate_latest_date <- function(url,query)
{
    # Constants
    osClientApp = "opensearch-client"

    # Outputs
    status   = 1 # Not ok
    yyyymmdd = ""

    # Latest, without specifying any date
    opensearchCmd=paste0(osClientApp," '",url,"'"," -p ","'count=1'"," -p ","'cat=",query,"'"," enclosure")
    message(opensearchCmd)
    res_enclosure = system(command = opensearchCmd,intern = T)
    #Example res_enclosure:"https://store.terradue.com/hydro-smhi/fanfar/Operational/hydrogfdei/2019/07/tasmin_hydrogfdei_201907_fanfar_SMHI.nc"

    if (nchar(res_enclosure > 0)) {
        # Locate and extract date from filename in returned http url
        # .*  - match any character
        # ()  - capturing group with expected match for 6 single digits
        # \\1 - reference to first captured pattern
        yearmon = gsub(pattern=".*([0-9]{6}).*",replace="\\1", res_enclosure)
        if (nchar(yearmon) >= 6) { 
            yyyy = substr(yearmon,1,4)
            mm   = substr(yearmon,5,6)
            dd   = last_day_in_month(as.numeric(yyyy),as.numeric(mm))
        
            #yyyymmdd = paste(yyyy,mm,dd,sep="-")
            yyyymmdd = paste0(yyyy,mm,dd)
            status   = 0
        }else{
            print("Could not locate date in filename")
        }
    }

    output = list("status"=status,
                   "date"=yyyymmdd)

    return (output)
} # utils_file_search_and_locate_latest_date
