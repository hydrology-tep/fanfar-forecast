#!/opt/anaconda/envs/cdo-env/bin/Rscript --vanilla --slave --quiet
#
# Victor Näslund 20190403
#
# Find hydrogfd files and process them for netcdf-to-obs

library("rciop")

get_issue_date <- function(issue_date){
    return (as.POSIXlt(as.Date(issue_date)))
}

get_begin_date <- function(issue_date, fc_duration){
    begin <- ""

    fc_duration <- as.character(fc_duration)
    
    if (grepl("Y", fc_duration, fixed=TRUE)) {
        issue_date_d <- as.Date(issue_date)
        length <- as.numeric(strsplit(fc_duration, split="Y")[1])
        begin <- issue_date_d
        begin <- as.POSIXlt(begin)
        begin$year <- begin$year - length
        print (fc_duration)
            
    } else if (grepl("M", fc_duration, fixed=TRUE)) {
        issue_date_d <- as.Date(issue_date)
        length <- as.numeric(strsplit(fc_duration, split="M")[1])
        begin <- issue_date_d
        begin <- as.POSIXlt(begin)
        begin$year <- begin$mon - length

    } else if (grepl("D", fc_duration, fixed=TRUE)) {
        issue_date_d <- as.Date(issue_date)
        length <- as.numeric(strsplit(fc_duration, split="D")[1])
        begin <- issue_date_d
        begin <- as.POSIXlt(begin)
        begin$mday <- begin$mday - length
    }
    return(begin)
}

# get files from the datastore
get_netcdf_file <- function(filename){
    if (grepl("_hydrogfdei_", filename, fixed=TRUE)) {
        path <- "./data/hydrogfd2.0/hydrogfdei/"

    } else if (grepl("_hydrogfdod_", filename, fixed=TRUE)) {
        path <- "./data/hydrogfd2.0/hydrogfdod/"

    } else if (grepl("_od-daily_", filename, fixed=TRUE)) {
        path <- "./data/hydrogfd2.0/od/"

    } else if (grepl("_ecoper_", filename, fixed=TRUE)) {
        path <- "./data/hydrogfd2.0/ecoper/"
    }

    v <- head(n=1, strsplit(filename, "_", fixed=T)[[1]])
    print (paste0("Using file ", filename))
    system(paste0("cdo -selvar,", v, " ", path, filename, " ./data/current_run/single/", filename))
    print ("Use opensearch code below!")
    return(0)

    # FIXME get date from filename, finish this code
    files <- system("opensearch-client -p q=hydrogfdei -p start=2017-11-01 -p stop=2017-11-30 https://catalog.terradue.com/hydro-smhi/search enclosure", intern=T)

    for (f in files) {
        rciop.copy(f, "./data/current_run/single")
        # Rename the file to correct name
    }
}

copy_hydrogfdei_netcdfs <- function(begin_date, issue_date){

    if (issue_date$mday < 12) {
        end_date <- issue_date
        end_date$mon <- end_date$mon - 4
    } else {
        end_date <- issue_date
        end_date$mon <- end_date$mon - 3
    }

    variables = c("pr", "tas", "tasmin", "tasmax")
    for (v in variables){

        counter <- begin_date
        end_year <- substr(as.character(end_date), 1, 4)
        end_mon <- substr(as.character(end_date), 6, 7)
    
        while (T) {
            curr_year <- substr(as.character(counter), 1, 4)
            curr_mon <- substr(as.character(counter), 6, 7)

            date <- paste0(curr_year, curr_mon)
            filename <- paste0(v, "_hydrogfdei_", date, "_fanfar_SMHI.nc")
            
            get_netcdf_file(filename)
                
            counter$mon <- counter$mon +1
            if (curr_year >= end_year && curr_mon >= end_mon) {
                break
            }
        }
    } 
}

copy_hydrogfdod_netcdfs <- function(begin_date, issue_date){

    if (issue_date$mday < 12) {
        begin_date <- issue_date
        begin_date$mon <- begin_date$mon - 3
        end_date <- issue_date
        end_date$mon <- issue_date$mon -2
    } else {
        begin_date <- issue_date
        begin_date$mon <- begin_date$mon - 2
        end_date <- issue_date
        end_date$mon <- issue_date$mon -1
    }
    
    variables = c("pr", "tas", "tasmin", "tasmax")
    for (v in variables){

        counter <- begin_date
        end_year <- substr(as.character(end_date), 1, 4)
        end_mon <- substr(as.character(end_date), 6, 7)
    
        while (T) {
            curr_year <- substr(as.character(counter), 1, 4)
            curr_mon <- substr(as.character(counter), 6, 7)

            date <- paste0(curr_year, curr_mon)
            filename <- paste0(v, "_hydrogfdod_", date, "_fanfar_SMHI.nc")
            
            get_netcdf_file(filename)

            counter$mon <- counter$mon +1
            if (curr_year >= end_year && curr_mon >= end_mon) {
                break
            }
        }
    } 
}

copy_od_netcdfs <- function(begin_date, issue_date){

    if (issue_date$mday < 12) {
        begin_date <- issue_date
        begin_date$mon <- begin_date$mon - 1
        end_date <- issue_date
        end_date$mon <- issue_date$mon -0
    } else {
        begin_date <- issue_date
        begin_date$mon <- begin_date$mon - 0
        end_date <- issue_date
        end_date$mon <- issue_date$mon -0
    }

    end_date$mday <- end_date$mday -1
    
    variables = c("pr", "tas", "tasmin", "tasmax")
    for (v in variables){

        counter <- begin_date
        end_year <- substr(as.character(end_date), 1, 4)
        end_mon <- substr(as.character(end_date), 6, 7)
        end_day <- substr(as.character(end_date), 9, 10)
    
        while (T) {
            curr_year <- substr(as.character(counter), 1, 4)
            curr_mon <- substr(as.character(counter), 6, 7)
            curr_day <- substr(as.character(counter), 9, 10)

            date <- paste0(curr_year, curr_mon, curr_day)
            filename <- paste0(v, "_od-daily_", date, "_fanfar_SMHI.nc")
            
            get_netcdf_file(filename)

            counter$mday <- counter$mday +1
            if (curr_year >= end_year && curr_mon >= end_mon && curr_day >= end_day) {
                break
            }
        }
    } 
}

copy_hydrogfd_ecoper_netcdfs <- function(issue_date){
    begin_date <- issue_date
    end_date <- issue_date
    #end_date$mday <- end_date$mday +10
        
    variables = c("pr", "tas", "tasmin", "tasmax")
    for (v in variables){

        counter <- begin_date
        end_year <- substr(as.character(end_date), 1, 4)
        end_mon <- substr(as.character(end_date), 6, 7)
        end_day <- substr(as.character(end_date), 9, 10)
    
        while (T) {
            curr_year <- substr(as.character(counter), 1, 4)
            curr_mon <- substr(as.character(counter), 6, 7)
            curr_day <- substr(as.character(counter), 9, 10)

            date <- paste0(curr_year, curr_mon, curr_day)
            filename <- paste0(v, "_ecoper_", date, "00_fanfar_SMHI.nc")
            
            get_netcdf_file(filename)

            counter$mday <- counter$mday +1
            if (curr_year >= end_year && curr_mon >= end_mon && curr_day >= end_day) {
                break
            }
        }
    } 
}


setup_netcdfs <- function(issue_date, length) {
    dir.create("./data/current_run/single",recursive = T)

    issue_date <- paste(substr(issue_date, 1, 4), "-", substr(issue_date, 5, 6), "-", substr(issue_date, 7, 8), sep="")

    issue_date <- get_issue_date(issue_date)
    begin <- get_begin_date(issue_date, length)

    copy_hydrogfdei_netcdfs(begin, issue_date)

    # Uncomment me when Emmanuel have inported HOD, OD and ecoper into the catalog
    #copy_hydrogfdod_netcdfs(begin, issue_date)
    #copy_od_netcdfs(begin, issue_date)
    #copy_hydrogfd_ecoper_netcdfs(issue_date)

    # merge single files into one
    variables = c("pr", "tas", "tasmin", "tasmax")
    for (v in variables){
      system(paste0("cdo -mergetime ./data/current_run/single/", v, "_*.nc ./data/current_run/", v, "_fanfar_SMHI.nc"))
    }
}

options(show.error.locations = TRUE)

args <- commandArgs(TRUE)

print (system("cdo --help", intern=T))

rciop.log ("DEBUG", "Starting netcdf-to-obs-get-files.R", "/util/R/netcdf-to-obs-get-files.R")
rciop.log ("DEBUG", "Doing nothing since hdyrogfd data is not avaliable in the catalog", "/util/R/netcdf-to-obs-get-files.R")
q()

command("source activate cdo-env", intern=T)

# Setup netcdfs
setup_netcdfs(args[1], args[2])

print (warnings())

command("source deactivate cdo-env", intern=T)
