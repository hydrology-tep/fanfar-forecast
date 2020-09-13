
if(app.sys=="tep") {
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-forcing-common.R",sep="/"))
}else{
    source ('process-forcing-common.R')
}

# Constants
nameOfSrcFile_PF3 <- "/util/R/process-forcing-hgfd3.R"

#verbose <- TRUE
#verbose <- FALSE
#verboseVerbose <- TRUE
#verboseVerbose <- FALSE

# Includes that the sourced R-files uses
#library(ncdf4)

# This handles the sequence for each x obs  file (iterates opensearch, download each meterological GFD, call function to convert to obs)
# Retrieve files (pr,tas,tasmin,tasmax) via opensearch and rciop.copy
# Configurable option to download all files into one dir or individual sub-dirs
# for pr, tas, tasmin and tasmax.
# Returns the number of missing files
download_netcdf_hgfd3 <- function(modelConfig,    # sub-dir to use for local download dir, url, query pattern etc
                                  netcdfDir,      # base dir to download files too
                                  ncSubDir,       # False-one dir, True-separate dir for each variable
                                  xCast=NULL,     # hindcast, forecast, elevation
                                  stateFileCreation=FALSE, # for hindcast
                                  he5StartDate=NULL,   # dates for search start/stop, expected date in filename to check against
                                  he5EndDate=NULL,
                                  he5tmStartDate=NULL,
                                  he5tmEndDate=NULL,
                                  he5tdStartDate=NULL,
                                  he5tdEndDate=NULL,
                                  odStartDate=NULL,
                                  odEndDate=NULL,
                                  odfStartDate=NULL,
                                  odfEndDate=NULL)
{
    if (verboseVerbose == TRUE) {
        print("download_netcdf_hgfd3:")
        print(modelConfig)
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
        # Handle he5
        if (! is.null(he5StartDate)){
            urlNC     <- modelConfig$gfdHe5Url
            query     <- modelConfig$gfdHe5Query
            startDate <- he5StartDate
            stopDate  <- he5EndDate

            process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
            nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                           "_he5_",fileSuffix)
            if (nMissing > 0){
                cmn.log(paste0(nMissing," file(s) missing for he5"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
            }
            nMissingFiles <- nMissingFiles + nMissing
        }

        if (! stateFileCreation){
            ## ------------------------------------------------------------------------------
            # Handle he5tm
            if (! is.null(he5tmStartDate)){
                urlNC     <- modelConfig$gfdHe5tmUrl
                query     <- modelConfig$gfdHe5tmQuery
                startDate <- he5tmStartDate
                stopDate  <- he5tmEndDate

                process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
                # Check retrieved filenames
                nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                               "_he5tm_",fileSuffix)
                if (nMissing > 0){
                    cmn.log(paste0(nMissing," file(s) missing for he5tm"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
                }
                nMissingFiles <- nMissingFiles + nMissing
            }

            ## ------------------------------------------------------------------------------
            # Handle he5td
            if (! is.null(he5tdStartDate)){
                urlNC     <- modelConfig$gfdHe5tdUrl
                query     <- modelConfig$gfdHe5tdQuery
                startDate <- he5tdStartDate
                stopDate  <- he5tdEndDate

                # nMissing = 0
                # n_attempts = 5
                # while(n_attempts > 0){
                #     process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
                #     # Check retrieved filenames
                #     nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                #                                                 "_he5td_",fileSuffix)
                #     if (nMissing > 0){
                #         n_attempts = n_attempts -1
                #         cmn.log(paste0(nMissing," file(s) missing for he5td"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
                #         Sys.sleep(5)
                #     }else{
                #         n_attempts = 0
                #         cmn.log(paste0(nMissing," file(s) missing NOT for he5td"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
                #     }
                # }
                # nMissingFiles <- nMissingFiles + nMissing

                nMissing <- process_search_download_check_wrapper(
                                url=urlNC,
                                query,
                                startDate,
                                stopDate,
                                rootDir=netcdfDir,
                                subDir=ncSubDir,
                                filePrefix="_he5td_",
                                fileSuffix)
                if (nMissing > 0){
                    cmn.log(paste0(nMissing," file(s) missing for he5td"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
                }
                nMissingFiles <- nMissingFiles + nMissing
            }

            ## ------------------------------------------------------------------------------
            # Handle od
            if (! is.null(odStartDate)){
                urlNC     <- modelConfig$gfdOdUrl
                query     <- modelConfig$gfdOdQuery
                startDate <- odStartDate
                stopDate  <- odEndDate

                process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
                nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                               "_od_",fileSuffix)
                if (nMissing > 0){
                    cmn.log(paste0(nMissing," file(s) missing for od"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
                }
                nMissingFiles <- nMissingFiles + nMissing
            }
        } # stateFileCreation
    } # hindcast
    if (xCast == "forecast"){
        ## ------------------------------------------------------------------------------
        # Handle odf
        if (! is.null(odfStartDate)){
            urlNC     <- modelConfig$gfdOdfUrl
            query     <- modelConfig$gfdOdfQuery
            startDate <- odfStartDate
            stopDate  <- odfEndDate

            process_search_and_download_netcdf(urlNC,query,startDate,stopDate,ncRootDir=netcdfDir,ncSubDir)
            nMissing <- process_check_date_interval_netcdf(startDate,stopDate,ncRootDir=netcdfDir,ncSubDir,
                                                           "_odf_",fileSuffix)
            if (nMissing > 0){
                cmn.log(paste0(nMissing," file(s) missing for odf"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
            }
            nMissingFiles <- nMissingFiles + nMissing
        }
    } # forecast
    if (xCast == "elevation"){
        ## ------------------------------------------------------------------------------
        # Handle elevation
        url       <- modelConfig$gfdElevationUrl
        query     <- modelConfig$gfdElevationQuery
        subDir    <- modelConfig$gfdElevationSubDir
        search    <- TRUE
        if (! is.null(modelConfig$gfdElevationDoSearch)){
            search <- modelConfig$gfdElevationDoSearch
        }

        # process_search_and_download(url,query,netcdfDir,subDir,search)
        # expFilename <- paste(netcdfDir,subDir,"hgfd_orography.nc",sep="/")
        # if (! file.exists(expFilename)){
        #     nMissing <- 1
        #     cmn.log(paste0("Missing file: ",expFilename), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
        # }

        expFilename <- paste(netcdfDir,subDir,"hgfd_orography.nc",sep="/")
        nMissing <- process_search_download_check_wrapper(
                        url=url,
                        query,
                        startDate=NULL,
                        stopDate=NULL,
                        rootDir=netcdfDir,
                        subDir=subDir,
                        filePrefix=NULL,
                        fileSuffix=NULL,
                        filename=expFilename,
                        search)
            if (nMissing > 0){
                cmn.log(paste0("Missing file: ",expFilename), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
            }
        nMissingFiles <- nMissingFiles + nMissing
    } # elevation
    if (xCast == "grid.meta"){
        ## ------------------------------------------------------------------------------
        # Handle grid meta shape files for point and polygon
        url       <- modelConfig$gfdGridUrl
        query     <- modelConfig$gfdGridQuery
        subDir    <- modelConfig$gfdGridSubDir
        search    <- TRUE
        if (! is.null(modelConfig$gfdGridDoSearch)){
            search <- modelConfig$gfdGridDoSearch
        }

        process_search_and_download(url,query,netcdfDir,subDir,search)
    } # elevation
    ## ------------------------------------------------------------------------------

    return (nMissingFiles)
} # download_netcdf_hgfd3


# External function
# Wrapper for netcdf2obs sequence
process_forcing_hydrogfd3_hindcast <- function(modelConfig, # Misc config data, urls to search for hydrogfd data etc.
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
                                               debugPublishFiles=FALSE, # Condition to publish files during development
                                               reforecastingMethod=1, # 1 - standard, 2 - minimal
                                               verbose=F) # More output
{
    if (verbose){
        print(paste0("modelConfig: ",modelConfig))
        print(paste0("modelFiles: ",modelFiles))
        print(paste0("forecastIssueDate: ",forecastIssueDate))
        print(paste0("hindcastPeriodLength: ",hindcastPeriodLength))
        print(paste0("reforecast: ",reforecast))
        print(paste0("stateFileCreation: ",stateFileCreation))
        print(paste0("meteoHindcastType: ",meteoHindcastType))
        print(paste0("statefileHindcastDate: ",statefileHindcastDate))
        print(paste0("configGridLinkFilename: ",configGridLinkFilename))
        print(paste0("netcdfDir: ",netcdfDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("modelFilesRunDir: ",modelFilesRunDir))
        print(paste0("obsDir: ",obsDir))
        print(paste0("debugPublishFiles: ",debugPublishFiles))
        print(paste0("reforecastingMethod: ",reforecastingMethod))
    }

    if(app.sys=="tep") {
        # source ('utils_file.R')
        # source ('reforecast_engine_common_args.R')
        # source ('reforecast_engine_common.R')
        #source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_engine_common.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_file_htep.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_run_reforecasting_engine.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_run_netcdf_to_obs.R",sep="/"))
    }else{
        cmn.log("DEBUG: System not supported", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 1)
    }


    # Calculate hindcast intervals
    
    intervals = run_hindcast_calc( # reforecast_run_reforecasting_engine.R
                    app_sys=app.sys,
                    issue_date=string_ymd_to_string(forecastIssueDate),
                    hindcast_period_length=hindcastPeriodLength,
                    hindcast_begin_date=NULL,
                    hindcast_end_date=NULL,
                    run_mode_reforecast=(reforecast==TRUE),
                    run_mode_operational=(reforecast==FALSE),
                    run_mode_state_file_creation=stateFileCreation,
                    monthly_break_day=6,
                    hindcast_window_days=130,
                    hype_state_file=NULL,
                    output_intervals_to_csv_file=NULL,
                    output_hype_dates_to_csv_file=NULL,
                    reforecasting_method=reforecastingMethod,
                    part1_url=modelConfig$gfdHe5Url,
                    part1_query=modelConfig$gfdHe5Query,
                    part2_url=modelConfig$gfdHe5tmUrl,
                    part2_query=modelConfig$gfdHe5tmQuery,
                    hype_state_file_bdate=statefileHindcastDate,
                    hype_state_files_path=paste0(modelFiles,"/statefiles")
                    )
    # Reduce one list layer
    #statefile_instate_date = intervals$statefile_instate_date # NULL or date
    statefile_instate      = intervals$statefile_instate # NULL or filename (not path+filename since renaming file)
    intervals              = intervals$intervals

    cmn.log(paste0("hindcast interval: ",intervals$hindcastStartDate," -> ",intervals$hindcastEndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("he5:               ",intervals$he5StartDate," -> ",intervals$he5EndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("he5tm:             ",intervals$he5tmStartDate," -> ",intervals$he5tmEndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("he5td:             ",intervals$he5tdStartDate," -> ",intervals$he5tdEndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("od:                ",intervals$odStartDate," -> ",intervals$odEndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)

    cmn.log(paste0("bdate: ",intervals$bdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("cdate: ",intervals$cdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("edate: ",intervals$edate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
q(save="no", status = 0)
    # Download netcdf files

    # Download grid meta shape files for point and polygon
    nMissingFiles <- download_netcdf_hgfd3(modelConfig,
                                           netcdfDir,
                                           ncSubDir,
                                           xCast="grid.meta")
    if (nMissingFiles > 0) {
        cmn.log("Aborting due to missing HydroGFD v3 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 2)
    }

    # Download hydrogfd elevation netcdf file
    nMissingFiles <- download_netcdf_hgfd3(modelConfig,
                                           netcdfDir,
                                           ncSubDir,
                                           xCast="elevation")
    if (nMissingFiles > 0) {
        cmn.log("Aborting due to missing HydroGFD v3 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 2)
    }

    # For the calculated time interval, search and download hydrogfd netcdf files
    nMissingFiles <- download_netcdf_hgfd3(modelConfig,
                                           netcdfDir,
                                           ncSubDir,
                                           xCast="hindcast",
                                           stateFileCreation,
                                           he5StartDate=intervals$he5StartDate,
                                           he5EndDate=intervals$he5EndDate,
                                           he5tmStartDate=intervals$he5tmStartDate,
                                           he5tmEndDate=intervals$he5tmEndDate,
                                           he5tdStartDate=intervals$he5tdStartDate,
                                           he5tdEndDate=intervals$he5tdEndDate,
                                           odStartDate=intervals$odStartDate,
                                           odEndDate=intervals$odEndDate,
                                           odfStartDate=NULL,
                                           odfEndDate=NULL)
    if (nMissingFiles > 0) {
        cmn.log("Aborting due to missing HydroGFD v3 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 2)
    }

    # Run netcdf_to_obs

    application_root=paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/netcdf_to_obs_hgfd3",sep="/")
    status = run_hindcast_netcdf_to_obs( # reforecast_run_netcdf_to_obs.R
                application_root=application_root,
                application=paste(application_root,"netcdf_to_obs_wrapper.R",sep="/"),
                app_sys=app.sys,
                #
                hgfd_nc_dir_monthly=NULL,
                hgfd_nc_dir_daily=NULL,
                hgfd_nc_dir_user=NULL,
                hgfd_nc_dir_htep=netcdfDir, # Dir with downloaded netcdf files
                hgfd_nc_subdir=ncSubDir,
                grid_shape_file_dir=paste(netcdfDir,modelConfig$gfdGridSubDir,"grid.meta","grid.meta",sep="/"),
                grid_elevation_file=paste(netcdfDir,modelConfig$gfdElevationSubDir,"hgfd_orography.nc",sep="/"),
                subid_shape_file=paste(modelFiles,"subidshapefile","SUBID_shapefile.shp", sep="/"),
                #grid_link_file=gridLinkFile,
                grid_link_file=paste0(modelFiles,"/shapefiles/",configGridLinkFilename),
                output_dir=obsDir,
                tmp_dir=dirNetcdfToObsTmp,
                #
                reforecasting_method=reforecastingMethod,
                run_mode_state_file_creation=stateFileCreation,
                dateobj_hindcast_startdate=intervals$hindcastStartDate,
                dateobj_hindcast_enddate=intervals$hindcastEndDate,
                dateobj_he5_startdate=intervals$he5StartDate,
                dateobj_he5_enddate=intervals$he5EndDate,
                dateobj_he5tm_startdate=intervals$he5tmStartDate,
                dateobj_he5tm_enddate=intervals$he5tmEndDate,
                dateobj_he5td_startdate=intervals$he5tdStartDate,
                dateobj_he5td_enddate=intervals$he5tdEndDate,
                dateobj_od_startdate=intervals$odStartDate,
                dateobj_od_enddate=intervals$odEndDate)

    # # Publish gridLink.Rdata
    # if (debugPublishFiles){
    #     gridLinkFile = paste0(obsDir,"/gridLink.Rdata")
    #     if (file.exists(gridLinkFile)){
    #         cmn.log(paste0("222222publish ",gridLinkFile), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    #         rciop.publish(path=gridLinkFile,recursive=FALSE,metalink=TRUE)
    #     }
    # }

    # Copy produced files to HYPE run dir
    process_copy_obs_files(fromDir=obsDir,
                            toDir=modelFilesRunDir,
                            publishFiles=debugPublishFiles,
                            textFilename='netcdf-to-obs-hgfd3-hindcast')

    # Copy state file to run dir
    dstFile = NULL
    if(! is.null(statefile_instate)) {
        requestedFileNameSuffix = utils_file_get_requested_statefile_suffix(meteoHindcastType,NULL,statefileHindcastDate)
        stateFileWithoutSuffix  = gsub(pattern=requestedFileNameSuffix,replacement="",statefile_instate)

        srcFile = paste0(modelFiles,"/statefiles/",statefile_instate)
        dstFile = paste0(modelFilesRunDir,"/",stateFileWithoutSuffix)

        if(file.exists(srcFile)) {
            cmn.log(paste0("cp ",srcFile," to ",dstFile), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
            file.copy(from=srcFile,to=dstFile,overwrite=TRUE)
            #print(list.files(modelFilesRunDir))
        }else{
            cmn.log("Problem copying state file for upcoming hindcast run", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF3)
        }
    }

    hindcast.forcing <- list("bdate"=intervals$bdate,
                             "cdate"=intervals$cdate,
                             "edate"=intervals$edate,
                             "stateFile"=dstFile)
    return (hindcast.forcing)

} # process_forcing_hydrogfd3_hindcast


# External function
# Wrapper for netcdf2obs sequence
process_forcing_hydrogfd3_forecast <- function(modelConfig, # Misc config data, now
                                               modelFiles, # Misc model config data, paths to state files, forcing, shape files
                                               forecastIssueDate, # yyyy-mm-dd
                                               configGridLinkFilename, # Name of grid link file from configuration
                                               netcdfDir, # Input dir with hydrogfd netcdf files
                                               ncSubDir, # False-one dir, True-separate dir for each variable
                                               modelFilesRunDir = NULL, # HYPE model data files
                                               obsDir, # Output dir for obs files
                                               dirNetcdfToObsTmp, # Temporary work dir for netcdf_to_obs
                                               debugPublishFiles=FALSE) # Condition to publish files during development
{
    if (verbose){
        print(paste0("modelConfig: ",modelConfig))
        print(paste0("modelFiles: ",modelFiles))
        print(paste0("forecastIssueDate: ",forecastIssueDate))
        # print(paste0("hindcastPeriodLength: ",hindcastPeriodLength))
        # print(paste0("reforecast: ",reforecast))
        # print(paste0("stateFileCreation: ",stateFileCreation))
        # print(paste0("meteoHindcastType: ",meteoHindcastType))
        # print(paste0("statefileHindcastDate: ",statefileHindcastDate))
        print(paste0("configGridLinkFilename: ",configGridLinkFilename))
        print(paste0("netcdfDir: ",netcdfDir))
        print(paste0("ncSubDir: ",ncSubDir))
        print(paste0("modelFilesRunDir: ",modelFilesRunDir))
        print(paste0("obsDir: ",obsDir))
        print(paste0("debugPublishFiles: ",debugPublishFiles))
        # print(paste0("reforecastingMethod: ",reforecastingMethod))
    }

    if(app.sys=="tep") {
        # source ('utils_file.R')
        # source ('reforecast_engine_common_args.R')
        # source ('reforecast_engine_common.R')
        #source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_engine_common.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_file_htep.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_run_reforecasting_engine.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/reforecast_run_netcdf_to_obs.R",sep="/"))
    }else{
        cmn.log("DEBUG: System not supported", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 1)
    }

    # Calculate forecast intervals    
    intervals = run_forecast_calc( # reforecast_run_reforecasting_engine.R
                    issue_date=string_ymd_to_string(forecastIssueDate),
                    forecast_begin_date=NULL,
                    output_intervals_to_csv_file=NULL,
                    output_hype_dates_to_csv_file=NULL
                    )

    cmn.log(paste0("forecast interval: ",intervals$forecastStartDate," -> ",intervals$forecastEndDate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
 
    cmn.log(paste0("bdate: ",intervals$bdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("cdate: ",intervals$cdate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)
    cmn.log(paste0("edate: ",intervals$edate), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_PF3)

    # For the calculated time interval, search and download hydrogfd netcdf files
    nMissingFiles <- download_netcdf_hgfd3(modelConfig,
                                           netcdfDir,
                                           ncSubDir,
                                           xCast="forecast",
                                           stateFileCreation=FALSE,
                                           he5StartDate=NULL,
                                           he5EndDate=NULL,
                                           he5tmStartDate=NULL,
                                           he5tmEndDate=NULL,
                                           he5tdStartDate=NULL,
                                           he5tdEndDate=NULL,
                                           odStartDate=NULL,
                                           odEndDate=NULL,
                                           odfStartDate=intervals$forecastStartDate,
                                           odfEndDate=intervals$forecastStartDate) # One file with ten time steps
    if (nMissingFiles > 0) {
        cmn.log("Aborting due to missing HydroGFD v3 netcdf file(s)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_PF3)
        q(save="no", status = 2)
    }

    # Run netcdf_to_obs

    application_root=paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/netcdf_to_obs_hgfd3",sep="/")
    status = run_forecast_netcdf_to_obs( # reforecast_run_netcdf_to_obs.R
                application_root=application_root,
                application=paste(application_root,"netcdf_to_obs_wrapper.R",sep="/"),
                app_sys=app.sys,
                #
                hgfd_nc_dir_monthly=NULL,
                hgfd_nc_dir_daily=NULL,
                hgfd_nc_dir_user=NULL,
                hgfd_nc_dir_htep=netcdfDir, # Dir with downloaded netcdf files
                hgfd_nc_subdir=ncSubDir,
                grid_shape_file_dir=paste(netcdfDir,modelConfig$gfdGridSubDir,"grid.meta","grid.meta",sep="/"),
                grid_elevation_file=paste(netcdfDir,modelConfig$gfdElevationSubDir,"hgfd_orography.nc",sep="/"),
                subid_shape_file=paste(modelFiles,"subidshapefile","SUBID_shapefile.shp", sep="/"),
                #grid_link_file=gridLinkFile,
                grid_link_file=paste0(modelFiles,"/shapefiles/",configGridLinkFilename),
                output_dir=obsDir,
                tmp_dir=dirNetcdfToObsTmp,
                #
                dateobj_forecast_startdate=intervals$forecastStartDate,
                dateobj_forecast_enddate=intervals$forecastEndDate)

    # Copy produced files to HYPE run dir
    process_copy_obs_files(fromDir=obsDir,
                            toDir=modelFilesRunDir,
                            publishFiles=debugPublishFiles,
                            textFilename='netcdf-to-obs-hgfd3-forecast')

    forecast.forcing <- list("bdate"=intervals$bdate,
                             "cdate"=intervals$cdate,
                             "edate"=intervals$edate)
    return (forecast.forcing)

} # process_forcing_hydrogfd3_forecast
