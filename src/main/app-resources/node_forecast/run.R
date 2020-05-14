#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-forecast/src/main/app-resources/node_forecast/run.R

# Copyright 2017 SMHI
#
# This file is part of H-TEP Hydrological Modelling Application, which is open source
# and distributed under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your option)
# any later version. The Hydrology TEP Hydrological Modelling Application is distributed
# in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU
# General Public License for more details. You should have received a copy of the Lesser
# GNU General Public License along with the Hydrology TEP Hydrological Modelling Application.
# If not, see <http://www.gnu.org/licenses/>.

# Application 1: "Niger-HYPE 10 day forecast" (hypeapps-forecast)
# Author:         David Gustafsson, SMHI
# Version:        2018-02-06

# Workflow overview:
# ------------------
# 1 Initialization          (load environmental variables, libraries, utility functions, etc)
# 2 Application inputs      (read all user inputs)
# 3 Application setup       (setup working folders, copy model parameters)
# 4 Hindcast input          (hindcast forcing, initial state, info)
# 5 Run hindcast            (run the hindcast model)
# 6 Forecast input          (forecast forcing, initial state, info)
# 7 Run forecast            (run the forecast model)
# 8 Output                  (pepare and publish output data)
# 9 End of workflow

#################################################################################
## 0 - Common functions - should be placed in a separate source code file
## ------------------------------------------------------------------------------
# Wrap this type of logging into a log wrapper function, may require a separate init(open) and close functions.
#    if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), nameOfSrcFile_Run)}
#    log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)
#
# Different inputs to handle log support for
#  - print() or message() to stdout
#  - file handle for appLogWrite (or handled internally and the user gets another handle/integer as return)
#  - rciop.log
#

#################################################################################
## 1 - Initialization
## ------------------------------------------------------------------------------
# Constants
nameOfSrcFile_Run <- "/node_forecast/run.R"
#verbose <- TRUE
verbose <- FALSE
#verboseVerbose <- TRUE
verboseVerbose <- FALSE
#debugPublish <- TRUE
debugPublish <- FALSE

# Enable filename prefix with e.g. 001
enableNumbersAsFilenamePrefix = FALSE

# Handle HTEP fake input to only run the code in one run slot
stdin_f <- file("stdin")
open(stdin_f)
while(length(input <- readLines(stdin_f, n=1)) > 0) {

    # RUN ID give the code a random number to see how many times the code was run based on the random number in the output filenames
    # run_id <- runif(n=1, min=1, max=10)
    # run_id <- as.character(run_id *100000)

    ## ------------------------------------------------------------------------------
    ## set application run date
    # app.date = paste0(format(Sys.time(), "%Y%m%d_"), run_id)
    app.date = format(Sys.time(), "%Y%m%d_%H%M")

    ## ------------------------------------------------------------------------------
    ## set application name
    app.name = "forecast"

    ## ------------------------------------------------------------------------------
    ## flag which environment is used, if not set
    if(!exists("app.sys")){
        app.sys ="tep"
    }
    ## ------------------------------------------------------------------------------
    ## load rciop package and set working directory to TMPDIR when running on TEP
    if(app.sys=="tep"){
        library("rciop")

        rciop.log ("DEBUG", " *** hypeapps-forecast *** TEP hydrological modelling applications ***", nameOfSrcFile_Run)
        rciop.log ("DEBUG", " rciop library loaded", nameOfSrcFile_Run)

        setwd(TMPDIR)
        rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), nameOfSrcFile_Run)

        ## ------------------------------------------------------------------------------
        ## Source non-hypeapps functions
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/constants.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-configuration.R",sep="/"))
    }

    ## ------------------------------------------------------------------------------
    ## Load hypeapps environment and additional R utility functions
    if(app.sys=="tep"){
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-forcing.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))

        rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), nameOfSrcFile_Run)
    }else if(app.sys=="win"){
        source("application/util/R/hypeapps-environment.R")
        source("application/util/R/hypeapps-utils.R")
    }

    ## ------------------------------------------------------------------------------
    ## Open application logfile
    ## create a date tag to include in output filenames
    if (enableNumbersAsFilenamePrefix == TRUE) {
        prefixLogFile="000"
    }else{
        prefixLogFile=NULL
    }
    logFile=appLogOpen(appName = app.name, tmpDir = getwd(),appDate = app.date,prefix=prefixLogFile)

    ## ------------------------------------------------------------------------------
    # # Get git commit
    # git_con <- file(paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/git_commit.txt"),"r")
    # git_commit <- readLines(git_con,n=1)
    # close(git_con)
    # log.res=appLogWrite(logText = paste0("Running using git commit ", git_commit), fileConn = logFile$fileConn)


    #################################################################################
    ## 2 - Application inputs
    ## ------------------------------------------------------------------------------
    # Handle options for run-time configuration. Global options defined in application.xml
    # Process user options/selections to later select between different variants
    # of models, datasets etc.
    rciop.log("INFO", "Processing user selection:")
    applRuntimeOptions <- process_configuration_application_runtime_options()

    if (verboseVerbose == TRUE) {
        print("applRuntimeOptions (output from process_configuration_application_runtime_options):")
        print(applRuntimeOptions)
    }

    ## ------------------------------------------------------------------------------
    ## Read the main input (from stdin). This is the reference link to the application configuration object
    ## Extract information from file dependencies.txt, part of the application configuration object
    rciop.log("INFO", paste("Processing input from stdin:", input, sep=" "))
    modelConfigData <- process_configuration_application_inputs(input,applRuntimeOptions)

    if (verboseVerbose == TRUE) {
        print("modelConfigData (output from process_configuration_application_input):")
        print(modelConfigData)
    }

    ## ------------------------------------------------------------------------------
    ## Handle application input parameters, rciop.get_param(), for internal hypeapps functionality
    app.input <- getHypeAppInput(appName = app.name)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste(" hypeapps inputs and parameters read"), nameOfSrcFile_Run)}
    log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)

    #################################################################################
    ## 3 - Application setup
    ## ------------------------------------------------------------------------------
    ## Prepare basic model setup (static input files and hype model executable copied to working folder)

    if(app.sys!="tep"){
        log.res=appLogWrite(logText = "Using configuration from hypeapps-model-settings.R",fileConn = logFile$fileConn)
    }else if (app.sys=="tep") {
        ## ------------------------------------------------------------------------------
        ## Get HYPE model dataset
        rciop.log("INFO", "Processing config for model data", nameOfSrcFile_Run)
        #modelDataPaths <- process_configuration_hype_data(applRuntimeOptions,modelConfigData) # ,paste0(TMPDIR,"/hype-model-data"))

        ## ------------------------------------------------------------------------------
        # Overwrite variables normally set in hypeapps-model-settings.R
        # model.files.path     <- modelDataPaths$dirModelFiles     # Instead of model.files.url
        # forcing.archive.path <- modelDataPaths$dirForcingArchive # Instead of forcing.archive.url
        # state.files.path     <- modelDataPaths$dirStateFiles     # Instead of state.files.url
        # shape.files.path     <- modelDataPaths$dirShapeFiles     # Instead of shapefile.url
        # hype2csv.path        <- modelDataPaths$dirHYPE2CSVFiles  # Instead of hype2csv.url
        #ToDo: When switching between different HYPE model datasets, we may need to output a list of filenames (currently hardcoded filenames)
        model.files.path     <- modelConfigData$modelConfig                # Instead of model.files.url
        forcing.archive.path <- paste0(model.files.path,"/forcingarchive") # Instead of forcing.archive.url
        state.files.path     <- paste0(model.files.path,"/statefiles")     # Instead of state.files.url
        shape.files.path     <- paste0(model.files.path,"/subidshapefile") # Instead of shapefile.url
        hype2csv.path        <- NULL                                       # Instead of hype2csv.url
        # Mainly set due to getHypeAppSetup(). These variables are otherwise not part of the HydroGFD 2 files downloaded
        # later on by the netcdf2obs sequence.
    }

    ## ------------------------------------------------------------------------------
    # if (applRuntimeOptions$hydModel == cHydModelVariant1) {
    #     # Name of local subdir (run-dir/subdir), prefix in some filenames
    #     modelName <- model.name # hypeapps-model-settings.R
    #     modelBin  <- model.bin  # hypeapps-model-settings.R
    # }
    #if (applRuntimeOptions$hydModel == cHydModelVariant2) {
    if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
        modelName <- "westafrica-hype"
        modelBin  <- "hype-5.8.0.exe"

        # HYPE 5.8.0 required newer gfortran than part of gcc-4.4.7 during build.
        #Apperantely not set   Sys.setenv(LD_LIBRARY_PATH=paste("/opt/anaconda/pkgs/gcc-4.8.5-7/lib", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
        Sys.setenv(LD_LIBRARY_PATH=paste0("/opt/anaconda/pkgs/gcc-4.8.5-7/lib"))
        #print(Sys.getenv(LD_LIBRARY_PATH))

        # ToDo: Move this
    }

    ## ------------------------------------------------------------------------------
    app.setup <- getHypeAppSetup(modelName = modelName,
                                 modelBin  = modelBin,
                                 tmpDir    = app.tmp_path,
                                 appDir    = app.app_path,
                                 appName   = app.name,
                                 appInput  = app.input,
                                 modelFilesPath = model.files.path,
                                 forcingArchivePath = forcing.archive.path,
                                 shapeFilesPath = shape.files.path,
                                 hype2csvPath = hype2csv.path,
                                 stateFilesPath = state.files.path,
                                 stateFilesIN = state.files,
                                 debugPublishFiles = debugPublish)

    if (verboseVerbose == TRUE) {
        print("app.setup (output from getHypeAppSetup):")
        print(app.setup)
    }
    if (debugPublish == TRUE) {
        toFile = paste(app.setup$runDir,"info-hindcast-template.txt",sep="/")
        rciop.publish(path=toFile, recursive=FALSE, metalink=TRUE)
        toFile = paste(app.setup$runDir,"info-forecast-template.txt",sep="/")
        rciop.publish(path=toFile, recursive=FALSE, metalink=TRUE)
    }

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), nameOfSrcFile_Run)}
    log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)

    #################################################################################
    ## 4 - Hindcast input data
    ## ------------------------------------------------------------------------------
    ## forcing data
    # if (applRuntimeOptions$metHC == cMetHCVariant1) {
    #     hindcast.forcing <- getModelForcing(appSetup   = app.setup,
    #                                         appInput   = app.input,
    #                                         dataSource = forcing.data.source,
    #                                         hindcast   = T)
    #     if (verboseVerbose == TRUE) {
    #         print("hindcast.forcing (output from getModelForcing):")
    #         print(hindcast.forcing)
    #     }

    #     if(app.sys=="tep"){rciop.log ("DEBUG", paste("hindcast forcing set"), nameOfSrcFile_Run)}
    #     log.res=appLogWrite(logText = "Hindcast forcing data downloaded and prepared",fileConn = logFile$fileConn)
    # }

    #if (applRuntimeOptions$metHC == cMetHCVariant2) {
    if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
        ## Download and process hydrogfd netcdf files
        # ToDo: Dir name based on config dataset
        dirNCFiles  <- paste(TMPDIR,"netcdf-files",sep="/")
        dirObsFiles <- paste(TMPDIR,"obs-files",sep="/") # Output dir of produced files
        #dirGridMeta <- paste(TMPDIR,"grid-meta",sep="/")
        hindcastForcing <- process_forcing_hydrogfd2_hindcast(modelConfigData$meteoConfig,
                                                              modelConfigData$modelConfig,
                                                              app.input$idate,
                                                              app.input$hcperiodlen,
                                                              reforecast=(applRuntimeOptions$runType == cRunTypeVariantReforecast),
                                                              stateFileCreation=(applRuntimeOptions$runTypeStateFileCreation == cRunTypeVariantStatefile),
                                                              metHCType=applRuntimeOptions$metHC,
                                                              modelConfigData$statefileHindcastDate,
                                                              modelConfigData$configGridLinkFilename,
                                                              dirNCFiles,
                                                              ncSubDir=TRUE,
                                                              app.setup$runDir,
                                                              dirObsFiles,
                                                              debugPublish)

        # Minimal variants of original list types returned by getModelForcing()
        hindcast.forcing <- list("status"=T,
                                 "localFile"=NULL,
                                 "issueDate"=app.input$idate,
                                 "archive"=F,
                                 "bdate"=hindcastForcing$bdate,
                                 "cdate"=hindcastForcing$cdate,
                                 "edate"=hindcastForcing$edate,
                                 "outstateDate"=NULL, # or NA
                                 #"stateFile"=stateFile) # ToDo: Add path to output
                                 "stateFile"=hindcastForcing$stateFile)

        if (verboseVerbose == TRUE) {
            print("hindcast.forcing (output from process_hindcast_netcdf2obs):")
            print(hindcast.forcing)
        }
    }

    ## ------------------------------------------------------------------------------
    ## get Xobs input file(s) from open catalogue
    xobs.data <- getXobsData(appInput = app.input, # xobsNum, xobs, xobsURL
                             appSetup = app.setup) # model run dir, res dir etc.

    if (verboseVerbose == TRUE) {
        print("xobs.data (output from getXobsData):")
        print(xobs.data)
    }

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data downloaded from catalogue"), nameOfSrcFile_Run)}
    log.res=appLogWrite(logText = "xobs data (if any) downloaded from catalogue",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## read downloaded Xobs input file(s) - merge into one Xobs.txt in the model run folder
    xobs.input <- readXobsData(appSetup = app.setup,
                                xobsData = xobs.data)

    if (verboseVerbose == TRUE) {
        print("xobs.input (output from readXobsData):")
        print(xobs.input)
    }

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data merged to model rundir"), nameOfSrcFile_Run)}
    log.res=appLogWrite(logText = "Xobs data (if any) merged into model directory",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## modify some model files (info.txt) based on input parameters
    hindcast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = T, modelForcing = hindcast.forcing, xobsInput = xobs.input)

    if (verboseVerbose == TRUE) {
        print("hindcast.input (output from updateModelInput):")
        print(hindcast.input)
    }

    # Publish updated info.txt
    fromFile <- paste(app.setup$runDir,"info.txt",sep="/")
    toFile   <- paste(app.setup$runDir,"info-for-hindcast.txt",sep="/")
    if(file.exists(fromFile)) {
        file.copy(from=fromFile,to=toFile,overwrite=T) # Rename file
        rciop.log ("INFO", paste0("cp ",fromFile," to ",toFile),nameOfSrcFile_Run)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
    }else {
        rciop.log("INFO",paste0("File missing: ",fromFile),nameOfSrcFile_Run)
        #q()
    }

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("...hindcast inputs modified"), nameOfSrcFile_Run)}
    log.res=appLogWrite(logText = "hindcast model inputs modified",fileConn = logFile$fileConn)

    #################################################################################
    ## 5 - Run hindcast
    ## ------------------------------------------------------------------------------
    ##  run hindcast
    if(hindcast.input==0){
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...starting hindcast model run", nameOfSrcFile_Run)}
        log.res=appLogWrite(logText = "starting hindcast model run ...",fileConn = logFile$fileConn)

        hindcast.run = system(command = app.setup$runCommand,intern = F)
        if (hindcast.run != 0){
            rciop.log("ERROR",paste0("hindcast.run exit code: ",hindcast.run),nameOfSrcFile_Run)

            # Publish hindcast log file(s) in case prepareHypeAppsOutput() is not called
            hyssLogFiles = dir(path=app.setup$runDir,pattern=".log")
            if (length(hyssLogFiles) > 0){
                for (i in 1:length(hyssLogFiles)) {
                    rciop.publish(path=paste(app.setup$runDir,hyssLogFiles[i],sep="/"),recursive=FALSE,metalink=TRUE)
                }
            }
        }

        # For run mode "create statefile", publish produced state file, ToDo: add output to hindcast.forcing$
        outStateDate <- hindcast.forcing$edate
        outStateDate <- gsub("-", "", as.character(outStateDate))
        stateFile <- paste0(app.setup$runDir,"/hindcast","/state_save",outStateDate,".txt")
        doPublishFile <- ((applRuntimeOptions$runTypeStateFileCreation == cRunTypeVariantStatefile) || (debugPublish == TRUE))
        if (doPublishFile) {
            if(file.exists(stateFile)) {
                rciop.publish(path=stateFile,recursive=FALSE,metalink=TRUE)
            }else {
                rciop.log("INFO",paste0("File missing: ",stateFile),nameOfSrcFile_Run)
                #q()
            }
        }

        if (verboseVerbose == TRUE) {
            print("List files after hindcast, dir run dir")
            tmpPath=app.setup$runDir
            print(list.files(tmpPath))
            print("List files after hindcast, dir hindcast")
            tmpPath=paste0(app.setup$runDir,"/hindcast")
            print(list.files(tmpPath))
        }

        log.res=appLogWrite(logText = "... hindcast model run ready",fileConn = logFile$fileConn)
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...hindcast model run ready", nameOfSrcFile_Run)}
    }else{
        if(app.sys=="tep"){rciop.log ("ERROR", "something wrong with hindcast model inputs (no run)", nameOfSrcFile_Run)}
        log.res=appLogWrite(logText = "something wrong with hindcast model inputs (no run)",fileConn = logFile$fileConn)
    }

    ## ------------------------------------------------------------------------------
    # Check if forecast sequence shall be run
    doForecastSequence <- (applRuntimeOptions$runTypeStateFileCreation != cRunTypeVariantStatefile)
    if (doForecastSequence) {

        #################################################################################
        ## 6 - Forecast input data
        ## ------------------------------------------------------------------------------
        ## forcing data
        # if (applRuntimeOptions$metHC == cMetHCVariant1) {
        #     forecast.forcing <- getModelForcing(appSetup   = app.setup,
        #                                         appInput   = app.input,
        #                                         dataSource = forcing.data.source,
        #                                         hindcast   = F)

        #     if (verboseVerbose == TRUE) {
        #         print("forecast.forcing (output from getModelForcing):")
        #         print(forecast.forcing)
        #     }
        # }

        # if ((applRuntimeOptions$metHC == cMetHCVariant2) &&
        #     (applRuntimeOptions$metFC == cMetFCVariant1)) {
        if (applRuntimeOptions$modelConfig == cModelConfigVariant1) {
            ## Download and process ecoper netcdf files
            forecastForcing <- process_forcing_hydrogfd2_forecast(modelConfigData$meteoConfig,
                                                                  app.input$idate,
                                                                  dirNCFiles,
                                                                  ncSubDir=TRUE,
                                                                  app.setup$runDir,
                                                                  dirObsFiles,
                                                                  debugPublish)

            # Copy produced state file from the hindcast run to run dir
            outStateDate <- forecastForcing$cdate
            outStateDate <- gsub("-", "", as.character(outStateDate))
            stateFile <- paste0(app.setup$runDir,"/hindcast","/state_save",outStateDate,".txt")
            if(file.exists(stateFile)) {
                file.copy(from=stateFile,to=app.setup$runDir,overwrite=TRUE)
                rciop.log ("INFO", paste0("cp ",stateFile," to ",app.setup$runDir,"/"),nameOfSrcFile_Run)
                if (debugPublish == TRUE) {
                    rciop.publish(path=stateFile,recursive=FALSE,metalink=TRUE)
                }
            }else {
                rciop.log("INFO",paste0("File missing: ",stateFile),nameOfSrcFile_Run)
                #q()
            }

            # Cleanup certain forcing files from run dir for forecast run
            hindcast_forcing_files = c('Qobs.txt','Xobs.txt')

            hindcast_run_dir = app.setup$runDir
            for(f in 1:length(hindcast_forcing_files)){
                file_to_be_removed = paste0(hindcast_run_dir,'/',hindcast_forcing_files[f])
                if (file.exists(file_to_be_removed)) {
                    print(paste0("rm ",file_to_be_removed))
                    file.remove(file_to_be_removed)
                }
            }

            # Minimal variants of original list types returned by getModelForcing()
            forecast.forcing <- list("status"=T,
                                    "localFile"=NULL,
                                    "issueDate"=app.input$idate,
                                    "archive"=F,
                                    "bdate"=forecastForcing$bdate,
                                    "cdate"=forecastForcing$cdate,
                                    "edate"=forecastForcing$edate,
                                    "outstateDate"=outStateDate,
                                    "stateFile"=stateFile)

            if (verboseVerbose == TRUE) {
                print("forecast.forcing (output from process_forecast_netcdf2obs):")
                print(forecast.forcing)
            }
        }

        if(app.sys=="tep"){rciop.log ("DEBUG", "forecast model forcing data downloaded and prepared", nameOfSrcFile_Run)}
        log.res=appLogWrite(logText = "forecast model forcing data downloaded and prepared",fileConn = logFile$fileConn)

        ## ------------------------------------------------------------------------------
        ## modify some model files based on input parameters
        forecast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                        hindcast = F, modelForcing = forecast.forcing, xobsInput = NULL)

        if (verboseVerbose == TRUE) {
            print("forecast.input (output from updateModelInput):")
            print(forecast.input)
        }

        # Publish updated info.txt
        fromFile <- paste(app.setup$runDir,"info.txt",sep="/")
        toFile <- paste(app.setup$runDir,"info-for-forecast.txt",sep="/")
        if(file.exists(fromFile)) {
            file.copy(from=fromFile,to=toFile,overwrite=T) # Rename file
            rciop.log ("INFO", paste0("cp ",fromFile," to ",toFile),nameOfSrcFile_Run)
            rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
        }else {
            rciop.log("INFO",paste0("File missing: ",fromFile),nameOfSrcFile_Run)
            #q()
        }

        if(app.sys=="tep"){rciop.log ("DEBUG", paste("...forecast inputs modified"), nameOfSrcFile_Run)}
        log.res=appLogWrite(logText = "forecast model input files modified",fileConn = logFile$fileConn)

        #################################################################################
        ## 7 - Run forecast
        ## ------------------------------------------------------------------------------
        ##  run model
        if(forecast.input==0){
            if(app.sys=="tep"){rciop.log ("DEBUG", " ...starting forecast model run", nameOfSrcFile_Run)}
            log.res=appLogWrite(logText = "starting forecast model run ...",fileConn = logFile$fileConn)

            forecast.run = system(command = app.setup$runCommand,intern = F)
            if (forecast.run != 0){
                rciop.log("ERROR",paste0("forecast.run exit code: ",forecast.run),nameOfSrcFile_Run)
            }

            if (verboseVerbose == TRUE) {
                print("List files after forecast, dir run dir")
                tmpPath=app.setup$runDir
                print(list.files(tmpPath))
                print("List files after forecast, dir forecast")
                tmpPath=paste0(app.setup$runDir,"/forecast")
                print(list.files(tmpPath))
            }

            log.res=appLogWrite(logText = "... forecast model run ready",fileConn = logFile$fileConn)
            if(app.sys=="tep"){rciop.log ("DEBUG", " ...forecast model run ready", nameOfSrcFile_Run)}
        }else{
            if(app.sys=="tep"){rciop.log ("ERROR", "something wrong with forecast model inputs (no run)", nameOfSrcFile_Run)}
            log.res=appLogWrite(logText = "something wrong with forecast model inputs (no run)",fileConn = logFile$fileConn)
        }

    } # if (doForecastSequence)

    #################################################################################
    ## 8 - Output
    ## ------------------------------------------------------------------------------
    ## publish postprocessed results # ToDo: HYPE model and log files shall remain published, review what part that is the postprocessing stuff
    # ToDo: Remove postprocessing like trigger etc. Move up publish

    ## ------------------------------------------------------------------------------
    ## post-process output data
    if (! doForecastSequence) {
        modelInput   <- hindcast.input
        modelForcing <- hindcast.forcing
        runRes       <- hindcast.run
    } else {
        modelInput   <- forecast.input
        modelForcing <- forecast.forcing
        runRes       <- forecast.run
    }

    app.outfiles <- prepareHypeAppsOutput(appSetup  = app.setup, appInput = app.input,
                                          modelInput = modelInput, modelForcing = modelForcing,
                                          runRes = runRes,
                                          appDate = app.date,
                                          numbersAsFilenamePrefix = enableNumbersAsFilenamePrefix)
    if(length(app.outfiles)>1){
        app.outfiles=sort(app.outfiles,decreasing = F)
    }
    log.res=appLogWrite(logText = "HypeApp outputs prepared",fileConn = logFile$fileConn)

    # trigger_distribution_outfiles <- NULL # or remove this section since no plots or maps are longer produced
    # # Prepare for trigger distribution
    # # Written by jafet.andersson@smhi.se
    # if(length(app.outfiles>1)){
    #     for (i in app.outfiles) {
    #         if (grepl("forecast_mapWarningLevel.txt", i)) {
    #             map_file <- i
    #             print(paste0("app.input$idate: ",app.input$idate)) # Only year present previous run??? idate ok here...
    #             trigger_distribution_outfiles <- TriggerDistribution(dirname(map_file), app.input$idate)
    #         }
    #     }
    # }

    if(app.sys=="tep"){
        for(k in 1:length(app.outfiles)){
            rciop.publish(path=app.outfiles[k], recursive=FALSE, metalink=TRUE)
        }
        log.res=appLogWrite(logText = "HypeApp outputs published",fileConn = logFile$fileConn)

        # if(length(trigger_distribution_outfiles) > 0){
        #     for(k in 1:length(trigger_distribution_outfiles)){
        #         rciop.publish(path=trigger_distribution_outfiles[k], recursive=FALSE, metalink=TRUE)
        #     }
        #     log.res=appLogWrite(logText = "trigger distribution outputs published",fileConn = logFile$fileConn)
        # }
    }

    ## close and publish the logfile
    log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
    if(app.sys=="tep"){
        rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=TRUE)
    }

    #}

    if (verboseVerbose == TRUE) {
        print("List files after end, dir output")
        tmpPath=paste0(app.setup$tmpDir,"/output")
        print(list.files(tmpPath))
        print("List files after end, dir output/hindcast")
        tmpPath=paste0(app.setup$tmpDir,"/output/hindcast")
        print(list.files(tmpPath))
        print("List files after end, dir output/forecast")
        tmpPath=paste0(app.setup$tmpDir,"/output/forecast")
        print(list.files(tmpPath))
    }

    #################################################################################
    ## 9 - End of workflow
    ## ------------------------------------------------------------------------------
    ## exit with appropriate status code
    q(save="no", status = 0)
} # while(length(input
