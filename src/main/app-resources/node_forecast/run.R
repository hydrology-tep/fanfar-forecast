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
    }

    ## ------------------------------------------------------------------------------
    ## Load common log functions and setup redirection of log text to log file, stdout and rciop
    if(app.sys=="tep") {
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hgfd3_reforecasting_engine/utils_log.R",sep="/"))
    } else {
        source("application/util/R/hgfd3_reforecasting_engine/utils_log.R")
    }

    if (enableNumbersAsFilenamePrefix) {
        prefix="000"
    } else {
        prefix=NULL
    }
    
    fileName=paste("r",app.date,"_","hypeapps-",app.name,".log",sep="")
    if(!is.null(prefix)){
        fileName = paste(prefix,"_",fileName,sep="")
    }

    logHandle <- cmn.logOpen(currentSystem=app.sys,
                             toStdout=(verbose==TRUE),
                             toRCIOPLOG=(app.sys=="tep"),
                             fileName=fileName,
                             filePath=TMPDIR)
    cmn.log(paste("app.sys:", app.sys, sep=" "), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    ## ------------------------------------------------------------------------------
    ## Load hypeapps environment and additional R utility functions
    if(app.sys=="tep"){
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/constants.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-configuration.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-forcing-hgfd2.R",sep="/"))

        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))

    }else if(app.sys=="win"){
        source("application/util/R/constants.R")
        source("application/util/R/process-configuration.R")
        source("application/util/R/process-forcing-hgfd2.R",sep="/")

        source("application/util/R/hypeapps-environment.R")
        source("application/util/R/hypeapps-utils.R")
    }
    cmn.log("Libraries loaded and utilities sourced", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    #################################################################################
    ## 2 - Application inputs
    ## ------------------------------------------------------------------------------
    # Handle options for run-time configuration. Global options defined in application.xml
    # Process user options/selections to later select between different variants
    # of models, datasets etc.
    cmn.log("Processing user selection:", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    applRuntimeOptions <- process_configuration_application_runtime_options()

    publishHindcastForcingFiles <- FALSE
    if (applRuntimeOptions$runTypeStateFileCreation == cRunTypeVariantStatefile){
        # Publish P/Tobs.txt or gridLink files for run type statefile creation
        publishHindcastForcingFiles <- TRUE
        cmn.log("Enabled publish of P/Tobs.txt and/or gridLink files for run type statefile creation", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    if (verboseVerbose) {
        cmn.log("applRuntimeOptions (output from process_configuration_application_runtime_options):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        cmn.log(applRuntimeOptions, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    ## ------------------------------------------------------------------------------
    ## Read the main input (from stdin). This is the reference link to the application configuration object
    ## Extract information from file dependencies.txt, part of the application configuration object
    cmn.log(paste("Processing input from stdin:", input, sep=" "), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    modelConfigData <- process_configuration_application_inputs(input,applRuntimeOptions)

    if (verboseVerbose) {
        cmn.log("modelConfigData (output from process_configuration_application_input):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        cmn.log(modelConfigData, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    ## ------------------------------------------------------------------------------
    ## Handle application input parameters, rciop.get_param(), for internal hypeapps functionality
    app.input <- getHypeAppInput(appName = app.name)

    cmn.log("Inputs and parameters read", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    #################################################################################
    ## 3 - Application setup
    ## ------------------------------------------------------------------------------
    ## Prepare basic model setup (static input files and hype model executable copied to working folder)

    ## ------------------------------------------------------------------------------
    ## Get HYPE model dataset
    cmn.log("Processing config for model data", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
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

    if (verboseVerbose) {
        cmn.log("app.setup (output from getHypeAppSetup):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        #cmn.log(app.setup, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }
    if (debugPublish) {
        toFile = paste(app.setup$runDir,"info-hindcast-template.txt",sep="/")
        rciop.publish(path=toFile, recursive=FALSE, metalink=TRUE)
        toFile = paste(app.setup$runDir,"info-forecast-template.txt",sep="/")
        rciop.publish(path=toFile, recursive=FALSE, metalink=TRUE)
    }

    cmn.log("HypeApp setup read", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    #################################################################################
    ## 4 - Hindcast input data
    ## ------------------------------------------------------------------------------
    ## forcing data
    # if (applRuntimeOptions$metHC == cMetHCVariant1) {
    #     hindcast.forcing <- getModelForcing(appSetup   = app.setup,
    #                                         appInput   = app.input,
    #                                         dataSource = forcing.data.source,
    #                                         hindcast   = T)
    #     if (verboseVerbose) {
    #         print("hindcast.forcing (output from getModelForcing):")
    #         print(hindcast.forcing)
    #     }

    #     cmn.log("Hindcast forcing data downloaded and prepared", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
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
                                                              publishHindcastForcingFiles)

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

        if (verboseVerbose) {
            cmn.log("hindcast.forcing (output from process_hindcast_netcdf2obs):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
            cmn.log(hindcast.forcing, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        }
    }

    ## ------------------------------------------------------------------------------
    ## get Xobs input file(s) from open catalogue
    xobs.data <- getXobsData(appInput = app.input, # xobsNum, xobs, xobsURL
                             appSetup = app.setup) # model run dir, res dir etc.

    if (verboseVerbose) {
        cmn.log("xobs.data (output from getXobsData):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        cmn.log(xobs.data, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    cmn.log("Xobs data (if any) downloaded from catalogue", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    ## ------------------------------------------------------------------------------
    ## read downloaded Xobs input file(s) - merge into one Xobs.txt in the model run folder
    xobs.input <- readXobsData(appSetup = app.setup,
                                xobsData = xobs.data)

    if (verboseVerbose) {
        cmn.log("xobs.input (output from readXobsData):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        cmn.log(xobs.input, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    cmn.log("Xobs data (if any) merged into model directory", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    ## ------------------------------------------------------------------------------
    ## modify some model files (info.txt) based on input parameters
    hindcast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = T, modelForcing = hindcast.forcing, xobsInput = xobs.input)

    if (verboseVerbose) {
        cmn.log("hindcast.input (output from updateModelInput):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        cmn.log(hindcast.input, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }

    # Publish updated info.txt
    fromFile <- paste(app.setup$runDir,"info.txt",sep="/")
    toFile   <- paste(app.setup$runDir,"info-for-hindcast.txt",sep="/")
    if(file.exists(fromFile)) {
        file.copy(from=fromFile,to=toFile,overwrite=T) # Rename file
        cmn.log(paste0("cp ",fromFile," to ",toFile), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
    }else {
        cmn.log(paste0("File missing: ",fromFile), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
        #q()
    }

    cmn.log("Hindcast model inputs modified", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

    #################################################################################
    ## 5 - Run hindcast
    ## ------------------------------------------------------------------------------
    ##  run hindcast
    if(hindcast.input==0){
        cmn.log("Starting hindcast model run", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

        hindcast.run = system(command = app.setup$runCommand,intern = F)
        if (hindcast.run != 0){
            cmn.log(paste0("Hindcast.run exit code: ",hindcast.run), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)

            # Publish hindcast log file(s) in case prepareHypeAppsOutput() is not called
            hyssLogFiles = dir(path=app.setup$runDir,pattern=".log")
            if (length(hyssLogFiles) > 0){
                for (i in 1:length(hyssLogFiles)) {
                    rciop.publish(path=paste(app.setup$runDir,hyssLogFiles[i],sep="/"),recursive=FALSE,metalink=TRUE)
                }
            }
        }else{
            cmn.log(paste0("Hindcast.run exit code: ",hindcast.run), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        }

        # For run mode "create statefile", publish produced state file, ToDo: add output to hindcast.forcing$
        outStateDate <- hindcast.forcing$edate
        outStateDate <- gsub("-", "", as.character(outStateDate))
        stateFile <- paste0(app.setup$runDir,"/hindcast","/state_save",outStateDate,".txt")
        doPublishFile <- ((applRuntimeOptions$runTypeStateFileCreation == cRunTypeVariantStatefile) || debugPublish)
        if (doPublishFile) {
            if(file.exists(stateFile)) {
                rciop.publish(path=stateFile,recursive=FALSE,metalink=TRUE)
            }else {
                cmn.log(paste0("File missing: ",stateFile), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
                #q()
            }
        }

        if (verboseVerbose) {
            print("List files after hindcast, dir run dir")
            tmpPath=app.setup$runDir
            print(list.files(tmpPath))
            print("List files after hindcast, dir hindcast")
            tmpPath=paste0(app.setup$runDir,"/hindcast")
            print(list.files(tmpPath))
        }

        cmn.log("Hindcast model run ready", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
    }else{
        cmn.log("Something wrong with hindcast model inputs (no run)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
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

        #     if (verboseVerbose) {
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
                cmn.log(paste0("cp ",stateFile," to ",app.setup$runDir,"/"), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
                if (debugPublish) {
                    rciop.publish(path=stateFile,recursive=FALSE,metalink=TRUE)
                }
            }else {
                cmn.log(paste0("File missing: ",stateFile), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
                #q()
            }

            # Cleanup certain forcing files from run dir for forecast run
            hindcast_forcing_files = c('Qobs.txt','Xobs.txt')

            hindcast_run_dir = app.setup$runDir
            for(f in 1:length(hindcast_forcing_files)){
                file_to_be_removed = paste0(hindcast_run_dir,'/',hindcast_forcing_files[f])
                if (file.exists(file_to_be_removed)) {
                    cmn.log(paste0("rm ",file_to_be_removed), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
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

            if (verboseVerbose) {
                cmn.log("forecast.forcing (output from process_forecast_netcdf2obs):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
                cmn.log(forecast.forcing, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
            }
        }

        cmn.log("Forecast model forcing data downloaded and prepared", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

        ## ------------------------------------------------------------------------------
        ## modify some model files based on input parameters
        forecast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                        hindcast = F, modelForcing = forecast.forcing, xobsInput = NULL)

        if (verboseVerbose) {
            cmn.log("forecast.input (output from updateModelInput):", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
            cmn.log(forecast.input, logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        }

        # Publish updated info.txt
        fromFile <- paste(app.setup$runDir,"info.txt",sep="/")
        toFile <- paste(app.setup$runDir,"info-for-forecast.txt",sep="/")
        if(file.exists(fromFile)) {
            file.copy(from=fromFile,to=toFile,overwrite=T) # Rename file
            cmn.log(paste0("cp ",fromFile," to ",toFile), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
            rciop.publish(path=toFile,recursive=FALSE,metalink=TRUE)
        }else {
            cmn.log(paste0("File missing: ",fromFile), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
            #q()
        }

        cmn.log("Forecast model input files modified", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

        #################################################################################
        ## 7 - Run forecast
        ## ------------------------------------------------------------------------------
        ##  run model
        if(forecast.input==0){
            cmn.log("Starting forecast model run", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

            forecast.run = system(command = app.setup$runCommand,intern = F)
            if (forecast.run != 0){
                cmn.log(paste0("Forecast.run exit code: ",forecast.run), logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
            }else{
                cmn.log(paste0("Forecast.run exit code: ",forecast.run), logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
            }

            if (verboseVerbose) {
                print("List files after forecast, dir run dir")
                tmpPath=app.setup$runDir
                print(list.files(tmpPath))
                print("List files after forecast, dir forecast")
                tmpPath=paste0(app.setup$runDir,"/forecast")
                print(list.files(tmpPath))
            }

            cmn.log("Forecast model run ready", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        }else{
            cmn.log("Something wrong with forecast model inputs (no run)", logHandle, rciopStatus="ERROR", rciopProcess=nameOfSrcFile_Run)
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
    cmn.log("HypeApp outputs prepared", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

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
        cmn.log("HypeApp outputs published", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)

        # if(length(trigger_distribution_outfiles) > 0){
        #     for(k in 1:length(trigger_distribution_outfiles)){
        #         rciop.publish(path=trigger_distribution_outfiles[k], recursive=FALSE, metalink=TRUE)
        #     }
        #     cmn.log("Trigger distribution outputs published", logHandle, rciopStatus="INFO", rciopProcess=nameOfSrcFile_Run)
        # }
    }

    ## close and publish the logfile
    status <- cmn.logClose(logHandle)
    # if (status != 0) {
    #     print("Error closing file")
    #     print(logHandle$file)
    # }
    if(app.sys=="tep"){
        if (file.exists(logHandle$file)) {
            rciop.publish(path=logHandle$file, recursive=FALSE, metalink=TRUE)
        }
    }

    #}

    if (verboseVerbose) {
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
