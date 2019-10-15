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
nameOfSrcFile <- "/node_forecast/run.R"

## create a date tag to include in output filenames

# Handle HTEP fake input to only run the code in one run slot
stdin_f <- file("stdin")
open(stdin_f)
while(length(input <- readLines(stdin_f, n=1)) > 0) {

    # From user interface via input
    #input='https://recast.terradue.com/t2api/search/hydro-smhi/models?uid=9345ED73B72F49E6FF31B07B57013BC519210E24' #niger-hype-model-2.23.zip, one dir when unzipped\n",

    # RUN ID give the code a random number to see how many times the code was run based on the random number in the output filenames
    # run_id <- runif(n=1, min=1, max=10)
    # run_id <- as.character(run_id *100000)
    # app.date = paste0(format(Sys.time(), "%Y%m%d_"), run_id)
    app.date = format(Sys.time(), "%Y%m%d_%H%M")

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

        rciop.log ("DEBUG", " *** hypeapps-forecast *** TEP hydrological modelling applications ***", nameOfSrcFile)
        rciop.log ("DEBUG", " rciop library loaded", nameOfSrcFile)

        setwd(TMPDIR)
        rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), nameOfSrcFile)

        # Source non-HYPE files
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-configuration.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/process-netcdf.R",sep="/"))
    }

    # Handle options for run-time configuration, e.g. switch between HYPE models etc.
    # Global options defined in application.xml
    rciop.log("INFO", "Processing user selection:")
    applRuntimeOptions <- process_application_runtime_options()
    #rciop.log("INFO", applRuntimeOptions)
    #print(applRuntimeOptions)

    ## Read the main input
    ## This is the reference link to the model configuration
    rciop.log("INFO", paste("Processing input from stdin:", input, sep=" "))
    modelConfigData <- process_input_model_configuration(applRuntimeOptions,
                                                         input) #,TMPDIR)
    rciop.log("INFO", modelConfigData)

    ## ------------------------------------------------------------------------------
    ## load hypeapps environment and additional R utility functions
    if(app.sys=="tep"){
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))

        source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/TriggerDistribution.r", sep="/"))

        rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), nameOfSrcFile)
        }else if(app.sys=="win"){
        source("application/util/R/hypeapps-environment.R")
        source("application/util/R/hypeapps-utils.R")
        }
    ## open application logfile
    logFile=appLogOpen(appName = app.name, tmpDir = getwd(),appDate = app.date,prefix="000")

    # Get git commit
    git_con <- file(paste0(Sys.getenv("_CIOP_APPLICATION_PATH"), "/git_commit.txt"),"r")
    git_commit <- readLines(git_con,n=1)
    close(git_con)
    #git_commit <- system(paste0("cd ", Sys.getenv("_CIOP_APPLICATION_PATH"), " ; git rev-parse HEAD"), intern=T)
    log.res=appLogWrite(logText = paste0("Running using git commit ", git_commit), fileConn = logFile$fileConn)


    #################################################################################
    ## 2 - Application user inputs
    ## ------------------------------------------------------------------------------
    ## Handle application input parameters, rciop.get_param()
    app.input <- getHypeAppInput(appName = app.name)

    ## ------------------------------------------------------------------------------
    # Handle options for run-time configuration, e.g. switch between HYPE models etc.
    # Global options defined in application.xml
    #rciop.log("INFO", paste("Processing user selection:", input, sep=" "))
    #applRuntimeOptions <- process_application_runtime_options()
    #rciop.log("INFO", applRuntimeOptions)
    #print(applRuntimeOptions)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste(" hypeapps inputs and parameters read"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)

    #################################################################################
    ## 3 - Application setup
    ## ------------------------------------------------------------------------------
    ## Prepare basic model setup (static input files and hype model executable copied to working folder)

    if(app.sys!="tep"){
        # Use these values defined in hypeapps-model-settings.R:
        #   model.files.url = "https://store.terradue.com/hydro-smhi/fanfar/model/niger-hype/v2.23" # model files root index
        #   forcing.archive.url  = "https://store.terradue.com/hydro-smhi/fanfar/model/niger-hype/v2.23/forcingarchive"
        #   state.files.url = "https://store.terradue.com/hydro-smhi/fanfar/model/niger-hype/v2.23/statefiles"
        log.res=appLogWrite(logText = "Using configuration from hypeapps-model-settings.R",fileConn = logFile$fileConn)
    }
    else if (app.sys=="tep") {
        ## ------------------------------------------------------------------------------
        ## Get Hype model data dirs, niger-hype-data/data and niger-hype-data/v2.23
        ##### This code now continues to retrieve the data dirs (rciop.copy)

        rciop.log("INFO", "Processing config for model data", nameOfSrcFile)
        modelDataPaths <- process_input_hype_model_data(applRuntimeOptions,modelConfigData) # ,paste0(TMPDIR,"/hype-model-data"))
        #print(modelDataPaths)

        model.files.path     <- modelDataPaths$pathModelFiles     # Instead of model.files.url
        forcing.archive.path <- modelDataPaths$pathForcingArchive # Instead of forcing.archive.url
        state.files.path     <- modelDataPaths$pathStateFiles     # Instead of state.files.url
        #shapefiles          <- modelDataPaths$pathShapeFiles
        rciop.log("INFO path", model.files.path)
        rciop.log("INFO path", forcing.archive.path)
        rciop.log("INFO path", state.files.path)

        ## ------------------------------------------------------------------------------
        # # For now, if needed, pass url from model_config_data['model-data-old-url'] to let present R code continue to do rciop.copy locally.
        # modelDataOldUrl <- modelConfigData$modelDataOldUrl

        # # Overwrite variables normally set in hypeapps-model-settings.R with url from the model configuration object
        # model.files.url     <- paste(modelDataOldUrl,"v2.23",sep="/")
        # forcing.archive.url <- paste(model.files.url,"forcingarchive",sep="/")
        # state.files.url     <- paste(model.files.url,"statefiles",sep="/")
        # # Check if other source code sources the file hypeapps-model-settings.R separately and are using any of these variables/constants.
        # # If its some other type of x-cast then it should not be of any problem.
        # rciop.log("INFO path", modelDataOldUrl)
        # rciop.log("INFO path", model.files.url)
        # rciop.log("INFO path", forcing.archive.url)
        # rciop.log("INFO path", state.files.url)
    }
    #q(save="no", status = 0)

    app.setup <- getHypeAppSetup(modelName = model.name,
                                 modelBin  = model.bin,
                                 tmpDir    = app.tmp_path,
                                 appDir    = app.app_path,
                                 appName   = app.name,
                                 appInput  = app.input,
                                 #modelFilesURL = model.files.url, #ToDo: TO BE CHANGED to read from downloaded zip (model_file)
                                 modelFilesPath = model.files.path,
                                 #forcingArchiveURL = forcing.archive.url, # Used by getHindcastForcingData, getModelForcing
                                 forcingArchivePath = forcing.archive.path,
                                 #stateFilesURL = state.files.url, # Used by getModelForcing
                                 stateFilesPath = state.files.path,
                                 stateFilesIN = state.files)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)

    #################################################################################
    ## 4 - Hindcast input data
    ## ------------------------------------------------------------------------------
    ## forcing data
    hindcast.forcing <- getModelForcing(appSetup   = app.setup,
                                        appInput  = app.input,
                                        dataSource = forcing.data.source,
                                        hindcast   = T)

    ## Download and process hydrogfd netcdf files
    # ToDo: Dir name based on config dataset
    dirNCFiles  <- paste(TMPDIR,"netcdf-files",sep="/")
    dirObsFiles <- paste(TMPDIR,"obs-files",sep="/")
    dirGridMeta <- paste(TMPDIR,"grid-meta",sep="/")
    process_hindcast_netcdf2obs(modelConfigData,
                                modelDataPaths,
                                app.input$idate,
                                app.input$hcperiodlen,
                                dirNCFiles,
                                ncSubDir=TRUE,
                                dirGridMeta,
                                dirObsFiles)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("hindcast forcing set"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "Hindcast forcing data downloaded and prepared",fileConn = logFile$fileConn)

    #q(save="no", status = 0)

    ## ------------------------------------------------------------------------------

    xobs.data <- getXobsData(appInput = app.input, # xobsNum, xobs, xobsURL
                             appSetup = app.setup) # model run dir, res dir etc.
    if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data downloaded from catalogue"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "xobs data (if any) downloaded from catalogue",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## read downloaded Xobs input file(s) - merge into one Xobs.txt in the model run folder
    xobs.input <- readXobsData(appSetup = app.setup,
                               xobsData = xobs.data)
    if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data merged to model rundir"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "Xobs data (if any) merged into model directory",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## modify some model files based on input parameters
    hindcast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = T, modelForcing = hindcast.forcing, xobsInput = xobs.input)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("hindcast inputs modified"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "hindcast model inputs modified",fileConn = logFile$fileConn)

    #################################################################################
    ## 5 - Run hindcast
    ## ------------------------------------------------------------------------------
    ##  run hindcast
    if(hindcast.input==0){
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...starting hindcast model run", nameOfSrcFile)}
        log.res=appLogWrite(logText = "starting hindcast model run ...",fileConn = logFile$fileConn)

        # TODO add try catch on execution to exit in case of error
        # Next Hype model should return exit code 0 instead of 84

        hindcast.run = system(command = app.setup$runCommand,intern = T)

        hyssLogFile = dir(path = app.setup$runDir, pattern =".log")
        if(length(hyssLogFile)>=0){
            for(j in 1:length(hyssLogFile)){
                file.copy(from = paste(app.setup$runDir,hyssLogFile[j],sep="/"), to = paste0(app.setup$runDir, "/", "000_", app.date, "_", gsub("hyss", "hindcast_hyss",hyssLogFile[j])))
                rciop.publish(path=paste0(app.setup$runDir, "/", "000_", app.date, "_", gsub("hyss", "hindcast_hyss",hyssLogFile[j])), recursive=FALSE, metalink=TRUE)
             }
        }

        log.res=appLogWrite(logText = "... hindcast model run ready",fileConn = logFile$fileConn)
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...hindcast model run ready", nameOfSrcFile)}

        }else{
        log.res=appLogWrite(logText = "something wrong with hindcast model inputs (no run)",fileConn = logFile$fileConn)
        }

    #################################################################################
    ## 6 - Forecast input data
    ## ------------------------------------------------------------------------------
    ## Download and process ecoper netcdf files

    # ToDo: Dir name based on config dataset
    #dirNCFiles  <- paste(TMPDIR,"netcdf-files",sep="/") already set before hindcast
    #dirObsFiles <- paste(TMPDIR,"obs-files",sep="/")    already set before hindcast
    process_forecast_netcdf2obs(modelConfigData,
                                modelDataPaths,
                                app.input$idate,
                                dirNCFiles,
                                ncSubDir=TRUE,
                                dirGridMeta,
                                dirObsFiles)
    ## forcing data
    forecast.forcing <- getModelForcing(appSetup   = app.setup,
                                        appInput  = app.input,
                                        dataSource = forcing.data.source,
                                        hindcast   = F)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("...forecast forcing set"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "forecast model forcing data downloaded and prepared",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## modify some model files based on input parameters
    forecast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = F, modelForcing = forecast.forcing, xobsInput = NULL)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("...forecast inputs modified"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "forecast model input files modified",fileConn = logFile$fileConn)

    #################################################################################
    ## 7 - Run forecast
    ## ------------------------------------------------------------------------------
    ##  run model
    if(forecast.input==0){
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...starting forecast model run", nameOfSrcFile)}
        log.res=appLogWrite(logText = "starting forecast model run ...",fileConn = logFile$fileConn)

        forecast.run = system(command = app.setup$runCommand,intern = T)

        log.res=appLogWrite(logText = "... forecast model run ready",fileConn = logFile$fileConn)
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...forecast model run ready", nameOfSrcFile)}
        }else{
        log.res=appLogWrite(logText = "something wrong with forecast model inputs (no run)",fileConn = logFile$fileConn)
        }

    #################################################################################
    ## 8 - Output
    ## ------------------------------------------------------------------------------
    ## post-process output data
    #if(attr(forecast.run,"status")==1){
    #  rciop.publish(paste(app.setup$runDir,"/*",sep=""),recursive=TRUE,metalink=TRUE)
    #}else{
    #app.outdir <- prepareHypeAppsOutput(appSetup  = app.setup, appInput = app.input,
    #                                    modelInput = forecast.input, modelForcing = forecast.forcing,
    #                                    runRes = attr(forecast.run,"status"))
    app.outfiles <- prepareHypeAppsOutput(appSetup  = app.setup, appInput = app.input,
                                          modelInput = forecast.input, modelForcing = forecast.forcing,
                                          runRes = attr(forecast.run,"status"),
                                          appDate = app.date)
    if(length(app.outfiles)>1){
        app.outfiles=sort(app.outfiles,decreasing = F)
        }
    log.res=appLogWrite(logText = "HypeApp outputs prepared",fileConn = logFile$fileConn)

    # Prepare for trigger distribution
    # Written by jafet.andersson@smhi.se
    for (i in app.outfiles) {
        if (grepl("forecast_mapWarningLevel.txt", i)) {
            map_file <- i
            }
        }
    trigger_distribution_outfiles <- TriggerDistribution(dirname(map_file), app.input$idate)

    ## ------------------------------------------------------------------------------
    ## publish postprocessed results
    if(app.sys=="tep"){
        #  for(k in 1:length(app.outdir)){
        #    rciop.publish(path=paste(app.outdir[k],"/*",sep=""), recursive=FALSE, metalink=TRUE)
        #  }
        for(k in 1:length(app.outfiles)){
            rciop.publish(path=app.outfiles[k], recursive=FALSE, metalink=TRUE)
            }
        log.res=appLogWrite(logText = "HypeApp outputs published",fileConn = logFile$fileConn)

        for(k in 1:length(trigger_distribution_outfiles)){
            rciop.publish(path=trigger_distribution_outfiles[k], recursive=FALSE, metalink=TRUE)
            }
        log.res=appLogWrite(logText = "trigger distribution outputs published",fileConn = logFile$fileConn)

        }

    ## close and publish the logfile
    log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
    if(app.sys=="tep"){
        rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=TRUE)
        }

    #}
    #################################################################################
    ## 9 - End of workflow
    ## ------------------------------------------------------------------------------
    ## exit with appropriate status code
    q(save="no", status = 0)
}
