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
#    if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), nameOfSrcFile)}
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
nameOfSrcFile <- "/node_forecast/run.R"

# Config parameter for process_hindcast_netcdf2obs and process_forcast_netcdf2obs functions
# TRUE  - copy obs files directly to model run dir.
# FALSE - standard hypeapps functions (reads and) copies obs files to model run dir
cEnableCopyObsFilesToRunDir <- FALSE


## create a date tag to include in output filenames

# Handle HTEP fake input to only run the code in one run slot
stdin_f <- file("stdin")
open(stdin_f)
while(length(input <- readLines(stdin_f, n=1)) > 0) {

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
    forecastIssueDate <- rciop.getparam("idate")
    fileDate <- gsub("-", "", as.character(forecastIssueDate))
    logFile=appLogOpen(appName = app.name, tmpDir = getwd(),appDate = fileDate,prefix="000")

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
        log.res=appLogWrite(logText = "Using configuration from hypeapps-model-settings.R",fileConn = logFile$fileConn)
    }else if (app.sys=="tep") {
        ## ------------------------------------------------------------------------------
        ## Get HYPE model data dirs
        rciop.log("INFO", "Processing config for model data", nameOfSrcFile)
        modelDataPaths <- process_input_hype_model_data(applRuntimeOptions,modelConfigData) # ,paste0(TMPDIR,"/hype-model-data"))

        ## ------------------------------------------------------------------------------
        # Overwrite variables normally set in hypeapps-model-settings.R
        model.files.path     <- modelDataPaths$dirModelFiles     # Instead of model.files.url
        forcing.archive.path <- modelDataPaths$dirForcingArchive # Instead of forcing.archive.url
        state.files.path     <- modelDataPaths$dirStateFiles     # Instead of state.files.url
        #shapefiles          <- modelDataPaths$dirShapeFiles
        rciop.log("INFO path", model.files.path)
        rciop.log("INFO path", forcing.archive.path)
        rciop.log("INFO path", state.files.path)
        #ToDo: When switching between different HYPE model datasets, we may need to output a list of filenames (currently hardcoded filenames)
    }

    if (applRuntimeOptions$hydModel == cHydModelVariant1) {
        # Name of local subdir (run-dir/subdir), prefix in some filenames
        modelName <- model.name # hypeapps-model-settings.R
        modelBin  <- model.bin  # hypeapps-model-settings.R
    }
    if (applRuntimeOptions$hydModel == cHydModelVariant2) {
        modelName <- "westafrica-hype"
        modelBin  <- "hype-5.8.0.exe"

        # HYPE 5.8.0 required newer gfortran than part of gcc-4.4.7 during build.
        #Apperantely not set   Sys.setenv(LD_LIBRARY_PATH=paste("/opt/anaconda/pkgs/gcc-4.8.5-7/lib", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
        Sys.setenv(LD_LIBRARY_PATH=paste0("/opt/anaconda/pkgs/gcc-4.8.5-7/lib"))
        #print(Sys.getenv(LD_LIBRARY_PATH))
    }

    app.setup <- getHypeAppSetup(modelName = modelName,
                                 modelBin  = modelBin,
                                 tmpDir    = app.tmp_path,
                                 appDir    = app.app_path,
                                 appName   = app.name,
                                 appInput  = app.input,
                                 modelFilesPath = model.files.path,
                                 forcingArchivePath = forcing.archive.path,
                                 stateFilesPath = state.files.path,
                                 stateFilesIN = state.files,
                                 modelDataPaths = modelDataPaths)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)

    #################################################################################
    ## 4 - Hindcast input data
    ## ------------------------------------------------------------------------------
    ## forcing data

    if (applRuntimeOptions$metHC == cMetHCVariant1) {
        hindcast.forcing <- getModelForcing(appSetup   = app.setup,
                                            appInput   = app.input,
                                            dataSource = forcing.data.source,
                                            hindcast   = T)
        print("hindcast.forcing returned1:")
        print(hindcast.forcing)

        # getModelForcing returns:
        # return(list("status"=T, # TRUE
        #            "localFile"=downloadInfo$localFile,# "/var/lib/hadoop-0.20/cache/mapred/mapred/local/taskTracker/tomcat/jobcache/job_201910161533_0110/attempt_201910161533_0110_m_000000_0/work/tmp/20190926.zip"
        #            "issueDate"=issueDate.Num, # "2019-09-26 GMT"
        #            "archive"=F, # FALSE
        #            "bdate"=bdate, # "2019-01-01 01:00:00 CET"
        #            "cdate"=cdate, # "2019-05-26 GMT"
        #            "edate"=edate, # "2019-09-25 GMT"
        #            "outstateDate"=outstateDate, # "2019-09-26 GMT"
        #            "stateFile"=stateFile)) # "/var/lib/hadoop-0.20/cache/mapred/mapred/local/taskTracker/tomcat/jobcache/job_201910161533_0110/attempt_201910161533_0110_m_000000_0/work/tmp/model/forecast/niger-hype/state_save20190101.txt"

        if(app.sys=="tep"){rciop.log ("DEBUG", paste("hindcast forcing set"), nameOfSrcFile)}
        log.res=appLogWrite(logText = "Hindcast forcing data downloaded and prepared",fileConn = logFile$fileConn)

        ## ------------------------------------------------------------------------------

        xobs.data <- getXobsData(appInput = app.input, # xobsNum, xobs, xobsURL
                                 appSetup = app.setup) # model run dir, res dir etc.

        print("xobs.data1.3:")
        print(xobs.data)
        if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data downloaded from catalogue"), nameOfSrcFile)}
        log.res=appLogWrite(logText = "xobs data (if any) downloaded from catalogue",fileConn = logFile$fileConn)
    }

    if (applRuntimeOptions$metHC == cMetHCVariant2) {
        ## Download and process hydrogfd netcdf files
        # ToDo: Dir name based on config dataset
        dirNCFiles  <- paste(TMPDIR,"netcdf-files",sep="/")
        dirObsFiles <- paste(TMPDIR,"obs-files",sep="/") # Output dir of produced files
        #dirGridMeta <- paste(TMPDIR,"grid-meta",sep="/")
        hindcast.forcingandxobs <- process_hindcast_netcdf2obs(modelConfigData,
                                                               modelDataPaths,
                                                               app.input$idate,
                                                               app.input$hcperiodlen,
                                                               dirNCFiles,
                                                               ncSubDir=TRUE,
                                                               modelDataPaths$dirGridMetaData,
                                                               cEnableCopyObsFilesToRunDir,
                                                               app.setup$runDir,
                                                               dirObsFiles)

        # updateModelInput() uses only:
        # hindcast.forcing$bdate # as.Date
        # hindcast.forcing$cdate # as.Date
        # hindcast.forcing$edate # as.Date
        # xobs.input$xobsVar # list
        # xobs.input$xobsSubid # list

        # Minimal variants of original list types
        hindcast.forcing <- hindcast.forcingandxobs$hindcast.forcing
        #xobs.input       <- hindcast.forcingandxobs$xobs.input
        xobs.data        <- hindcast.forcingandxobs$xobs.data
        print("hindcast.forcing returned2:")
        print(hindcast.forcing)
        # print("xobs.input returned2:")
        # print(xobs.input)
        print("xobs.data returned2:")
        print(xobs.data)
    }

    ## ------------------------------------------------------------------------------
    ## read downloaded Xobs input file(s) - merge into one Xobs.txt in the model run folder
    xobs.input <- readXobsData(appSetup = app.setup,
                                xobsData = xobs.data)
    print("xobs.input returned1:")
    print(xobs.input)
    if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data merged to model rundir"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "Xobs data (if any) merged into model directory",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## modify some model files based on input parameters
    #if (applRuntimeOptions$metHC == cMetHCVariant1) {
    hindcast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = T, modelForcing = hindcast.forcing, xobsInput = xobs.input)
    print("hindcast.input:")
    print(hindcast.input)
    #q(save="no", status = 0)

    #}
    #if (applRuntimeOptions$metHC == cMetHCVariant2) {
    #    hindcast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
    #                                    hindcast = T, modelForcing = hindcast.forcing, xobsInput = xobs.input) # ToDo: data x2
    #}

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
                toFile <- paste0(app.setup$runDir, "/", "000_", fileDate, "_", gsub("hyss", "hindcast_hyss",hyssLogFile[j]))
                file.copy(from = paste(app.setup$runDir,hyssLogFile[j],sep="/"),to = toFile)
                rciop.publish(path=toFile, recursive=FALSE, metalink=TRUE)
             }
        }

        log.res=appLogWrite(logText = "... hindcast model run ready",fileConn = logFile$fileConn)
        if(app.sys=="tep"){rciop.log ("DEBUG", " ...hindcast model run ready", nameOfSrcFile)}

        }else{
        log.res=appLogWrite(logText = "something wrong with hindcast model inputs (no run)",fileConn = logFile$fileConn)
        }

    q(save="no", status = 0)

    #################################################################################
    ## 6 - Forecast input data
    ## ------------------------------------------------------------------------------
    ## forcing data

    if (applRuntimeOptions$metHC == cMetHCVariant1) {
        forecast.forcing <- getModelForcing(appSetup   = app.setup,
                                            appInput   = app.input,
                                            dataSource = forcing.data.source,
                                            hindcast   = F)
        print("forecast.forcing returned1:")
        print(forecast.forcing)
        #   return(list("status"=T, # TRUE
        #               "localFile"=downloadInfo$localFile, # "/var/lib/hadoop-0.20/cache/mapred/mapred/local/taskTracker/tomcat/jobcache/job_201910161533_0110/attempt_201910161533_0110_m_000000_0/work/tmp/20190926.zip"
        #               "issueDate"=issueDate.Num, # "2019-09-27 GMT" # Should be 26...
        #               "archive"=F, # FALSE
        #               "bdate"=bdate, # "2019-09-26 GMT"
        #               "cdate"=cdate, # "2019-09-26 GMT"
        #               "edate"=edate, # "2019-10-05 GMT"
        #               "outstateDate"=outstateDate, # NA
        #               "stateFile"=stateFile)) # "/var/lib/hadoop-0.20/cache/mapred/mapred/local/taskTracker/tomcat/jobcache/job_201910161533_0110/attempt_201910161533_0110_m_000000_0/work/tmp/model/forecast/niger-hype/state_save20190926.txt"

        # As previously set for forecast
        xobs.input <- NULL
    }

    if ((applRuntimeOptions$metHC == cMetHCVariant2) &&
        (applRuntimeOptions$metFC == cMetFCVariant1)) {
        ## Download and process ecoper netcdf files

        # ToDo: Dir name based on config dataset
        #dirNCFiles  <- paste(TMPDIR,"netcdf-files",sep="/") already set before hindcast
        #dirObsFiles <- paste(TMPDIR,"obs-files",sep="/")    already set before hindcast
        forecast.forcingandxobs <- process_forecast_netcdf2obs(modelConfigData,
                                                               modelDataPaths,
                                                               app.input$idate,
                                                               dirNCFiles,
                                                               ncSubDir=TRUE,
                                                               modelDataPaths$dirGridMetaData,
                                                               cEnableCopyObsFilesToRunDir,
                                                               app.setup$runDir,
                                                               dirObsFiles)
        # Minimal variants of original list types
        forecast.forcing <- forecast.forcingandxobs$forecast.forcing
        print("forecast.forcing returned2:")
        print(forecast.forcing)

        xobs.data <- forecast.forcingandxobs$xobs.data
        print("xobs.data returned2:")
        print(xobs.data)

        ## ------------------------------------------------------------------------------
        ## read downloaded Xobs input file(s) - merge into one Xobs.txt in the model run folder
        xobs.input <- readXobsData(appSetup = app.setup,
                                   xobsData = xobs.data)
        #xobs.input <- NULL
        print("xobs.input returned1:")
        print(xobs.input)
        if(app.sys=="tep"){rciop.log ("DEBUG", paste("xobs data merged to model rundir"), nameOfSrcFile)}
        log.res=appLogWrite(logText = "Xobs data (if any) merged into model directory",fileConn = logFile$fileConn)
    }

    q(save="no", status = 0)

    if(app.sys=="tep"){rciop.log ("DEBUG", paste("...forecast forcing set"), nameOfSrcFile)}
    log.res=appLogWrite(logText = "forecast model forcing data downloaded and prepared",fileConn = logFile$fileConn)

    ## ------------------------------------------------------------------------------
    ## modify some model files based on input parameters
    forecast.input <- updateModelInput(appSetup = app.setup, appInput = app.input,
                                       hindcast = F, modelForcing = forecast.forcing, xobsInput = NULL) # ToDo:xobsInput = xobs.input

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
                                          appDate = fileDate)
    if(length(app.outfiles)>1){
        app.outfiles=sort(app.outfiles,decreasing = F)
    }
    log.res=appLogWrite(logText = "HypeApp outputs prepared",fileConn = logFile$fileConn)

    trigger_distribution_outfiles <- NULL # or remove this section since no plots or maps are longer produced
    # Prepare for trigger distribution
    # Written by jafet.andersson@smhi.se
    if(length(app.outfiles>1)){
        for (i in app.outfiles) {
            if (grepl("forecast_mapWarningLevel.txt", i)) {
                map_file <- i
                print(paste0("app.input$idate: ",app.input$idate)) # Only year present previous run??? idate ok here...
                trigger_distribution_outfiles <- TriggerDistribution(dirname(map_file), app.input$idate)
            }
        }
    }

    ## ------------------------------------------------------------------------------
    ## publish postprocessed results # ToDo: HYPE model and log files shall remain published, review what part that is the postprocessing stuff
    if(app.sys=="tep"){
        #  for(k in 1:length(app.outdir)){
        #    rciop.publish(path=paste(app.outdir[k],"/*",sep=""), recursive=FALSE, metalink=TRUE)
        #  }
        for(k in 1:length(app.outfiles)){
            rciop.publish(path=app.outfiles[k], recursive=FALSE, metalink=TRUE)
        }
        log.res=appLogWrite(logText = "HypeApp outputs published",fileConn = logFile$fileConn)

        if(length(trigger_distribution_outfiles) > 0){
            for(k in 1:length(trigger_distribution_outfiles)){
                rciop.publish(path=trigger_distribution_outfiles[k], recursive=FALSE, metalink=TRUE)
            }
            log.res=appLogWrite(logText = "trigger distribution outputs published",fileConn = logFile$fileConn)
        }
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
} # while(length(input

# Oct 16 09:23 000_20191016_0437_forecast_hyss_000_191016_0439.log
# Oct 16 09:23 000_20191016_0437_hindcast_hyss_000_191016_0439.log
# Oct 16 09:23 000_20191016_0437_hypeapps-forecast.log
# Oct 16 09:23 001_20191016_0437_forecast_0004244_discharge-forecast.png
# Oct 16 09:23 001_20191016_0437_forecast_0004244_discharge-forecast.pngw
# Oct 16 09:23 001_20191016_0437_forecast_mapWarningLevel.png
# Oct 16 09:23 001_20191016_0437_forecast_mapWarningLevel.pngw
# Oct 16 09:23 002_20191016_0437_forecast_0004244.csv
# Oct 16 09:23 002_20191016_0437_hindcast_0004244.csv
# Oct 16 09:23 003_20191016_0437_forecast_0004244.txt
# Oct 16 09:23 003_20191016_0437_hindcast_0004244.txt           <<<<<< app.date
# Oct 16 09:23 004_20191015_email_message.txt                   <<<<<< app.input$idate
# Oct 16 09:23 004_20191015_sms_message.txt
# Oct 16 09:23 004_20191016_0437_forecast_mapCOUT.txt
# Oct 16 09:23 004_20191016_0437_forecast_mapWarningLevel.txt
# Oct 16 09:23 004_20191016_0437_hindcast_mapCOUT.txt
# Oct 16 09:23 005_20191016_0437_forecast_timeCOUT.txt
# Oct 16 09:23 005_20191016_0437_hindcast_timeCOUT.txt
# Oct 16 09:23 006_20191016_0437_forecast_simass.txt
# Oct 16 09:23 006_20191016_0437_forecast_subass1.txt
# Oct 16 09:23 006_20191016_0437_hindcast_simass.txt
# Oct 16 09:23 006_20191016_0437_hindcast_subass1.txt
# Oct 16 09:23 niger-hype-rp-cout.txt
# Oct 16 09:23 subbasin_shp.zip
