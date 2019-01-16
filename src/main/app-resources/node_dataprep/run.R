#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-eodata/src/main/app-resources/node_eodata/run.R

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

# Application 3: "EO data pre-processing" (hypeapps-eodata)
# Author:         David Gustafsson, SMHI
# Version:        2018-02-06

#################################################################################
## 1 - Initialization
## ------------------------------------------------------------------------------
## create a date tag to include in output filenames
app.date = format(Sys.time(), "%Y%m%d_%H%M")

## set application name
app.name = "eodata"
## ------------------------------------------------------------------------------
## flag which environment is used, if not set
if(!exists("app.sys")){
  app.sys ="tep"
}

## ------------------------------------------------------------------------------
## load rciop package and set working directory to TMPDIR when running on TEP 
if(app.sys=="tep"){
  library("rciop")
  
  rciop.log ("DEBUG", " *** hypeapps-eodata *** TEP hydrological modelling applications ***", "/node_eodata/run.R")
  rciop.log ("DEBUG", " rciop library loaded", "/node_eodata/run.R")
  
  setwd(TMPDIR)
  rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), "/node_eodata/run.R")
}

## ------------------------------------------------------------------------------
## load the R hypeapps-environment and hypeapps-utils
if(app.sys=="tep"){
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))
  rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), "/node_eodata/run.R")
}else if(app.sys=="win"){
  source("application/util/R/hypeapps-environment.R")  
  source("application/util/R/hypeapps-utils.R")
}
## open application logfile
logFile=appLogOpen(appName = app.name,tmpDir = getwd(),appDate = app.date, prefix = "000")
#################################################################################
## 2 - Application user inputs
## ------------------------------------------------------------------------------
## application input parameters
app.input <- getHypeAppInput(appName = app.name)

if(app.sys=="tep"){rciop.log ("DEBUG", paste("...hypeapps-eodata input parameters read"), "/node_eodata/run.R")}
log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)

if(app.input$wlDataInput){

#################################################################################
## 3 - Application setup
## -------------------------------------------------------------------------------
## prepare working directories and copy necessary model files
app.setup <- getHypeAppSetup(modelName = model.name,
                             modelBin  = model.bin,
                             tmpDir    = app.tmp_path,
                             appDir    = app.app_path,
                             appName   = app.name,
                             appInput  = app.input,
                             modelFilesURL = model.files.url)

if(app.sys=="tep"){rciop.log ("DEBUG", paste("...hypeapps-eodata setup prepared"), "/node_eodata/run.R")}
log.res=appLogWrite(logText = "Application setup prepared",fileConn = logFile$fileConn)

#################################################################################
## 4 - Download and process EO data
## -------------------------------------------------------------------------------
## get eo data from open catalogue
eo.data <- getEoData(appInput = app.input,
                     appSetup = app.setup)
if(app.sys=="tep"){rciop.log ("DEBUG", paste("eodata downloaded from catalogue"), "/node_eodata/run.R")}
log.res=appLogWrite(logText = "eodata downloaded from catalogue",fileConn = logFile$fileConn)

## -------------------------------------------------------------------------------
## Read eodata and prepare data on the Xobs format
xobs.data <- readEoData(appSetup = app.setup,
                        eoData = eo.data)
if(app.sys=="tep"){rciop.log ("DEBUG", paste("eodata processed to HYPE xobs format"), "/node_eodata/run.R")}
log.res=appLogWrite(logText = "eodata processed to HYPE xobs format",fileConn = logFile$fileConn)

#################################################################################
## 5 - Write Xobs file and publish
## -------------------------------------------------------------------------------
## Write eo data in Xobs format
app.output = writeEoData(appSetup = app.setup,
                         xobsData = xobs.data,
                         appDate  = app.date)

if(app.sys=="tep"){rciop.log ("DEBUG", paste("eodata xobsfile written to output"), "/node_eodata/run.R")}
log.res=appLogWrite(logText = "eodata xobsfile written to output",fileConn = logFile$fileConn)


## ------------------------------------------------------------------------------
## publish postprocessed results
if(app.sys=="tep"){
  #rciop.publish(path=paste(app.output$outDir,"/*",sep=""), recursive=FALSE, metalink=TRUE)
  for(k in 1:length(app.output$files)){
    rciop.publish(path=app.output$files[k], recursive=FALSE, metalink=TRUE)
  }
  log.res=appLogWrite(logText = "application output published",fileConn = logFile$fileConn)
}

}else{
  log.res=appLogWrite(logText = "something wrong with application input... application stops",fileConn = logFile$fileConn)
}

## close and publish the logfile
log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
if(app.sys=="tep"){
  rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=TRUE)
}

## ------------------------------------------------------------------------------
## exit with appropriate status code
q(save="no", status = 0)
